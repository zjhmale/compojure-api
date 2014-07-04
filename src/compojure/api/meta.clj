(ns compojure.api.meta
  (:require [compojure.api.common :refer :all]
            [compojure.core :refer [routes]]
            [ring.util.response :as response]
            [ring.util.http-response :as http-response]
            [plumbing.core :refer :all]
            [plumbing.fnk.impl :as fnk-impl]
            [ring.swagger.core :as swagger]
            [ring.swagger.schema :as schema]
            [ring.swagger.common :refer :all]
            [schema.core :as s]
            [clojure.walk16 :refer [keywordize-keys]]
            [clojure.tools.macro :refer [name-with-attributes]]))

(def +compojure-api-request+
  "lexically bound ring-request for handlers."
  '+compojure-api-request+)

(defn strict [schema]
  (dissoc schema 'schema.core/Keyword))

(defn fnk-schema [bind]
  (:input-schema
   (fnk-impl/letk-input-schema-and-body-form
     nil (with-meta bind {:schema s/Any}) [] nil)))

(defn body-coercer-middleware [handler responses]
  (fn [request]
    (if-let [response (handler request)]
      (let [status (:status response)]
        (if-let [model (responses status)]
          (assoc response
            ::serializable? true
            :body (schema/coerce! model (:body response)))
          (if (= status 200)
            response ; if there aren't response model for 200, just let the response go thru
            (http-response/internal-server-error (str "non-specified http status code: " status))))))))

(defn src-coerce!
  "Return source code for coerce! for a schema with coercer type,
   extracted from a key in a ring request."
  [schema key type]
  `(schema/coerce!
     ~schema
     (keywordize-keys
       (~key ~+compojure-api-request+))
     ~type))

;;
;; Response messages mangling
;;

(defn- responses->messages [responses]
  (for [[code model] responses
        :when (not= code 200)]
    (let [response-message {:code code :responseModel model}]
      (if-let [message (:message (meta model))]
        (assoc response-message :message message)
        response-message))))


(defn- collect-responses [{:keys [return responses] :as parameters}]
  (into {200 return} responses))

;;
;; Extension point
;;

(defmulti restructure-param
          "Restructures a key value pair in smart routes. By default the key
           is consumed form the :parameters map in acc. k = given key, v = value."
          (fn [k v acc] k))

;;
;; Pass-through swagger metadata
;;

(defmethod restructure-param :summary [k v acc]
  (update-in acc [:parameters] assoc k v))

(defmethod restructure-param :notes [k v acc]
  (update-in acc [:parameters] assoc k v))

(defmethod restructure-param :nickname [k v acc]
  (update-in acc [:parameters] assoc k v))


;;
;; Pass-through :return too
;;

(defmethod restructure-param :return [k v acc]
  (update-in acc [:parameters] assoc k v))

;;
;; Smart restructurings
;;

(defmethod restructure-param :responses [k responses acc]
  "Defines a return type and coerced the return value of a body against it."
  (let [messages (responses->messages responses)
        acc' (if (empty? messages) ; if messages is empty, don't add response-messages to metadata at all
               acc
               (update-in acc [:parameters ] assoc :responseMessages (responses->messages responses)))]
      (update-in acc' [:middlewares] conj `(body-coercer-middleware ~responses))))

(defmethod restructure-param :body [_ [value model model-meta] acc]
  "reads body-params into a enchanced let. First parameter is the let symbol,
   second is the Model to coerced! against, third parameter is optional meta-
   data for the model. Examples:
   :body [user User]
   :body [user User {:key \"value\"}]"
  (-> acc
      (update-in [:lets] into [value (src-coerce! model :body-params :json)])
      (update-in [:parameters :parameters] conj {:type :body
                                                 :model model
                                                 :meta model-meta})))

(defmethod restructure-param :query [_ [value model model-meta] acc]
  "reads query-params into a enchanced let. First parameter is the let symbol,
   second is the Model to coerced! against, third parameter is optional meta-
   data for the model. Examples:
   :query [user User]
   :query [user User {:key \"value\"}]"
  (-> acc
      (update-in [:lets] into [value (src-coerce! model :query-params :query)])
      (update-in [:parameters :parameters] conj {:type :query
                                                 :model model
                                                 :meta model-meta})))

(defmethod restructure-param :body-params [_ body-params acc]
  "restructures body-params with plumbing letk notation. Example:
   :body-params [id :- Long name :- String]"
  (let [schema (strict (fnk-schema body-params))]
    (-> acc
        (update-in [:parameters :parameters] conj {:type :body :model schema})
        (update-in [:letks] into [body-params (src-coerce! schema :body-params :json)]))))

(defmethod restructure-param :header-params [_ header-params acc]
  "restructures query-params with plumbing letk notation. Example:
   :header-params [id :- Long name :- String]"
  (let [schema (fnk-schema header-params)]
    (-> acc
        (update-in [:parameters :parameters] conj {:type :header :model schema})
        (update-in [:letks] into [header-params (src-coerce! schema :headers :query)]))))

(defmethod restructure-param :query-params [_ query-params acc]
  "restructures query-params with plumbing letk notation. Example:
   :query-params [id :- Long name :- String]"
  (let [schema (fnk-schema query-params)]
    (-> acc
        (update-in [:parameters :parameters] conj {:type :query :model schema})
        (update-in [:letks] into [query-params (src-coerce! schema :query-params :query)]))))

(defmethod restructure-param :path-params [_ path-params acc]
  "restructures path-params by plumbing letk notation. Example:
   :path-params [id :- Long name :- String]"
  (let [schema (fnk-schema path-params)]
    (-> acc
        (update-in [:parameters :parameters] conj {:type :path :model schema})
        (update-in [:letks] into [path-params (src-coerce! schema :route-params :query)]))))

(defmethod restructure-param :middlewares [_ middlewares acc]
  "Applies the given vector of middlewares for the route from left to right"
  (assert (and (vector? middlewares) (every? (comp ifn? eval) middlewares)))
  (update-in acc [:middlewares] into (reverse middlewares)))

;;
;; Api
;;

(defmacro middlewares
  "Wraps routes with given middlewares using thread-first macro."
  [middlewares & body]
  (let [middlewares (reverse middlewares)]
    `(-> (routes ~@body)
         ~@middlewares)))

(defn- destructure-compojure-api-request [lets arg]
  (cond
    (vector? arg) [lets (into arg [:as +compojure-api-request+])]
    (map? arg) (if-let [as (:as arg)]
                 [(conj lets +compojure-api-request+ as) arg]
                 [lets (merge arg [:as +compojure-api-request+])])
    (symbol? arg) [(conj lets +compojure-api-request+ arg) arg]
    :else (throw
            (RuntimeException.
              (str "unknown compojure destruction synxax: " arg)))))

(defn restructure [method [path arg & args]]
  (let [method-symbol (symbol (str (-> method meta :ns) "/" (-> method meta :name)))
        [parameters body] (extract-parameters args)
        [lets letks middlewares] [[] [] nil]
        [lets arg-with-request] (destructure-compojure-api-request lets arg)
        {:keys [lets
                letks
                middlewares
                parameters
                body]} (reduce
                         (fn [{:keys [lets letks middlewares parameters body]} [k v]]
                           (let [parameters (dissoc parameters k)
                                 acc (map-of lets letks middlewares parameters body)]
                             (restructure-param k v acc)))
                         (map-of lets letks middlewares parameters body)
                         (assoc parameters :responses (collect-responses parameters)))
        body `(do ~@body)
        body (if (seq letks) `(letk ~letks ~body) body)
        body (if (seq lets) `(let ~lets ~body) body)
        body (if (seq parameters) `(meta-container ~parameters ~body) body)
        body `(~method-symbol ~path ~arg-with-request ~body)]
    (if (seq middlewares) `(middlewares [~@middlewares] ~body) body)))
