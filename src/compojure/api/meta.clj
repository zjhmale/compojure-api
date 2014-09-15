(ns compojure.api.meta
  (:require [compojure.api.common :refer :all]
            [compojure.core :refer [routes]]
            [ring.util.response :as response]
            [ring.util.http-response :refer [internal-server-error]]
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
    (if-let [{:keys [status] :as response} (handler request)]
      (if-let [model (responses status)]
        (let [body (schema/coerce model (:body response))]
          (if (schema/error? body)
            (internal-server-error {:errors (:error body)})
            (assoc response
              ::serializable? true
              :body body)))
        response))))

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
    {:code code
     :message (or (some-> model meta :message) "")
     :responseModel (eval model)}))

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
;; Smart restructurings
;;
(defmethod restructure-param :return [k model acc]
  "Defines a return type and coerced the return value of a body against it.
   Examples:
   :return MyModel
   :return {:value String}
   :return #{{:key (s/maybe Long)}}"
  (-> acc
      (update-in [:parameters] assoc k model)
      (update-in [:responses] assoc 200 model)))

(defmethod restructure-param :responses [_ responses acc]
  "value is a map of http-response-code -> Model. Translates to both swagger
   parameters and return model coercion. Models can be decorated with meta-data.
   Examples:
   :responses {403 ErrorEnvelope}
   :responses {403 ^{:message \"Underflow\"} ErrorEnvelope}"
  (let [messages (responses->messages responses)]
    (-> acc
        (update-in [:parameters :responseMessages] (comp distinct concat) messages)
        (update-in [:responses] merge responses))))

(defmethod restructure-param :body [_ [value model] acc]
  "reads body-params into a enchanced let. First parameter is the let symbol,
   second is the Model to coerced! against.
   Examples:
   :body [user User]"
  (-> acc
      (update-in [:lets] into [value (src-coerce! model :body-params :json)])
      (update-in [:parameters :parameters] conj {:type :body
                                                 :model model})))

(defmethod restructure-param :query [_ [value model] acc]
  "reads query-params into a enchanced let. First parameter is the let symbol,
   second is the Model to coerced! against.
   Examples:
   :query [user User]"
  (-> acc
      (update-in [:lets] into [value (src-coerce! model :query-params :query)])
      (update-in [:parameters :parameters] conj {:type :query
                                                 :model model})))

(defmethod restructure-param :body-params [_ body-params acc]
  "restructures body-params with plumbing letk notation. Example:
   :body-params [id :- Long name :- String]"
  (let [schema (strict (fnk-schema body-params))]
    (-> acc
        (update-in [:parameters :parameters] conj {:type :body :model schema})
        (update-in [:letks] into [body-params (src-coerce! schema :body-params :json)]))))

(defmethod restructure-param :form-params [_ form-params acc]
  "restructures form-params with plumbing letk notation. Example:
   :form-params [id :- Long name :- String]"
  (let [schema (strict (fnk-schema form-params))]
    (-> acc
        (update-in [:parameters :parameters] conj {:type :form :model schema})
        (update-in [:letks] into [form-params (src-coerce! schema :form-params :query)]))))

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
        [lets letks responses middlewares] [[] [] nil nil]
        [lets arg-with-request] (destructure-compojure-api-request lets arg)

        {:keys [lets
                letks
                responses
                middlewares
                parameters
                body]}
        (reduce
          (fn [acc [k v]]
            (restructure-param k v (update-in acc [:parameters] dissoc k)))
          (map-of lets letks responses middlewares parameters body)
          parameters)

        body `(do ~@body)
        body (if (seq letks) `(letk ~letks ~body) body)
        body (if (seq lets) `(let ~lets ~body) body)
        body (if (seq parameters) `(meta-container ~parameters ~body) body)
        body `(~method-symbol ~path ~arg-with-request ~body)
        body (if (seq middlewares) `(middlewares [~@middlewares] ~body) body)
        body (if responses `(body-coercer-middleware ~body  ~responses) body)]
    body))
