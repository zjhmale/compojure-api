(ns compojure.api.swagger
  (:require [clojure.string :as s]
            [clojure.walk :as walk]
            [clojure.set :refer [union]]
            [ring.util.response :refer :all]
            [ring.swagger.core :as swagger]
            [ring.swagger.common :refer :all]
            [compojure.api.common :refer :all]
            [compojure.route :as route]
            [compojure.core :refer :all]))

;;
;; Evil Global State
;;

(defonce swagger (atom {}))

;;
;; Route peeling
;;

(def compojure-route?     #{#'GET #'POST #'PUT #'DELETE #'HEAD #'OPTIONS #'PATCH #'ANY})
(def compojure-context?   #{#'context})
(def compojure-letroutes? #{#'let-routes})
(def compojure-macro?     (union compojure-route? compojure-context? compojure-letroutes?))
(def with-meta?           #{#'with-meta})

(defn inline? [x] (and (symbol? x) (-> x eval-re-resolve value-of meta :inline)))

(defn macroexpand-to-compojure [form]
  (walk/prewalk
    (fn [x]
      (cond
        (inline? x) (-> x value-of meta :source)
        (seq? x)    (let [sym (first x)]
                      (if (and
                            (symbol? sym)
                            (compojure-macro? (eval-re-resolve sym)))
                        (filter (comp not nil?) x)
                        (macroexpand-1 x)))
        :else       x))
    form))

(defrecord CompojureRoute [p b])
(defrecord CompojureRoutes [p c])

(defn filter-routes [c]
  (filter #(#{CompojureRoute CompojureRoutes} (class %)) (flatten c)))

(defn collect-compojure-routes [form]
  (walk/postwalk
    (fn [x]
      (or
        (and
          (seq? x)
          (let [[m p] x
                rm (and (symbol? m) (eval-re-resolve m))]
            (cond
              (with-meta? rm)           (eval x)
              (compojure-route? rm)     (->CompojureRoute p x)
              (compojure-context? rm)   (->CompojureRoutes p  (filter-routes x))
              (compojure-letroutes? rm) (->CompojureRoutes "" (filter-routes x))
              :else                     x)))
        x))
    form))

(defn create-uri [s]
  (-> s
    (s/replace #":(.[^:|/]*)" " :$1 ")
    (s/split #" ")
    (->> (map #(if (.startsWith % ":") (keyword (.substring % 1)) %)))))

(defn create-api-route [[ks v]]
  [{:method (first (keep second ks))
    :uri (->> ks (map first) s/join create-uri)} v])

(defn extract-method [body]
  (-> body first str .toLowerCase keyword))

(defn create-paths [{:keys [p b c] :as r}]
  (apply array-map
    (condp = (class r)
      CompojureRoute  (let [route-meta (meta r)
                            method-meta (meta (first b))
                            parameter-meta (first (extract-parameters (drop 3 b)))
                            metadata (merge route-meta method-meta parameter-meta)
                            new-body [(with-meta (first b) metadata) (rest b)]]
                        [[p (extract-method b)] new-body])
      CompojureRoutes [[p nil] (->> c (map create-paths) ->map)])))

(defn transform-parameters [parameters]
  (let [parameters (map swagger/resolve-model-vars parameters)]
    (if-not (empty? parameters) parameters)))

(defn route-metadata [body]
  (remove-empty-keys
    (let [{:keys [body return parameters] :as meta} (or (meta (first body)) {})]
      (merge meta {:parameters (transform-parameters parameters)
                   :return (some-> return swagger/resolve-model-vars)}))))

(defn attach-meta-data-to-route [[route body]]
  (let [meta   (route-metadata body)]
    (if-not (empty? meta)
      (assoc route :metadata meta)
      route)))

(defn peel [x]
  (or (and (seq? x) (= 1 (count x)) (first x)) x))

(defn extract-routes [body]
  (->> body
    peel
    macroexpand-to-compojure
    collect-compojure-routes
    create-paths
    path-vals
    (map create-api-route)
    (map attach-meta-data-to-route)
    reverse))

(defn path-to-index [path] (s/replace (str path "/index.html") #"//" "/"))

(defn swagger-info [body]
  (let [[parameters body] (extract-parameters body)
        routes  (extract-routes body)
        details (assoc parameters :routes routes)]
    [details body]))

;;
;; Public api
;;

(defn swagger-ui
  "Bind the swagger-ui to the given path. defaults to \"/\""
  ([] (swagger-ui "/"))
  ([path]
    (routes
      (GET path [] (redirect (path-to-index path)))
      (route/resources path {:root "swagger-ui"}))))

(defn swagger-docs
  "Route to serve the swagger api-docs. If the first
   parameter is a String, it is used as a url for the
   api-docs, othereise \"/api/api-docs\" will be used.
   Next Keyword value pairs for meta-data. Valid keys:

   :title :description :termsOfServiceUrl
   :contact :license :licenseUrl"
  [& body]
  (let [[path key-values] (if (string? (first body))
                            [(first body) (rest body)]
                            ["/api/api-docs" body])
        parameters (apply hash-map key-values)]
    (routes
      (GET path []
        (swagger/api-listing parameters @swagger))
      (GET (str path "/:api") {{api :api} :route-params :as request}
        (swagger/api-declaration parameters @swagger api (swagger/extract-basepath request))))))

(defmacro swaggered
   "Defines a swagger-api. Takes api-name, optional
   Keyword value pairs or a single Map for meta-data
   and a normal route body. Macropeels the body and
   extracts route, model and endpoint meta-datas."
  [name & body]
  (let [[details body] (swagger-info body)
        name (str (eval name))
        models (swagger/extract-models details)]
    (swap! swagger assoc name details)
    `(with-meta (routes ~@body) {:swagger '~details})))
