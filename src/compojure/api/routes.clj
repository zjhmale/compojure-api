(ns compojure.api.routes
  (:require [compojure.core :refer :all]
            [clojure.string :as st]
            [schema.core :as s]
            [ring.swagger.common :refer :all]
            [ring.swagger.core :as swagger]
            [plumbing.core :refer [fn->]]
            [clojure.walk16 :as walk]
            [ring.swagger.schema :as schema]
            [clojure.set :refer [union]]
            [compojure.api.common :refer :all]
            [ring.swagger.impl :as swagger-impl]))

;;
;; Route store
;;

(def +routes-sym+ '+routes+)

(defmacro with-routes [& body]
  `(do (def ~+routes-sym+ (atom {}))
       (routes ~@body)))

(defmacro get-routes []
  `(if-let [data# ~+routes-sym+]
     @data#
     (throw (IllegalStateException. "no +routes+ bound in this namespace."))))


;;
;; Schema helpers
;;

(defn direct-or-contained [f x]
  (if (swagger-impl/valid-container? x) (f (first x)) (f x)))

;;
;; Route peeling
;;

(def compojure-route?     #{#'GET #'POST #'PUT #'DELETE #'HEAD #'OPTIONS #'PATCH #'ANY})
(def compojure-context?   #{#'context})
(def compojure-letroutes? #{#'let-routes})
(def compojure-macro?     (union compojure-route? compojure-context? compojure-letroutes?))

(defn inline? [x] (and (symbol? x) (-> x eval-re-resolve value-of meta ::inline)))
(defn route-tree? [x] (and (symbol? x) (-> x eval-re-resolve value-of meta ::routes)))

(defrecord Route [p b])
(defrecord Routes [p c])
(defrecord RouteTree [p c])

(defn macroexpand-to-compojure [form]
  (walk/prewalk
    (fn [x]
      (cond
        (route-tree? x) (-> x value-of meta :source) ;; FIXME
        (inline? x) (-> x value-of meta :source) ;; resolve the syms!
        (seq? x)    (let [sym (first x)]
                      (if (and
                            (symbol? sym)
                            (or
                              (compojure-macro? (eval-re-resolve sym))
                              (meta-container? (eval-re-resolve sym))))
                        (filter (comp not nil?) x)
                        (let [result (macroexpand-1 x)]
                          ;; stop if macro expands to itself
                          (if (= result x) result (list result)))))
        :else x))
    form))

(defn is-a?
  "like instanceof? but compares .toString of a classes"
  [c x] (= (str c) (str (class x))))

(defn filter-routes [c]
  (filter #(or (is-a? Route %)
               (is-a? Routes %)) (flatten c)))

(defn collect-compojure-routes [form]
  (walk/postwalk
    (fn [x]
      (or
        (and
          (seq? x)
          (let [[m p] x
                rm (and (symbol? m) (eval-re-resolve m))]
            (cond
              (compojure-route? rm)     (->Route p x)
              (compojure-context? rm)   (->Routes p  (filter-routes x))
              (compojure-letroutes? rm) (->Routes "" (filter-routes x))
              :else                     x)))
        x))
    form))

(defn remove-param-regexes [p] (if (vector? p) (first p) p))

(defn strip-trailing-spaces [s] (st/replace-first s #"(.)\/+$" "$1"))

(defn create-api-route [[ks v]]
  [{:method (keyword (.getName (first (keep second ks))))
    :uri (->> ks (map first) (map remove-param-regexes) st/join strip-trailing-spaces)} v])

(defn extract-method [body]
  (-> body first str .toLowerCase keyword))

(defn create-paths [{:keys [p b c] :as r}]
  (cond
    (is-a? Route r)  (let [route-meta (meta r)
                                    method-meta (meta (first b))
                                    parameter-meta (first (extract-parameters (drop 3 b)))
                                    metadata (merge route-meta method-meta parameter-meta)
                                    new-body [(with-meta (first b) metadata) (rest b)]]
                                [[p (extract-method b)] new-body])
    (is-a? Routes r) [[p nil] (reduce (partial apply assoc-map-ordered) {} (map create-paths c))]))

(defn route-metadata [body]
  (remove-empty-keys
    (let [{:keys [return parameters] :as meta} (unwrap-meta-container (last (second body)))]
      (merge meta {:parameters (and parameters (doall (mapv eval parameters)))
                   :return (eval return)}))))

(defn ensure-path-parameters [uri route-with-meta]
  (if (seq (swagger/path-params uri))
    (let [all-parameters (get-in route-with-meta [:metadata :parameters])
          path-parameter? (fn-> :type (= :path))
          existing-path-parameters (some->> all-parameters
                                            (filter path-parameter?)
                                            first
                                            :model
                                            value-of
                                            swagger-impl/strict-schema)
          string-path-parameters (swagger/string-path-parameters uri)
          all-path-parameters (update-in string-path-parameters [:model]
                                         merge (or existing-path-parameters {}))
          new-parameters (conj (remove path-parameter? all-parameters)
                               all-path-parameters)]
      (assoc-in route-with-meta [:metadata :parameters] new-parameters))
    route-with-meta))


(defn ensure-parameter-schema-names [route-with-meta]
  (if-let [all-parameters (get-in route-with-meta [:metadata :parameters])]
    (->> all-parameters
         (mapv (fn [{:keys [model type] :as parameter}]
                (if-not (direct-or-contained schema/named-schema? model)
                  (update-in parameter [:model]
                             swagger-impl/update-schema
                             (fn-> (s/schema-with-name
                                     (gensym (->CamelCase (name type))))))
                  parameter)))
         (assoc-in route-with-meta [:metadata :parameters]))
    route-with-meta))

(defn ensure-return-schema-names [route-with-meta]
  (if-let [return (get-in route-with-meta [:metadata :return])]
    (if-not (or (direct-or-contained schema/named-schema? return)
                (direct-or-contained (comp not map?) return))
      (update-in route-with-meta [:metadata :return]
                 swagger-impl/update-schema
                 (fn-> (s/schema-with-name
                         (gensym (->CamelCase "return")))))
      route-with-meta)
    route-with-meta))

(defn attach-meta-data-to-route [[{:keys [uri] :as route} body]]
  (let [meta (route-metadata body)
        route-with-meta (if-not (empty? meta) (assoc route :metadata meta) route)]
    (->> route-with-meta
         (ensure-path-parameters uri)
         ensure-parameter-schema-names
         ensure-return-schema-names)))

(defn peel [x]
  (or (and (seq? x) (= 1 (count x)) (first x)) x))

(defn ensure-routes-in-root [body]
  (if (seq? body)
    (->Routes "" (filter-routes body))
    body))

(defn extract-routes [body]
  (->> body
       peel
       macroexpand-to-compojure
       collect-compojure-routes
       ensure-routes-in-root
       create-paths
       (apply array-map)
       path-vals
       (map create-api-route)
       (map attach-meta-data-to-route)
       reverse
       vec))

(defn route-info [body]
  (let [[parameters body] (extract-parameters body)
        routes  (extract-routes body)
        details (assoc parameters :routes routes)]
    [details body]))

