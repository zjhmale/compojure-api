(ns compojure.api.swagger
  (:require [clojure.string :as st]
            [potemkin :refer [import-vars]]
            [ring.swagger.core :as swagger]
            [compojure.api.common :refer :all]
            ring.swagger.ui
            [compojure.api.routes :as routes]
            [compojure.core :refer :all]))

(import-vars [ring.swagger.ui swagger-ui])

(defmacro swagger-docs
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
    `(routes
       (GET ~path []
            (swagger/api-listing ~parameters @~routes/+routes-sym+))
       (GET ~(str path "/:api") {{api# :api} :route-params :as request#}
            (let [produces# (-> request# :meta :produces (or []))
                  consumes# (-> request# :meta :consumes (or []))
                  parameters# (merge ~parameters {:produces produces#
                                                  :consumes consumes#})]
              (swagger/api-declaration parameters# @~routes/+routes-sym+ api# (swagger/basepath request#)))))))

(defmacro swaggered
  "Defines a swagger-api. Takes api-name, optional
   Keyword value pairs or a single Map for meta-data
   and a normal route body. Macropeels the body and
   extracts route, model and endpoint meta-datas."
  [name & body]
  (let [[details body] (routes/route-info body)
        name (st/replace (str (eval name)) #" " "")]
    `(do
       (swap! ~routes/+routes-sym+ assoc-map-ordered ~name '~details)
       (routes ~@body))))
