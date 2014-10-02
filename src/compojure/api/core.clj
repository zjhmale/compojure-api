(ns compojure.api.core
  (:require [clojure.tools.macro :refer [name-with-attributes]]
            [compojure.api.meta :refer [restructure]]
            [compojure.api.middleware :refer [api-middleware]]
            [compojure.api.routes :refer [with-routes]]
            [compojure.core :refer :all]
            [potemkin :refer [import-vars]]
            [ring.swagger.common :refer [extract-parameters]]))

(defmacro defapi
  "If first element of body is a map, it will be used as options for api-middleware."
  {:requires [#'defroutes #'with-routes api-middleware]}
  [name & body]
  (let [[opts body] (extract-parameters body)]
    `(defroutes ~name
       (api-middleware
         (with-routes ~@body)
         ~opts))))

(import-vars [compojure.api.meta middlewares])

(defmacro defroutes*
  "Define a Ring handler function from a sequence of routes. The name may
   optionally be followed by a doc-string and metadata map."
  [name & handlers]
  (let [source (drop 2 &form)
        [name handlers] (name-with-attributes name handlers)]
    `(def ~name (with-meta (routes ~@handlers) {:source '~source
                                                :inline true}))))

(defmacro GET*     [& args] (restructure #'GET     args))
(defmacro ANY*     [& args] (restructure #'ANY     args))
(defmacro HEAD*    [& args] (restructure #'HEAD    args))
(defmacro PATCH*   [& args] (restructure #'PATCH   args))
(defmacro DELETE*  [& args] (restructure #'DELETE  args))
(defmacro OPTIONS* [& args] (restructure #'OPTIONS args))
(defmacro POST*    [& args] (restructure #'POST    args))
(defmacro PUT*     [& args] (restructure #'PUT     args))
