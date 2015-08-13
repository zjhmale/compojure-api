(ns compojure.api.sweet
  (:require compojure.api.core
            compojure.api.swagger
            compojure.core
            [potemkin :refer [import-vars]]))

(import-vars

  ;; compojure routing
  [compojure.core

   let-request
   routing
   routes
   let-routes
   wrap-routes]

  ;; with enchanced methods
  [compojure.api.core

   api
   defapi
   middlewares

   defroutes*
   context*

   GET*
   ANY*
   HEAD*
   PATCH*
   DELETE*
   OPTIONS*
   POST*
   PUT*]

  ;; routes
  [compojure.api.routes

   path-for]

  ;; swaggered
  [compojure.api.swagger

   swagger-ui
   swagger-docs
   swaggered]

  [ring.swagger.json-schema

   describe])
