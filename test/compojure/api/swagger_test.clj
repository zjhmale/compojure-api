(ns compojure.api.swagger-test
  (:require [compojure.api.core :refer :all]
            [compojure.api.swagger :refer :all]
            [compojure.core :refer :all]
            [midje.sweet :refer :all])
  (:import [java.io StringWriter]))

(fact "extracting compojure paths"

  (fact "all compojure.core macros are interpreted"
    (extract-routes
      '(context "/a" []
         (routes
           (context "/b" []
             (let-routes []
               (GET     "/c" [] identity)
               (POST    "/d" [] identity)
               (PUT     "/e" [] identity)
               (DELETE  "/f" [] identity)
               (OPTIONS "/g" [] identity)
               (PATCH   "/h" [] identity)))
           (context "/:i/:j" []
             (GET "/k/:l/m/:n" [] identity)))))

    => {"/a/b/c" {:get nil}
        "/a/b/d" {:post nil}
        "/a/b/e" {:put nil}
        "/a/b/f" {:delete nil}
        "/a/b/g" {:options nil}
        "/a/b/h" {:patch nil}
        "/a/:i/:j/k/:l/m/:n" {:get {:parameters {:path {:i String
                                                        :j String
                                                        :l String
                                                        :n String}}}}})

  (fact "runtime code in route is ignored"
    (extract-routes
      '(context "/api" []
         (if false
           (GET "/true" [] identity)
           (PUT "/false" [] identity)))) => {"/api/true" {:get nil}
                                             "/api/false" {:put nil}})

  (fact "route-macros are expanded"
    (defmacro optional-routes [p & body] (when p `(routes ~@body)))
    (extract-routes
      '(context "/api" []
         (optional-routes true
           (GET "/true" [] identity))
         (optional-routes false
           (PUT "/false" [] identity)))) => {"/api/true" {:get nil}})

  (fact "endpoint-macros are expanded"
    (defmacro GET+ [p & body] `(GET* ~(str "/xxx" p) ~@body))
    (extract-routes
      '(context "/api" []
         (GET+ "/true" [] identity))) => {"/api/xxx/true" {:get nil}})

  (fact "Vanilla Compojure defroutes are NOT followed"
    (defroutes even-more-routes (GET "/even" [] identity))
    (defroutes more-routes (context "/more" [] even-more-routes))
    (extract-routes
      '(context "/api" []
         (GET "/true" [] identity)
         more-routes)) => {"/api/true" {:get nil}})

  (fact "Compojure Api defroutes are followed"
    (defroutes* even-more-routes* (GET "/even" [] identity))
    (defroutes* more-routes* (context "/more" [] even-more-routes*))
    (extract-routes
      '(context "/api" []
         (GET "/true" [] identity)
         more-routes*)) => {"/api/true" {:get nil}
                            "/api/more/even" {:get nil}})

  (fact "Parameter regular expressions are discarded"
    (extract-routes '(context "/api" []
                       (GET ["/:param" :param #"[a-z]+"] [] identity)))

    => {"/api/:param" {:get {:parameters {:path {:param String}}}}}))


(fact "->swagger2info"
  (fact "old format get's converted to new with warnings"
    (binding [*out* (StringWriter.)]
      (select-swagger2-parameters
        {:version ..version..
         :title ..title..
         :description ..description..
         :termsOfServiceUrl ..url..
         :license ..license..})

      => {:info {:version ..version..
                 :title ..title..
                 :description ..description..
                 :termsOfService ..url..
                 :license {:name ..license..}}}))

  (fact "with all datas"
    (let [info {:info {:version "1.0.0"
                       :title "Sausages"
                       :description "Sausage description"
                       :termsOfService "http://helloreverb.com/terms/"
                       :contact {:name "My API Team"
                                 :email "foo@example.com"
                                 :url "http://www.metosin.fi"}
                       :license {:name "Eclipse Public License"
                                 :url "http://www.eclipse.org/legal/epl-v10.html"}}
                :tags [{:name "kikka", :description "kukka"}]}]
      (select-swagger2-parameters
        info) => info)))

(facts "swagger-info"

  (fact "with keyword-parameters"
    (first
      (swagger-info
        '(:title ..title..
          :description ..description..
          :paths ..overridded..
          (context "/api" []
            (GET "/user/:id" [] identity)))))

    => {:title  ..title..
        :description ..description..
        :paths {"/api/user/:id" {:get {:parameters {:path {:id String}}}}}})

  (fact "with map-parameters"
    (first
      (swagger-info
        '({:title ..title..
           :description ..description..
           :paths ..overridded..}
          (context "/api" []
            (GET "/user/:id" [] identity)))))

    => {:title  ..title..
        :description ..description..
        :paths {"/api/user/:id" {:get {:parameters {:path {:id String}}}}}})

  (fact "context* meta-data"

    (first
     (swagger-info
      '((context* "/api/:id" []
          :summary "top-summary"
          :path-params [id :- String]
          :tags [:kiss]
          (GET* "/kikka" []
            identity)
          (context* "/ipa" []
            :summary "mid-summary"
            :tags [:wasp]
            (GET* "/kukka/:kukka" []
              :summary "bottom-summary"
              :path-params [kukka :- String]
              :tags [:venom])
            (GET* "/kakka" []
              identity))))))

    => {:paths {"/api/:id/kikka" {:get {:summary "top-summary"
                                        :tags #{:kiss}
                                        :parameters {:path {:id String}}}}
                "/api/:id/ipa/kukka/:kukka" {:get {:summary "bottom-summary"
                                                   :tags #{:venom}
                                                   :parameters {:path {:id String
                                                                       :kukka String}}}}
                "/api/:id/ipa/kakka" {:get {:summary "mid-summary"
                                            :tags #{:wasp}
                                            :parameters {:path {:id String}}}}}}))
