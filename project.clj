(defproject metosin/compojure-api "0.8.0"
  :description "Compojure Api"
  :url "https://github.com/metosin/compojure-api"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "same as Clojure"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [potemkin "0.3.4"]
                 [cheshire "5.3.1"]
                 [compojure "1.1.6"]
                 [prismatic/schema "0.2.1"]
                 [metosin/ring-http-response "0.3.0"]
                 [metosin/ring-swagger "0.7.2"]]
  :profiles {:dev {:ring {:handler compojure.api.example.handler/app}
                   :plugins [[lein-clojars "0.9.1"]
                             [lein-midje "3.1.3"]
                             [lein-ring "0.8.10"]]
                   :dependencies [[ring-mock "0.1.5"]
                                  [javax.servlet/servlet-api "2.5"]
                                  [midje "1.6.2"]
                                  [metosin/ring-swagger-ui "2.0.12-1"]]}})
