(defproject metosin/compojure-api "0.11.3"
  :description "Compojure Api"
  :url "https://github.com/metosin/compojure-api"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "same as Clojure"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [prismatic/plumbing "0.2.2"]
                 [potemkin "0.3.4"]
                 [cheshire "5.3.1"]
                 [compojure "1.1.7"]
                 [prismatic/schema "0.2.2"]
                 [metosin/ring-http-response "0.4.0"]
                 [metosin/ring-swagger "0.8.6"]]
  :profiles {:thingie {:ring {:handler examples.thingie/app
                              :reload-paths ["src" "examples/src"]}
                       :source-paths ["examples/src"]
                       :main examples.server
                       :dependencies [[metosin/ring-swagger-ui "2.0.16-2"]
                                      [http-kit "2.1.18"]]}
             :dev {:ring {:handler examples.handler/app}
                   :plugins [[lein-clojars "0.9.1"]
                             [lein-midje "3.1.3"]
                             [lein-ring "0.8.10"]]
                   :dependencies [[peridot "0.2.2"]
                                  [javax.servlet/servlet-api "2.5"]
                                  [midje "1.6.3"]]}}
  :aliases {"start-thingie"    ["with-profile" "thingie" "ring" "server"]
            "http-kit-thingie" ["with-profile" "thingie" "run"]})
