(defproject genetic_algorithm "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [hiccup "1.0.5"]
                 [compojure "1.1.1"]
                 [hiccup-bridge "1.0.1"]]
  :plugins [[lein-ring "0.9.1"]
            [lein2-eclipse "2.0.0"]]
  :ring {:handler representation.routes/app})



