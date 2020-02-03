(defproject amperity/clj-cron-parse "0.1.6-SNAPSHOT"
  :description "A Clojure library for using cron expressions"
  :url "https://github.com/amperity/clj-cron-parse"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :deploy-branches ["master"]
  :pedantic? :warn
  :deploy-repositories {"releases" {:url "https://repo.clojars.org"}}

  :dependencies [[org.clojure/clojure "1.10.1"]
                 [clj-time "0.15.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]]
  :profiles {:dev {:dependencies [[midje "1.9.9"]]
                   :plugins [[lein-cljfmt "0.1.10"]
                             [lein-midje "3.2.1"]]}})
