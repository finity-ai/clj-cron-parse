(defproject org.clojars.quartet/clj-cron-parse "0.1.6-SNAPSHOT"
  :description "A Clojure library for using cron expressions"
  :url "https://github.com/shmish111/clj-cron-parse"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clj-time "0.12.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]]
  :profiles {:dev {:dependencies [[midje "1.6.3"]]
                   :plugins [[lein-cljfmt "0.1.10"]
                             [lein-midje "3.1.3"]]}})
