(defproject llll-work "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  ;; add per WARNING: JVM argument TieredStopAtLevel=1 is active...
  :jvm-opts ^:replace []
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [llll "0.1.0"]])
