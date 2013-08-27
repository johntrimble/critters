(defproject critters "1.0.0-SNAPSHOT"
  :description "Critters"
  :jvm-opts ["-Xmx3g"]
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :repl-options { :init (dorun
                          (require
                            '[clojure.string :refer [split-lines lower-case join]]
                            '[clojure.walk :refer [macroexpand-all]])) }
)