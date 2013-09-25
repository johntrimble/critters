(ns critters.main
  (:require
    [critters.critters :refer :all]
    [critters.engine :refer :all]
    [critters.swing :as view]))

(def width 100)
(def height 50)
(def density 0.10)
(def critter-types [flytrap food landmine wanderer rover xenophobe])

(defn -main [& args]
  (let [count-per-type (/ (* width height density) (count critter-types))
        critters (apply concat
                   (map (partial repeat count-per-type)
                     critter-types))
        world (create-world width height critters)]
  (view/swing-view world)))
