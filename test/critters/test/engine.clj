(ns critters.test.engine
  (:require [critters.engine :refer :all],
            [clojure.test :refer :all]
            [clojure.set :as set]))

(def world {:width 5
            :height 5
            :critters {[0 0] {:position [0 0]
                              :id "1"
                              :orientation :north
                              :type :a}
                       [0 1] {:position [0 1]
                              :id "2"
                              :orientation :east
                              :type :b}
                       [1 0] {:position [1 0]
                              :id "3"
                              :orientation :west
                              :type :a}}})

(defn submap? [map1 map2]
  (= map1 (select-keys map2 (keys map1))))

(deftest utilities
  (testing "card<->rel"
    (is (= :west (get-in card<->rel [:north :left])))
    (is (= :north (get-in card<->rel [:south :back])))
    (is (= :left (get-in card<->rel [:east :north]))))

  (testing "neighbors"
    (is (= {:west [1 2], :north [2 3], :east [3 2], :south [2 1]}
          (neighbors 5 5 [2 2])))
    (is (= {:east [1 0], :north [0 1]}
          (neighbors 5 5 [0 0]))))

  (testing "find-surroundings"
    (is (submap? {:front :other, :right :same, :back :wall, :left :wall}
          (find-surroundings world {:orientation :north, :type :a, :position [0 0]})))

    (is (submap? {:front :same, :back :empty, :left :other, :right :empty}
          (find-surroundings world {:orientation :west, :type :b, :position [1 1]}))))

  (testing "relation"
    (is (= :empty (relation
                    {:type :something}
                    nil)))
    (is (= :other (relation
                   {:type :something}
                   {:type :something-else})))
    (is (= :same (relation
                   {:type :something}
                   {:type :something})))))

(deftest world-creation
  (testing "create-world"
    (let [world (create-world 5 5 [{:id (uuid)} {:id (uuid)} {:id (uuid)}])]
      (is (= 3 (count (:critters world))))
      (doall
        (map
          (fn [[pos {:keys [position]}]] (is (= pos position)))
          (:critters world))))))

(defn- create-test-world [critters]
  {:width 5
   :height 5
   :critters (into {} (map (juxt :position identity) critters))})

(deftest world-transitions
  (letfn [(dummy-critter [pos orientation char action]
            {:type char
             :character char
             :move (fn [_] action)
             :position pos
             :orientation orientation})]

      (testing "infect"
        (let [world (create-test-world
                      [(dummy-critter [1 1] :north \D :nothing)
                       (dummy-critter [1 0] :north \I :infect)])]
          (is (= \I
                (get-in (next-world world) [:critters [1 1] :character])))))

      (testing "hop"
        (let [world (create-test-world
                      [(dummy-critter [1 1] :north \H :hop)])
              new-world (next-world world)]
          (is (= \H
                (get-in new-world [:critters [1 2] :character])))
          (is (= nil
                (get-in new-world [:critters [1 1]]))))
        (let [world (create-test-world
                     [(dummy-critter [1 1] :south \H :hop)])
              new-world (next-world world)]
          (is (= \H
                (get-in new-world [:critters [1 0] :character])))
          (is (= nil
                (get-in new-world [:critters [1 1]])))))

      (testing "hop-blocked-wall"
        (let [world (create-test-world [(dummy-critter [1 4] :north \H :hop)])
              new-world (next-world world)]
          (is (= \H
                (get-in new-world [:critters [1 4] :character])))
          (is (= 1 (count (:critters new-world))))))

      (testing "hop-blocked-critter"
         (let [world (create-test-world [(dummy-critter [1 4] :south \H :hop)
                                         (dummy-critter [1 3] :north \D :nothing)])
               new-world (next-world world)]
           (is (= \H
                 (get-in new-world [:critters [1 4] :character])))
           (is (= \D
                 (get-in new-world [:critters [1 3] :character])))))

      (testing "rotate"
        (let [world (create-test-world [(dummy-critter [1 4] :north \R :left)])
              new-world (next-world world)]
          (is (= :west
                (get-in new-world [:critters [1 4] :orientation])))))))
