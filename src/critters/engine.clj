(ns critters.engine
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn uuid "Generates a random UUID." [] (str (java.util.UUID/randomUUID)))

(def cardinal-directions [:north :east :south :west])
(def relative-directions [:front :right :back :left])
(def possible-moves [:infect :hop :left :right])

(defn combinations
  "Returns a sequence of the cartesian product of a-coll b-coll."
  [a-coll b-coll]
  (for [y b-coll x a-coll] [(long x) (long y)]))

(defn- shift [[item & more :as l]] (when (seq l) (concat more [item])))

(def card<->rel
  "A mapping of orientations to maps that allow converting between cardinal
  and relative directions. For example, (get-in card<->rel [:north :left])
  yields :east and (get-in card<->rel [:south :east]) yeilds :left."
  ;; cardinal-directions and relative-directions already line up when oriented
  ;; north (north is to the front, east is to the right, etc.). The trick here
  ;; is to use shift to change the alignment for each orientation.
  (->> cardinal-directions
    (iterate shift)
    (map zipmap (repeat (count relative-directions) relative-directions))
    (map (fn [m] (merge m (set/map-invert m))))
    (zipmap cardinal-directions)))

(defn rand-rotate
  "Associates a random orientation."
  [critter]
  (assoc critter :orientation (rand-nth cardinal-directions)))

(defn create-world
  "Creates a critter world with the given list of crittres randomly
  distributed in it."
  [width height critters]
  {:width width,
   :height height,
   :critters (->> (combinations (range width) (range height)) ;; list of all coordinates
               (shuffle)
               (map (partial hash-map :position))
               (map conj critters) ;; give critters random position
               (map rand-rotate) ;; give critters random orientation
               (map (juxt :position identity)) ;; turn items {...} into [position {...}]
               (into {}))})

(defn contains-point?
  [width height [x y]]
  (and (< -1 x width) (< -1 y height)))

(defn neighbors
  "Creates a map of cardinal directions to coordinates neighboring the given
  position."
  [width height pos]
  (->> [[0 1] [1 0] [0 -1] [-1 0]]
    (map (partial map + pos))
    (map #(if (contains-point? width height %) % nil)) ;; replace point with nil if not in world
    (zipmap cardinal-directions)
    (filter second) ;; removes mappings to nil
    (into {})))

(defn can-hop? [{front :front}] (= front :empty))
(defn can-infect? [{front :front}] (some #{:other} [front]))
(defn can-rotate? [direction surroundings] true)

(defn relation
  "Gives the relationship a critter has with a given neighboring critter.
  Possible values include:
    :same if the two critters are of the same type.
    :other if the two critters are of different types.
    :empty if the neighbor is nil."
  [critter neighbor]
  (cond
    (= nil neighbor) :empty
    (= (:type critter) (:type neighbor)) :same
    :default :other))

(defn find-surroundings
  "Gives a mapping of relative directions to the relationship (:empty, :same,
  :other) the given critter has with its neighbors in those directions. The
  map will also include an :id key containing the given critter's ID."
  [{:keys [width height critters] :as world}
   {:keys [position orientation id] :as critter}]
  (set/rename-keys
    (->> (neighbors width height position)
      (map (fn [[k pos]]
             [k (relation critter (get critters pos))]))
      (into {})
      (merge (zipmap cardinal-directions (repeat :wall)))
      (merge {:id id}))
    (get card<->rel orientation)))

(defn- rotate
  [direction
   world
   {:keys [position orientation] :as critter}
   surroundings]
  (if (can-rotate? direction surroundings)
    (assoc-in world [:critters position :orientation]
      (get-in card<->rel [orientation direction]))
    world))

(defn- hop
  [{:keys [width height critters] :as world}
   {:keys [position orientation] :as critter}
   surroundings]
  (let [new-position (get (neighbors width height position) orientation)]
    (if (can-hop? surroundings)
      (assoc world :critters
        (assoc (dissoc critters position)
          new-position
          (assoc critter :position new-position)))
      world)))

(defn- infect
  [{:keys [width, height, critters] :as world}
   {:keys [position orientation] :as critter}
   surroundings]
  (let [new-position (get (neighbors width height position) orientation)
        infected (get critters new-position)]
    (if (can-infect? surroundings)
      (assoc-in world [:critters new-position]
        ;; create the new critter by using the one infecting as a template.
        (merge
          critter
          {:id (uuid), :position new-position, :orientation (rand-nth cardinal-directions)}))
      world)))

(defn next-world
  "Given a critter world at time t, generates the critter world at time t+1."
  ([{:keys [width height critters] :as world}]
    (next-world world (shuffle (vals critters))))

  ([{:keys [width height critters] :as world}
    [{:keys [position orientation] :as critter} & work-list]]
    (cond
      ;; case 1 (base case): no more critters to process, we're done
      (not critter)
      world

      ;; case 2: critter no longer in world, was infected before it could move
      (not= (get critters position) critter)
      (recur world work-list)

      ;; case 3: get critter move and process it
      :default
      (let [move-map {:infect infect,
                      :hop hop,
                      :left (partial rotate :left),
                      :right (partial rotate :right)}
            nop-move (fn [_ _ _] world)
            surroundings (find-surroundings world critter)
            move ((:move critter) surroundings)]
        (recur
          ((get move-map move nop-move) world critter surroundings)
          work-list)))))

(defn world->character-view
  "Given a critter world, generates a representation of the world as a 2D
  vector with each critter's character appropriately positioned."
  [{:keys [width height critters]}]
  (partition width (map #(get-in critters [% :character] \.)
                     (combinations (range width) (range height)))))

(defn world->string
  "Given a critter world, generates a string representation of that world."
  [world]
  (str/join "\n" (map #(str/join %) (world->character-view world))))

(defn print-world
  "Given a critter world, prints its string representation to the standard
  out."
  [world]
  (println (world->string world)))

