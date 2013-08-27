(ns critters
  (:require [clojure.string :as str])
  (:import [javax.swing JFrame JPanel Timer JButton]
           [java.awt Font Color Graphics BorderLayout]
           [java.awt.event ActionListener]))

(def cardinal-directions [:north :east :south :west])
(def relative-directions [:front :right :back :left])
(def possible-moves [:infect :hop :left :right])

(defmulti get-move :type)

(defn create-world [width height critters]
  {:width width,
   :height height,
   :critters (into {}
               (map (fn [critter pos] [pos (assoc critter :position pos)])
                 (map #(assoc % :orientation (rand-nth [:north :south :east :west]))
                   (shuffle critters))
                 (take
                   (count critters)
                   (shuffle (for [x (range width) y (range height)] [x y])))))})

(defn neighbors [width height pos]
  (->> [[0 1] [1 0] [0 -1] [-1 0]]
    (map #(map + % pos))
    (map (fn [[x y]] (if (and (< -1 x width) (< -1 y height)) [x y] nil)))
    (zipmap [:north :east :south :west])
    (filter #(second %))
    (into {})))

(defn can-hop? [{front :front}] (= front :empty))
(defn can-infect? [{front :front}] (some #{:other} [front]))
(defn can-rotate-left? [surroundings] true)
(defn can-rotate-right? [surroundings] true)

(defn cardinal-to-relative-direction [orientation m]
  (let [direction-count (count cardinal-directions)
        current-direction-index (.indexOf cardinal-directions orientation)
        cardinal-relative-map (zipmap
                                (map cardinal-directions
                                  (map #(mod (+ % current-direction-index) direction-count)
                                    (range direction-count)))
                                relative-directions)]
    (->> m
      (map (fn [[k v]] [(get cardinal-relative-map k) v]))
      (into {}))))

(defn find-surroundings
  [{:keys [width height critters] :as world}
   {:keys [position orientation] :as critter}]
  (cardinal-to-relative-direction
    orientation
    (->> (neighbors width height position)
      (map (fn [[k pos]] (let [neighbor (get critters pos)]
                           [k (cond
                                (= nil neighbor) :empty
                                (= (:type critter) (:type neighbor)) :same
                                :default :other)])))
      (into {})
      (merge (zipmap cardinal-directions (repeat :wall))))))

(defn valid-moves [{:keys [width height critters] :as world} critter]
  (let [surroundings (find-surroundings world critter)
        pred-action [[can-hop? :hop]
                     [can-infect? :infect]
                     [can-rotate-left? :left]
                     [can-rotate-right? :right]]]
    (->> pred-action
      (map (fn [[p a]] (if (p surroundings) a nil)))
      (filter identity))))

(defn next-world
  ([{:keys [width height critters] :as world}]
    (next-world world (shuffle (vals critters))))

  ([{:keys [width height critters] :as world} [critter & work-list]]
    (if critter
      (let [{ :keys [position orientation]} critter
            surroundings (find-surroundings world critter)
            move (get-move critter surroundings)]
        (cond
          ;; case 1: rotating
          (some #{move} [:left :right])
          (recur
            (assoc-in world [:critters position :orientation]
              (get cardinal-directions
                (mod
                  (({:left -, :right +} move) (.indexOf cardinal-directions orientation) 1)
                  4)))
            work-list)

          ;; case 2: hopping
          (and (= move :hop) (can-hop? surroundings))
          (let [new-position (get (neighbors width height position) orientation)]
            (recur
              (assoc world :critters
                (assoc (dissoc critters position)
                  new-position
                  (assoc critter :position new-position)))
              work-list))

          ;; case 3: infecting
          (and (= move :infect) (can-infect? surroundings))
          (let [new-position (get (neighbors width height position) orientation)
                infected (get critters new-position)]
            (recur
              (assoc-in world [:critters new-position]
                (merge
                  critter
                  {:position new-position, :orientation (rand-nth cardinal-directions)}))
              ; TODO: This last bit is O(n), fix it.
              (remove #(= infected %) work-list)))

          ;; case 4: invalid move - do nothing
          :default
          (recur world work-list)))
      world)))

(defn world-to-character-view [{:keys [width height critters]}]
  (partition width (map #(get-in critters [% :character] \.)
                     (for [y (range height) x (range width)] [x y]))))

(defn world-to-string [world]
  (str/join "\n" (map #(str/join %) (world-to-character-view world))))

(defn print-world [world]
  (println (world-to-string world)))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn critter-constructor [type character]
  (fn [] {:id (uuid), :type type, :character character}))
                                                                             1
(def create-flytrap (critter-constructor :flytrap \T))
(def create-food (critter-constructor :food \F))
(def create-landmine (critter-constructor :landmine \L))
(def create-wanderer (critter-constructor :wanderer \W))
(def create-rover (critter-constructor :rover \R))
(def create-avoider (critter-constructor :avoider \A))

(defmethod get-move :flytrap
  [critter {:keys [front back right left]}]
  (if (= front :other) :infect :left))

(defmethod get-move :food
  [critter {:keys [front back right left]}]
  :left)

(defmethod get-move :landmine
  [critter {:keys [front back right left]}]
  (if (= front :wall) :right :infect))

(defmethod get-move :wanderer
  [critter {:keys [front back right left]}]
  (cond
    (= front :other) :infect
    (= front :same) (rand-nth [:left :right])
    (<= (rand) 0.75) :hop
    :default (rand-nth [:left :right])))

(defmethod get-move :rover
  [critter {:keys [front back right left]}]
  (cond
    (= front :other) :infect
    (some #{front} [:same :wall]) (rand-nth [:left :right])
    :default :hop))

(defmethod get-move :avoider
  [critter {:keys [front back right left]}]
  (cond
    (= front :other) :infect
    (and (= front :empty) (some #{:other} [back right left])) :hop
    (= left :other) :left
    (= right :other) :right
    (= back :other) (rand-nth [:left :right])
    (and (= back :same) (= front :empty)) :hop
    (and (< (rand) 0.75) (= front :same)) :infect
    (< (rand) (* 0.10 (count (filter #(= % :same) [left right front back])))) :infect
    :default (rand-nth [:left :right :hop])))

(def critters (flatten (map #(for [n (range (first %))] ((second %)))
                (partition 2
                  [50 create-flytrap
                   50 create-food
                   50 create-landmine
                   50 create-wanderer
                   50 create-rover
                   50 create-avoider
                   ]))))

(def world (create-world 75 25 critters))

(print-world world)

(print-world (next-world world))

(defn world-states [world] (cons world (lazy-seq (world-states (next-world world)))))

(defn draw-character-matrix [^Graphics g x-gap y-gap y-offset [row & more]]
  (if row
    (let [^Font font (.getFont g)
          font-height (.getSize font)
          font-width (.stringWidth (.getFontMetrics g font) "A")
          row (vec row)]
      (dorun
        (->> (interleave (range (count row)) row)
          (partition 2)
          (map (fn [[i character]]
                 [(* i (+ x-gap font-width)) character]))
          (map (fn [[x-offset character]]
                 (.drawString g (str character) x-offset y-offset)))))
      (recur g x-gap y-gap (+ y-gap y-offset font-height) more))
    nil))

(defn swing-view [{:keys [width height] :as world}]
  (let [world (ref world)
        font-height 15
        ^Font font (Font. Font/MONOSPACED Font/BOLD font-height)

        panel (proxy [JPanel] []
                (paintComponent [^Graphics g]
                  (proxy-super paintComponent g)
                  (let [font-width (.stringWidth (.getFontMetrics g font) "A")
                        character-view (world-to-character-view @world)
                        x-gap (max 1 (int (/ (- (.getWidth this) (* font-width width)) (- width 1))))
                        y-gap (max 1 (int (/ (- (.getHeight this) (* font-height height)) (- height 1))))]
                    (.setFont g font)
                    (.setColor g Color/BLACK)
                    (draw-character-matrix g x-gap y-gap font-height character-view))))

        update-handler (proxy [ActionListener] []
                         (actionPerformed [e]
                           (dosync
                             (ref-set world (next-world @world))
                             (.repaint panel))))

        update-timer (Timer. 100 update-handler)]

    (doto panel
      (.setBackground Color/CYAN))

    (doto update-timer
      (.start))

    (doto (JFrame.)
      (.setSize 1100 800)
      (.add panel BorderLayout/CENTER)
      (.add
        (doto (JPanel.)
          (.add
            (doto (JButton. "Start")
              (.addActionListener
                (proxy [ActionListener] []
                  (actionPerformed [e]
                    (.start update-timer))))))
          (.add
            (doto (JButton. "Pause")
              (.addActionListener
                (proxy [ActionListener] []
                  (actionPerformed [e]
                    (.stop update-timer)))))))
        BorderLayout/PAGE_END)
;      (.setContentPane panel)
      (.setVisible true)
      (.setDefaultCloseOperation javax.swing.WindowConstants/DISPOSE_ON_CLOSE))))
