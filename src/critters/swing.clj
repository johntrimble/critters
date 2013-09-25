(ns critters.swing
  (:require [critters.engine :as engine])
  (:import [javax.swing JFrame JPanel Timer JButton]
           [java.awt Font Color Graphics BorderLayout Dimension]
           [java.awt.color ColorSpace]
           [java.awt.font FontRenderContext]
           [java.awt.event ActionListener]))

(def font-size 14)

(def empty-char \.)

(def golden-ratio-conjugate 0.6180339887)

(def background-color (Color. (Color/HSBtoRGB 0.5 0.5 0.5)))

;; This simple method of random color generation comes from a neat article 
;; written by Martin Ankerl: goo.gl/xKQU2s
(def rand-colors
  (map #(Color. (Color/HSBtoRGB % 0.5 0.95))
    (iterate
      #(mod (+ % golden-ratio-conjugate) 1)
      (rand))))

(defn- get-font-dimensions
  "Given a font, returns its dimensions as a map o(docf :width and :height."
  [^Font font]
  (let [frc (FontRenderContext. (.getTransform font) (boolean true) (boolean true))
        bounds (.getStringBounds font "A" frc)]
    {:width (int (.getWidth bounds)) :height (int (.getHeight bounds))}))

(defn render-fancy-ascii-grid
  "Given a fancy-ascii-grid, draws the fancy-asciis in that grid to the given
  Graphics instance using the font specified on that instance."
  [^Graphics g ^long x-gap ^long y-gap {:keys [width height fancy-asciis], :as fancy-ascii-grid}]
  ;; Note: refrain from destructuring in this method for performance reasons.
  (let [^Font font (.getFont g)
        {^int font-width :width, ^int font-height :height} (get-font-dimensions font)
        tran-coord (fn [p]
                     (let [^long x (p 0)
                           ^long y (p 1)]
                       [(+ (* x x-gap) (* x font-width))
                        (+ (* y y-gap) (* (inc y) font-height))]))]
    (dorun
      (->> fancy-asciis
        (map (fn [e]
               [(tran-coord (e 0)) (e 1)]))
        (map (fn [e]
               (let [point (e 0)
                     ^long x (point 0)
                     ^long y (point 1)
                     fancy-ascii (e 1)
                     ^Color color (:color fancy-ascii)
                     ^Character character (:character fancy-ascii)]
                 (doto g
                   (.setColor color)
                   (.drawString (str character) x y)))))))))

(defn- critter->fancy-ascii
  "Given a mapping of critter types to Color instances and a critter object,
  generates the appropriate fancy-ascii (a map of :character and :color)."
  [critter-colors critter]
  {:color (get critter-colors (:type critter) Color/BLACK)
   :character (get critter :character empty-char)})

(defn- world->fancy-ascii-grid
  "Given a critter world, generates the appropriate fancy-ascii-grid (a
  mapping of relative positions as x and y coordinates to fancy asciis."
  [critter-colors
   {:keys [width height critters] :as world}]
  (let [coords (engine/combinations (range width) (range height))
        fancy-asciis (->> coords
                       (map (partial get critters))
                       (map (partial critter->fancy-ascii critter-colors))
                       (zipmap coords))]
    {:width width, :height height, :fancy-asciis fancy-asciis}))

(defn swing-view [{:keys [width height critters] :as world}]
  "Creates a Swing view of the given world and iterates through its successive
   states."
  (let [critter-colors (zipmap
                         (distinct (map :type (vals critters)))
                         rand-colors)
        world (atom world)
        ^Font font (Font. Font/MONOSPACED Font/BOLD font-size)
        {font-width :width font-height :height} (get-font-dimensions font)

        panel (proxy [JPanel] []
                (paintComponent [^Graphics g]
                  (proxy-super paintComponent g)
                  (let [x-gap (max 1 (int (/ (- (.getWidth this) (* font-width width)) (- width 1))))
                        y-gap (max 1 (int (/ (- (.getHeight this) (* font-height height)) (- height 1))))]
                    (.setFont g font)
                    (.setColor g Color/BLACK)
                    (render-fancy-ascii-grid g x-gap y-gap (world->fancy-ascii-grid critter-colors @world)))))

        update-handler (proxy [ActionListener] []
                         (actionPerformed [e]
                           (swap! world engine/next-world)
                           (.repaint panel)))

        update-timer (Timer. 100 update-handler)]

    (doto panel
      (.setBackground background-color)
      (.setPreferredSize (Dimension. (* width (+ font-width 1)) (* height (+ font-height 1)))))

    (doto update-timer
      (.start))

    (doto (JFrame.)
      (.setTitle "Critters")
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
      (.pack)
      (.setVisible true)
      (.setDefaultCloseOperation javax.swing.WindowConstants/EXIT_ON_CLOSE))))
