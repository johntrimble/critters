(ns critters.critters
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defmacro defcritter [name character move-func]
  `(def ~name
     {:character ~character
      :type ~(keyword name)
      :move ~move-func}))

(defcritter flytrap \T
  (fn [{:keys [front back left right] :as surroundings}]
    (if (= front :other) :infect :left)))

(defcritter food \F
  (fn [{:keys [front back left right] :as surroundings}] :left))

(defcritter landmine \L
   (fn [{:keys [front back left right] :as surroundings}]
     (if (= front :wall) :right :infect)))

(defcritter wanderer \W
  (fn [{:keys [front back right left]}]
    (cond
      (= front :other) :infect
      (= front :same) (rand-nth [:left :right])
      (<= (rand) 0.75) :hop
      :default (rand-nth [:left :right]))))

(defcritter rover \R
  (fn [{:keys [front back right left]}]
    (cond
      (= front :other) :infect
      (some #{front} [:same :wall]) (rand-nth [:left :right])
      :default :hop)))

(defcritter xenophobe \X
  (fn [{:keys [front back right left]}]
    (cond
      (= front :other) :infect
      (and (= front :empty) (some #{:other} [back right left])) :hop
      (= left :other) :left
      (= right :other) :right
      (= back :other) (rand-nth [:left :right])
      (and (= back :same) (= front :empty)) :hop
      (and (< (rand) 0.75) (= front :same)) :infect
      (< (rand) (* 0.10 (count (filter #(= % :same) [left right front back])))) :infect
      :default (rand-nth [:left :right :hop]))))
