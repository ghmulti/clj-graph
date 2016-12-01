(ns graph-algorithms.core
  (:use [clojure.core]
        [clojure.set])
  (:gen-class))

(defn parse-undirected-graph [graph]
  (reduce (fn [acc [one two]] (-> acc
                                  (update-in [one] conj two)
                                  (update-in [two] conj one))) {} graph))

(defn parse-directed-graph [graph]
  (reduce (fn [acc [one two]] (-> acc
                                  (update-in [one] conj two))) {} graph))

(defn parse-input [graph parse-graph-fn]
  (-> (parse-graph-fn graph)
      (assoc-in [:visited] #{})
      (assoc-in [:tick-counter] 0)
      ))

(defn check-cycle [graph v]
  (let [visited (filter #(contains? (:visited graph) %) (get graph v))
        [entry & _ :as matches] (filter #(let [[prev postv] (get-in graph [:tick v])
                                                  [pree poste] (get-in graph [:tick %1])]
                                 (> prev pree)) visited)]
    (if entry (update-in graph [:cycle v] concat matches) graph)))

(defn explore [graph v]
  (let [tick-count (fn [graph v] (if v
                                   (-> graph
                                       (update-in [:tick-counter] inc)
                                       (update-in [:tick v] (comp (partial into []) conj) (inc (:tick-counter graph))))
                                   graph))
        find-unvisited-children (fn [graph v] (filter #(not (contains? (:visited graph) %)) (get graph v)))
        previsit tick-count
        postvisit tick-count
        mark-visited (fn [graph v] (if v (update-in graph [:visited] conj v) graph))]
    (if-not v
      graph
      (loop [stack []
             currentv v
             currentgraph (previsit graph v)]
        (if (nil? currentv)
          currentgraph
          (let [[top & bottom :as vertices] (find-unvisited-children currentgraph currentv)
                currentgraph (check-cycle currentgraph currentv)]
            (if (empty? vertices)
              (recur (into [] (butlast stack)) (last stack) (mark-visited (postvisit currentgraph currentv) (last stack)))
              (recur (conj stack currentv) top (mark-visited (previsit currentgraph top) top))))
          )))))

(defn topological-sort [{:keys [tick] :as graph}]
  (let [pairs (reduce (fn [acc [k [pre post]]] (assoc acc post k)) (sorted-map) tick)]
    (map second pairs)))

(defn is-directed-acylic-graph [input]
  (let [edges (set (concat (map first input) (map second input)))]
    (loop [graph (parse-input input parse-directed-graph)]
      (if-let [v (some (fn [e] (when-not (contains? (:visited graph) e) e)) edges)]
        (let [it-graph (explore graph v)]
          (if (:cycle it-graph)
            (do (println "Cycle has been found" it-graph)
                false)
            (recur it-graph)))
        true))))

(defn label-components [input parse-graph-fn]
  (let [edges (set (concat (map first input) (map second input)))]
    (loop [counter 1
           graph (parse-input input parse-graph-fn)]
      (if-let [v (some (fn [e] (when-not (contains? (:visited graph) e) e)) edges)]
        (let [it-graph (explore graph v)
              lbl-graph (assoc-in it-graph [:labeled counter] (clojure.set/difference (:visited it-graph) (:visited graph)))]
          (recur (inc counter) lbl-graph))
        graph))))

(defn num-components [input directed]
  (count (:labeled (label-components input (if directed parse-directed-graph parse-undirected-graph)))))

(defn connected? [input [start end] directed]
  (let [graph (parse-input input (if directed parse-directed-graph parse-undirected-graph))
        connection (explore graph start)]
    (contains? (:visited connection) end)))

(defn get-input []
  (with-open [rdr (clojure.java.io/reader "resources/neighbour-zones.txt")]
    (->> (line-seq rdr) (map #(clojure.string/split % #";")) doall)))

(defn -main [& args]
  (label-components (get-input) parse-undirected-graph))