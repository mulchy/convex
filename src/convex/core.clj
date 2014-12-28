(ns core
  (require [clojure.java.io :as io])  
  require [clojure.set :as set]

  (defn- dist [a b] 
    (Math/sqrt (+ (* (- (second b) (second a))
                     (- (second b) (second a)))
                  (* (- (first b) (first a))
                     (- (first b) (first a)))))))
  
(defn- dist-from-line-to-point [a,b,c]
  (let [area (* 0.5 (-(* (- (first a) (first c)) (- (second b) (second a))) 
                      (* (- (first a) (first b)) (- (second c) (second a)))))
        base (dist a b)
        height (/ (* 2 area) base)]
    height))

(defn- dist-map [a, b, points] 
 (zipmap points (map #(dist-from-line-to-point a b %) points)))

(defn- upper-hull [a, b, points, hull] 
  (if (empty? points)
    hull
    (let [distance-map (dist-map a b points)
          upper (into {} (filter #(pos? (val %)) dist-map))] 
      (if (empty? upper)
        hull
        (let [pmax (key (apply max-key val upper))
              hull (set/union hull #{pmax})
              upper (disj (into #{} (keys upper)) pmax)
              left (filter #(< (first %) (first pmax)) upper)        
              right (filter #(> (first %) (first pmax)) upper)]
          (set/union (upper-hull a pmax left hull)
                     (upper-hull pmax b right hull)))))))

(defn- lower-hull [a, b, points, hull] 
  (if (empty? points)
    hull
    (let [distanceMap (dist-map a b points)
          lower (into {} (filter #(neg? (val %)) distanceMap))] 
      (if (empty? lower)
        hull
        (let [pmin (key (apply min-key val lower))
              hull (set/union hull #{pmin})
              lower (disj (into #{} (keys lower)) pmin)
              left (filter #(< (first %) (first pmin)) lower)        
              right (filter #(> (first %) (first pmin)) lower)]
          (set/union (lower-hull a pmin left hull)
                     (lower-hull pmin b right hull)))))))

(defn convex-hull [points]
  (let [sorted (apply sorted-set (set points))
        fi (first sorted)
        la (last sorted)
        hull #{fi, la}
        sorted (disj sorted fi la)
        distance-map (dist-map fi la sorted)
        left (keys (into {} (filter #(pos? (val %)) distance-map))) 
        right (keys (into {} (filter #(neg? (val %)) distance-map)))         
        upper (upper-hull fi, la, left, hull)
        lower (lower-hull fi, la, right, hull)
        ]
    ((set/union upper lower))))
