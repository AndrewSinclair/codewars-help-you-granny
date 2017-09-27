(ns help-you-granny.core)

(def grammas "X0")

(defn adjacent-road
  "Given 2 roads representing the hypotenuse and opposite sides of a right angle triangle,
  find the road that represents the adjacent side."
  [{_ :a b1 :b dist1 :dist} {_ :a b2 :b dist2 :dist}]
  (->>
    (- (* dist2 dist2) (* dist1 dist1))
    Math/abs
    Math/sqrt
    (->Road b1 b2)))

(defn distances->roads
  [distances]
  (->>
    distances
    (map #(->Road grammas (first %) (second %)))))

(defn reverse-road
  [{:keys [a b dist]}]
  (->Road b a dist))

(defn concat-reverse-roads
  [roads]
  (concat roads (map reverse-road roads)))

(defn infer-other-roads
  "We have all roads from grammas to Xi. We also need all roads from Xi to Xi+1."
  [roads]
  (concat
    roads
    (->>
      roads
      (partition 2 1)
      (map
        (fn [[road1 road2]]
          (adjacent-road road1 road2))))))

(defn complete-road-map
  [dist-table]
  (->>
    dist-table
    distances->roads
    infer-other-roads
    concat-reverse-roads))

(defn distance-between-towns
  [town1 town2 road-map]
  (->>
    road-map
    (filter (fn [{:keys [a b dist]}] (and (= a town1) (= b town2))))
    first
    :dist))

(defn path-thru-grammas
  [town1 town2 road-map]
  (+
    (distance-between-towns town1 grammas road-map)
    (distance-between-towns grammas town2 road-map)))

(defn cumulative-distances
  [dist-table]
  (->>
    dist-table
    rest
    (reduce
      (fn [[[_ prev-dist] & _ :as acc] [town dist]]
        (cons [town (+ prev-dist dist)] acc))
      [(first dist-table)])))

(defn path-avoiding-grammas
  "using the math approach, we can just add up the distances on the cumulative distances on the high
  road and subtract the distance between two towns to get their distance"
  [town1 town2 cumulative-dists]
  (let [[_ dist1] (->> cumulative-dists (filter #(= (first %) town1)) first)
        [_ dist2] (->> cumulative-dists (filter #(= (first %) town2)) first)]
    (->
      (- dist1 dist2)
      Math/abs)))

(defn shortest-distance-between-towns
  "Finds the shortest path between two ``towns''.
  Due to the unique geometry stipulated in the puzzle description,
  towns are arranged in such a way that we don't have to search
  through all possible paths to find the shortest.
  The shortest path can be one  of the following scenarios:
   1: If there is a direct (1-step) path between two towns, then it is the shortest
    Otherwise the shortest path is the smallest between:
   2: The (2-step) path that goes from A to X0 and then X0 to B.
   3: Or the multi-step path that goes from A to B without going through X0."
  [town1 town2 dist-table]
  (let [road-map (complete-road-map dist-table)
        dist     (distance-between-towns town1 town2 road-map)]
    (or
      dist
      (min
        (path-thru-grammas town1 town2 road-map)
        (path-avoiding-grammas town1 town2 (cumulative-distances dist-table))))))

(defn find-town-by-friend
  [friend-towns friend]
  (->>
    friend-towns
    (filter #(= (first %) friend))
    first
    second))

(defn distance-accumulator
  [acc [town1 town2]]
  (->>
    (shortest-distance-between-towns town1 town2 dist-table)
    (+ acc)))

(defn tour
  "for each friend in the order provided,
    find the town they're in
    and sum the distance traveled so far with the distance to get there.
  Afterwards, travel one more time back home."
  [friends friend-towns dist-table]
  (as-> friends f
    (map (partial find-town-by-friend friend-towns) f)
    (conj f grammas)
    (apply vector f)
    (conj f grammas)
    (partition 2 1 f)
    (reduce distance-accumulator 0 f)
    (int f)))

