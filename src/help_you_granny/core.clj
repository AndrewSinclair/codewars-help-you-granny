(ns help-you-granny.core)

(defn remove-nils
  [xs]
  (filter identity xs))

(defn squared
  [x]
  (* x x))

(defn get-subsequence-between
  [x y xs]
  (let [vect  (vec xs)
        start (.indexOf vect x)
        end   (.indexOf vect y)]
    (subvec vect start (inc end))))

(defn find-by
  [pred xs]
  (->> xs
    (filter pred)
    first))

(defn distance-to-town
  [town dist-table]
  (find-by #(= town (first %)) dist-table))

(defn find-town-by-friend
  [friend-towns friend]
  (->> friend-towns
    (find-by #(= friend (first %)))
    second))

(defn distance-to-grammas
  [town dist-table]
  (->> dist-table
    (distance-to-town town)
    second))

(defn angle-of-triangle
  [adjacent hypotenuse]
  (Math/acos (/ adjacent hypotenuse)))

(defn get-pairwise-distances
  [town-dists]
  (->> town-dists
    (map second)
    (partition 2 1)))

(defn angle-between
  "We need to calculate the angle of the ``fan'' between two towns,
  These angles are in right-angle triangles, so we use pythogorean theorem.
  Then use this angle for calculating the euclidean distance between those towns."
  [town1 town2 dist-table]
  (let [dist1 (distance-to-town town1 dist-table)
        dist2 (distance-to-town town2 dist-table)]
    (->> dist-table
      (get-subsequence-between dist1 dist2)
      get-pairwise-distances
      (map (fn [[dist1 dist2]] (angle-of-triangle dist1 dist2)))
      (reduce +))))

(defn distance-between-towns
  "Calculate euclidean distance between two towns using trigonometry's ``Law of Cosines''."
  [town1 town2 dist-table]
  (let [dist1  (distance-to-grammas town1 dist-table)
        dist2  (distance-to-grammas town2 dist-table)
        angle  (angle-between town1 town2 dist-table)]
    (-> (squared dist1)
      (+ (squared dist2))
      (- (* 2 dist1 dist2 (Math/cos angle)))
      Math/sqrt)))

(defn sort-dist-table-by-towns
  [dist-table towns]
  (->> dist-table
    (sort-by #(.indexOf towns (first %)))))

(defn sort-friend-towns-by-visiting-order
  [friends friend-towns]
  (->> friend-towns
    (sort-by #(.indexOf friends (first %)))))

(defn towns->distances-between-towns
  [dist-table towns]
  (let [sorted-dist-table (sort-dist-table-by-towns dist-table towns)]
    (->> towns
      (partition 2 1)
      (map (fn [[town1 town2]]
             (distance-between-towns town1 town2 sorted-dist-table))))))

(defn get-first-and-last-town-that-is-actually-visited
  [friend-towns friends]
  (let [last-town   (->> friend-towns
                      (sort-friend-towns-by-visiting-order friends)
                      last
                      second)
        first-town  (-> friend-towns
                      (find-town-by-friend (first friends)))]
    [first-town last-town]))

(defn distance-to-and-from-grammas
  [friend-towns friends dist-table]
  (let [[first-town last-town] (get-first-and-last-town-that-is-actually-visited friend-towns friends)]
    (+ (distance-to-grammas first-town dist-table)
       (distance-to-grammas last-town dist-table))))

(defn get-visited-towns-in-order
  [friend-towns friends]
  (->> friends
    (map (partial find-town-by-friend friend-towns))
    remove-nils))

(defn tour
  [friends friend-towns dist-table]
  (->> friends
    (get-visited-towns-in-order friend-towns)
    (towns->distances-between-towns dist-table)
    (reduce +)
    (+ (distance-to-and-from-grammas friend-towns friends dist-table))
    int))

