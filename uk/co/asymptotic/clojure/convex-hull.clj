(ns uk.co.asymptotic.clojure.convex-hull
  (:use clojure.contrib.test-is)
  (:use [uk.co.asymptotic.clojure.convex-hull.display :only (show-main-window)]))

(defstruct point :x :y)

(defn preferred-starting-point
  "Returns which of two points we will prefer as the starting point for the algorithm"
  [p1 p2]
  (cond
   (< (:y p1) (:y p2)) p1
   (> (:y p1) (:y p2)) p2
   :default (if (< (:x p1) (:x p2)) p1 p2)))

(deftest test-preferred-starting-point
  (is 
   (=
    (struct point 32 42)
    (preferred-starting-point
     (struct point 32 42) (struct point 32 43))))
  (is
   (=
    (struct point 12 42)
    (preferred-starting-point
     (struct point 112 42) (struct point 12 42)))))

(defn find-starting-point
  "Find the point within a set that will be used as the starting point for Graham's algorithm.
We use the point with the lowest y-coordinate, and if there is a tie, the point with the
lowest x-coordinate"
  [points]
  (reduce preferred-starting-point points))

(deftest test-find-starting-point
  (is
   (struct point 12 12)
   (find-starting-point
    #{(struct point 12 12)
      (struct point 32 14)
      (struct point 6 54)})))

(defn x-axis-cotan
  "Return the cotangent of the angle formed between the line to the specified point and the
x-axis. The point is assumed to be with positive x and y coordinate, since for the purpose
of the Graham scan we can just transform the coordinates if it isn't"
  [point]
  (/ (:x point) (:y point)))

(defn angle-compare
  "Compare two points with regard to the angle they form with the x-axis. This is done
by comparing the cotangents (which form a monotonically decreasing function of angle in the
first quadrant, which is all we care about. This returns -1 if p1 < p2, 1 if p1 > p2 and 0
if they are the same"
  [p1 p2]
  (let [cot1 (x-axis-cotan p1)
        cot2 (x-axis-cotan p2)]
    (cond
     (> cot1 cot2) -1
     (< cot1 cot2) 1
     :else 0)))

(deftest test-angle-compare
  (is
   (=
    -1
    (angle-compare (struct point 10 1) (struct point 1 10))))
  (is
   (=
    1
    (angle-compare (struct point 3 5) (struct point 5 3))))
  (is
   (=
    0
    (angle-compare (struct point 1 1) (struct point 2 2)))))

(def angle-comparator
     (proxy [java.util.Comparator] []
       (compare [p1 p2]
                (angle-compare p1 p2))))

(deftest test-sort-points
  (is
   (=
    '({:x 10 :y 1}
      {:x 10 :y 2}
      {:x 3 :y 3}
      {:x 3 :y 10})
    (sort angle-comparator
          #{(struct point 3 10)
            (struct point 3 3)
            (struct point 10 2)
            (struct point 10 1)})))
  (is 
   (=
    '({:x 100 :y 50}
      {:x 100 :y 100}
      {:x 50 :y 100}
      {:x 48 :y 320})
    (sort angle-comparator
          #{{:x 100 :y 50}
            {:x 100 :y 100}
            {:x 50 :y 100}
            {:x 48 :y 320}}))))

(defn left-turn?
  "Check whether the 3 points specified make a \"left turn\" as viewed from the perspective
of someone travalling through the points in sequence"
  [p1 p2 p3]
  (< 0
     (-
      (* (- (:x p2) (:x p1))
         (- (:y p3) (:y p1)))
      (* (- (:y p2) (:y p1))
         (- (:x p3) (:x p1))))))

(deftest test-left-turn?
  (is
   (=
    true
    (left-turn?
     (struct point 0 0)
     (struct point 2 0)
     (struct point 2 1))))
  (is
   (=
    false
    (left-turn? 
     (struct point 1 1)
     (struct point 2 2)
     (struct point 3 3))))
  (is
   (left-turn?
    (struct point 100 50)
    (struct point 150 175)
    (struct point 50 100))))

(defn make-convex-hull
  "Make a convex hull for a set of points, all of which are assumed to have positive x and y
coordinates. Returns a vector of lines, each of which is a sequence of two points"
  [points]
  (let [starting-point (find-starting-point points)
        remaining-points (remove #(= % starting-point) points)
        working-set (sort angle-comparator remaining-points)]
    (loop [remaining-points working-set
           p1 starting-point
           p2 (first working-set)
           p3 (second working-set)
           hull []]
      (if (= p3 (last working-set))
        ; We have reached the end of the set, return the full hull
        (if (left-turn? p1 p2 p3)
          (concat hull [[p1 p2] [p2 p3] [p3 starting-point]])
          (concat hull [[p1 p3] [p3 starting-point]]))

        ; Does this form a left turn?
        (if (left-turn? p1 p2 p3)
          (recur (rest remaining-points) p2 p3 (nth remaining-points 2) (conj hull [p1 p2]))
          (recur (rest remaining-points) p1 p3 (nth remaining-points 2) hull))))))

(deftest test-convex-hull
  (is 
   (=
    [[{:x 50 :y 50} {:x 100 :y 50}]
     [{:x 100 :y 50} {:x 100 :y 100}]
     [{:x 100 :y 100} {:x 50 :y 100}]
     [{:x 50 :y 100} {:x 50 :y 50}]]
    (make-convex-hull
     [(struct point 50 50)
      (struct point 50 100)
      (struct point 100 50)
      (struct point 100 100)])))
  (is
   (=
    [[{:x 50 :y 50} {:x 100 :y 50}]
     [{:x 100 :y 50} {:x 150 :y 175}]
     [{:x 150 :y 175} {:x 50 :y 100}]
     [{:x 50 :y 100} {:x 50 :y 50}]]
    (make-convex-hull
     [(struct point 50 50)
      (struct point 50 100)
      (struct point 100 50)
      (struct point 100 100)
      (struct point 150 175)])))
  (is
   (=
    [[{:x 50 :y 50} {:x 100 :y 50}]
     [{:x 100 :y 50} {:x 100 :y 100}]
     [{:x 100 :y 100} {:x 48 :y 320}]
     [{:x 48 :y 320} {:x 50 :y 50}]]
    (make-convex-hull
     [(struct point 50 50)
      (struct point 50 100)
      (struct point 100 50)
      (struct point 100 100)
      (struct point 48 320)]))))