(ns uk.co.asymptotic.clojure.convex-hull.display
  (:use clojure.contrib.test-is)
  (:import 
   (javax.swing JPanel JFrame)
   (java.awt BorderLayout)))

(defstruct screen-info
  ; Describes the screen we're writing to. This is necessary so that we can turn notional
  ; coordinates into physical screen coordinates (apart from anything else, swing has the positive
  ; y axis going in the opposite direction to what we want)"
  :width
  :height)

(defn map-screen-coordinates
  [screen-info x y]
  [x (- (:height screen-info) y)])

(defn make-display-canvas
  "Make a JPanel display of a given set of 2D points and lines between them"
  [screen-info point-set line-set]
  (proxy [JPanel] []
    (paintComponent 
     [g]
     (doseq [point point-set]
       (let [[screen-x screen-y] (map-screen-coordinates screen-info (:x point) (:y point))]
         (.drawOval g (- screen-x 2) (- screen-y 2) 4 4)))
     (doseq [line line-set]
       (let [p1 (first line)
             p2 (second line)
             [x1 y1] (map-screen-coordinates screen-info (:x p1) (:y p1))
             [x2 y2] (map-screen-coordinates screen-info (:x p2) (:y p2))]
         (.drawLine g x1 y1 x2 y2))))))


(defn show-main-window
  [point-set line-set]
  (let [main-window (new JFrame "Plot")
        info (struct screen-info 500 500)]
    (doto (.getContentPane main-window)
      (.setLayout (new BorderLayout))
      (.add (make-display-canvas info point-set line-set) (BorderLayout/CENTER)))
    (doto main-window
      (.setSize (:width info) (+ (:height info) 50))
      ; TODO: Can't figure out how to set the outer container to be exactly the right size
      ; to contain an inner panel of the specified size, so we just add a fudge factor
      (.setVisible true))))