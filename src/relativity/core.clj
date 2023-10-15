(ns relativity.core
  (:require [clojure.pprint :refer [cl-format]])
  (:gen-class))

(def speed-of-light 299792458.0) ; speed of light in m/s

(defn relative-velocity
  "Calculate the relative velocity w between two frames with velocities velocity-a and velocity-b."
  [velocity-a velocity-b]
  (/ (+ velocity-a velocity-b) (+ 1 (* (/ velocity-a speed-of-light) (/ velocity-b speed-of-light)))))


(defn general-relative-velocity [vA vB]
  (let [c 299792458.0 ; speed of light in m/s
        dot-product (* vA vB)
        numerator (- (* c c) (* vA vA))
        denominator (- (* c c) dot-product)
        square-root-term (Math/sqrt (- 1 (/ (* numerator numerator) (* denominator denominator))))]
    (* square-root-term c)))


(defn relativistic-correction
  "Calculate the square root term common to both time dilation and Lorentz factor."
  [velocity]
  (Math/sqrt (- 1 (/ (* velocity velocity) (* speed-of-light speed-of-light)))))

(defn time-dilation
  "Calculate time dilation using the formula Δt' = Δt * sqrt(1 - v^2 / c^2)"
  [delta-time velocity]
  (* delta-time (relativistic-correction velocity)))

(defn lorentz-factor
  "Calculate the Lorentz factor."
  [velocity]
  (if (>= velocity speed-of-light) ;; Lorentz breaks down when we get to light speed.
    ##NaN
    (/ 1 (relativistic-correction velocity))))

(let [velocity 100000000
      dilation (time-dilation 600 velocity) ; If proper time Δt is 1 second and velocity is 1000 m/s
      lorentz (lorentz-factor velocity)]
  (cl-format true "Velocity: ~R~%Dilation: ~A~%Lorentz: ~A" velocity dilation lorentz))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
