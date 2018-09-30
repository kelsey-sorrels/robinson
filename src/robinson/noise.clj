;; Functions for making random noise
(ns robinson.noise
  (:require [robinson.random :as rr]))

(defprotocol Noise
  ; signed noise [-1, 1]
  (snoise [this xin yin])
  ; unsigned noise [0, 1]
  (noise [this x y])
  ; signed 3d noise [-1, 1]
  (snoise3d [this xin yin zin])
  ; unsigned 3d noise [0, 1]
  (noise3d [this x y z]))

(defn sqrt [x]
  (Math/sqrt x))

(def ^:private grad3 [[ 1  1  0]
                      [-1  1  0]
                      [ 1 -1  0]
                      [-1 -1  0]
                      [ 1  0  1]
                      [-1  0  1]
                      [ 1  0 -1]
                      [-1  0 -1]
                      [ 0  1  1]
                      [ 0 -1  1]
                      [ 0  1 -1]
                      [ 0 -1 -1]])
(def ^:private grad4 [[ 0,  1,  1,  1]
                      [ 0,  1,  1, -1]
                      [ 0,  1, -1,  1]
                      [ 0,  1, -1, -1]
                      [ 0, -1,  1,  1]
                      [ 0, -1,  1, -1]
                      [ 0, -1, -1,  1]
                      [ 0, -1, -1, -1]
                      [ 1,  0,  1,  1]
                      [ 1,  0,  1, -1]
                      [ 1,  0, -1,  1]
                      [ 1,  0, -1, -1]
                      [-1,  0,  1,  1]
                      [-1,  0,  1, -1]
                      [-1,  0, -1,  1]
                      [-1,  0, -1, -1]
                      [ 1,  1,  0,  1]
                      [ 1,  1,  0, -1]
                      [ 1, -1,  0,  1]
                      [ 1, -1,  0, -1]
                      [-1,  1,  0,  1]
                      [-1,  1,  0, -1]
                      [-1, -1,  0,  1]
                      [-1, -1,  0, -1]
                      [ 1,  1,  1,  0]
                      [ 1,  1, -1,  0]
                      [ 1, -1,  1,  0]
                      [ 1, -1, -1,  0]
                      [-1,  1,  1,  0]
                      [-1,  1, -1,  0]
                      [-1, -1,  1,  0]
                      [-1, -1, -1,  0]])
(def ^:private f2 (* 0.5 (dec (sqrt 3.0))))
(def ^:private g2 (/ (- 3.0 (sqrt 3.0))
                     6.0))
(def ^:private f3 (/ 1.0 3.0))
(def ^:private g3 (/ 1.0 6.0))
(def ^:private f4 (/ (dec( sqrt 5.0))
                     4.0))
(def ^:private g4 (/ (- 5.0 (sqrt 5.0))
                     20.0))

(defn- fast-floor
  [x]
  (let [xi (int x)]
    (if (< x xi)
      (dec xi)
      xi)))

(defn- dot
  ([grad x y]
  (let [[gx gy] grad]
    (+ (* gx x) (* gy y))))
  ([grad x y z]
  (let [[gx gy gz] grad]
    (+ (* gx x) (* gy y) (* gz z))))
  ([grad x y z w]
  (let [[gx gy gz gw] grad]
    (+ (* gx x) (* gy y) (* gz z) (* gw w)))))


(defn create-noise
  ([]
  (create-noise rr/*rnd*))
  ([rnd]
  (let [p           (rr/rnd-shuffle rnd (vec (range 256)))
        perm        (vec (concat p p))
        perm-mod-12 (mapv (fn [n] (mod n 12)) perm)]
    (reify Noise
      (snoise [this xin yin]
        (let [s (* f2 (+ xin yin))
              i (fast-floor (+ xin s))
              j (fast-floor (+ yin s))
              t (* (+ i j) g2)
              X0 (- i t)
              Y0 (- j t)
              x0 (- xin X0)
              y0 (- yin Y0)
              [i1 j1] (if (> x0 y0)
                        [1 0]
                        [0 1])
              x1 (+ (- x0 i1) g2)
              y1 (+ (- y0 j1) g2)
              x2 (+ (dec x0) (* 2 g2))
              y2 (+ (dec y0) (* 2 g2))
              ii (bit-and i 0xFF)
              jj (bit-and j 0xFF)
              gi0 (nth perm-mod-12 (+ ii (nth perm jj)))
              gi1 (nth perm-mod-12 (+ ii i1 (nth perm (+ jj j1))))
              gi2 (nth perm-mod-12 (+ ii 1 (nth perm (inc jj))))
              t0  (- 0.5 (* x0 x0) (* y0 y0))
              n0  (if (neg? t0)
                    0.0
                    (* t0 t0 t0 t0 (dot (nth grad3 gi0) x0 y0)))
              t1  (- 0.5 (* x1 x1) (* y1 y1))
              n1  (if (neg? t1)
                    0.0
                    (* t1 t1 t1 t1 (dot (nth grad3 gi1) x1 y1)))
              t2  (- 0.5 (* x2 x2) (* y2 y2))
              n2  (if (neg? t2)
                    0.0
                    (* t2 t2 t2 t2 (dot (nth grad3 gi2) x2 y2)))]
          (* 70.0 (+ n0 n1 n2))))
      (noise
        [this x y]
        (+ 0.5 (* 0.5 (snoise this x y))))
      (snoise3d [this xin yin zin]
        (let [;var n0, n1, n2, n3; // Noise contributions from the four corners
		      ; Skew the input space to determine which simplex cell we're in
			  ; Very nice and simple skew factor for 3D 
			  s (* (+ xin yin zin) f3)
			  i (fast-floor (+ xin s))
			  j (fast-floor (+ yin s))
			  k (fast-floor (+ zin s))
			  t (* (+ i j k) g3)
              ; Unskew the cell origin back to (x,y,z) space
			  X0 (- i t)
			  Y0 (- j t)
			  Z0 (- k t)
              ; The x,y,z distances from the cell origin
			  x0 (- xin X0)
			  y0 (- yin Y0)
			  z0 (- zin Z0)
			  ; For the 3D case, the simplex shape is a slightly irregular tetrahedron.
			  ; Determine which simplex we are in.
               ; Offsets for second corner of simplex in (i,j,k) coords
			  [i1, j1, k1
               ; Offsets for third corner of simplex in (i,j,k) coords
			   i2, j2, k2] (if (>= x0 y0)
							 (cond
							   ; X Y Z order
							   (>= y0 z0)
							 	[1 0 0 1 1 0]
							   ; X Z Y order
							   (>= x0 z0)
							 	[1 0 0 1 0 1]
							   ; Z X Y order
							   :else
							 	[0 0 1 1 0 1])
							 ; x0<y0
							 (cond
							   ; Z Y X order
							   (< y0 z0)
							 	[0 0 1 0 1 1]
							   ; Y Z X order
							   (< x0 z0)
							 	[0 1 0 0 1 1]
							   ; Y X Z order
							   :else
							 	[0 1 0 1 1 0]))
			  ; A step of (1,0,0) in (i,j,k) means a step of (1-c,-c,-c) in (x,y,z),
			  ; a step of (0,1,0) in (i,j,k) means a step of (-c,1-c,-c) in (x,y,z), and
			  ; a step of (0,0,1) in (i,j,k) means a step of (-c,-c,1-c) in (x,y,z), where
			  ; c = 1/6.
			  x1 (+ (- x0 i1) g3) ; Offsets for second corner in (x,y,z) coords
			  y1 (+ (- y0 j1) g3)
			  z1 (+ (- z0 k1) g3)
			  x2 (+ (- x0 i2) (* 2.0 g3)); Offsets for third corner in (x,y,z) coords
			  y2 (+ (- y0 j2) (* 2.0 g3))
			  z2 (+ (- z0 k2) (* 2.0 g3))
			  x3 (+ (- x0 1.0) (* 3.0 g3)) ; Offsets for last corner in (x,y,z) coords
			  y3 (+ (- y0 1.0) (* 3.0 g3))
			  z3 (+ (- z0 1.0) (* 3.0 g3))
			  ; Work out the hashed gradient indices of the four simplex corners
			  ii (bit-and i 0xFF)
			  jj (bit-and j 0xFF)
			  kk (bit-and k 0xFF)
              gi0 (* (nth perm-mod-12 (+ ii (nth perm (+ jj (nth perm kk))))) 1.0)
              gi1 (* (nth perm-mod-12 (+ ii i1 (nth perm (+ jj j1 (nth perm (+ kk k1)))))) 1.0)
              gi2 (* (nth perm-mod-12 (+ ii i2 (nth perm (+ jj j2 (nth perm (+ kk k2)))))) 1.0)
              gi3 (* (nth perm-mod-12 (+ ii 1 (nth perm (+ jj 1 (nth perm (inc kk)))))) 1.0)
              ;_ (println gi0 gi1 gi2 gi3 (count grad3))
			  ; Calculate the contribution from the four corners
			  t0 (- 0.6  (* x0 x0) (* y0 y0) (* z0 z0))
              n0  (if (neg? t0)
                    0.0
                    (* t0 t0 t0 t0 (dot (nth grad3 gi0) x0 y0 z0)))
			  t1 (- 0.6 (* x1 x1) (* y1 y1) (* z1 z1))
              n1  (if (neg? t1)
                    0.0
                    (* t1 t1 t1 t1 (dot (nth grad3 gi1) x1 y1 z1)))
			  t2 (- 0.6 (* x2 x2) (* y2 y2) (* z2 z2))
              n2  (if (neg? t2)
                    0.0
                    (* t2 t2 t2 t2 (dot (nth grad3 gi2) x2 y2 z2)))
			  t3 (- 0.6 (* x3 x3) (* y3 y3) (* z3 z3))
              n3  (if (neg? t3)
                    0.0
                    (* t3 t3 t3 t3 (dot (nth grad3 gi3) x3 y3 z3)))]
			  ; Add contributions from each corner to get the final noise value.
			  ; The result is scaled to stay just inside [-1,1]
			  (* 32.0 (+ n0 n1 n2 n3))))
      (noise3d
        [this x y z]
        (+ 0.5 (* 0.5 (snoise3d this x y z))))))))




;; REPL code for displaying noise as ascii art
;(def px (partition 200 (for [x (range -100 100) y (range -100 100)] (noise n (/ x 10) (/ y 10)))))

(def ^:private ascii-chars [\# \A \@ \$ \% \= \+ \* \: \, \. \space])

;;(def n (create-noise))

(defn print-fn
  ([f width height]
  (print-fn (fn [v] 
              (let [idx (if (zero? v)
                            (dec (count ascii-chars)) 
                            (- (count ascii-chars)
                               (* v (count ascii-chars))))]
                (nth ascii-chars (min (max idx 0) (dec (count ascii-chars))))))
            f width height))
  ([map-f f width height]
  (let [px (partition height (for [x (range (- (/ width 2)) (/ width 2))
                                   y (range (- (/ height 2)) (/ height 2))]
                               (f (/ x 10) (/ y 10))))]
    (doseq [line px]
      (println
        (apply str (map map-f line)))))))

