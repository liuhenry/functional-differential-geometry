;; Define E-L functional equation
(define ((Lagrange-equations Lagrangian) w)
  (- (D (compose ((partial 2) Lagrangian) (Gamma w)))
     (compose ((partial 1) Lagrangian) (Gamma w))))

;; Define free Lagrangian
(define ((Lfree mass) state)
  (* 1/2 mass (square (velocity state))))

;; Transform colatitude (theta) and longitude (phi) to R^3 embedding
(define ((sphere->R3 R) state)
  (let ((q (coordinate state)))
    (let ((theta (ref q 0)) (phi (ref q 1)))
      (up (* R (sin theta) (cos phi)) ; x
	  (* R (sin theta) (sin phi)) ; y
	  (* R (cos theta))))))       ; z

;; Applies coordinate transformation to state
(define ((F->C F) state)
  (up (time state)                     ; time unchanged
      (F state)                        ; transform coordinate
      (+ (((partial 0) F) state)       ; dF/dt + dF/dq * qdot (chain rule)
	 (* (((partial 1) F) state)
	    (velocity state)))))

;; Free motion on sphere is free Lagrangian plus state transformation
(define (Lsphere m R)
  (compose (Lfree m) (F->C (sphere->R3 R))))

;; Symbolic value of free-sphere Langrangian
((Lsphere 'm 'R)
 (up 't (up 'theta 'phi) (up 'thetadot 'phidot)))


















