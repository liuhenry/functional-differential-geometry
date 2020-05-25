;; Define E-L functional equation
(define ((Lagrange-equations Lagrangian) w)
  (- (D (compose ((partial 2) Lagrangian) (Gamma w)))
     (compose ((partial 1) Lagrangian) (Gamma w))))

;; Define state-space path Gamma from configuration-space path w
(define ((Gamma w) t)
  (up t (w t) ((D w) t)))

;; Lagrangian for harmonic oscillator
(define ((L-harmonic m k) local)
  (let ((q (coordinate local))
	(v (velocity local)))
    (- (* 1/2 m (square v))
       (* 1/2 k (square q)))))

;; The standard (explicit sinusoid) solution for H.O.
(define (proposed-solution t)
  (* 'a (cos (+ (* 'omega t) 'phi))))

;; Show the residual, where k-mw^2 = 0 if a != 0
(show-expression
 (((Lagrange-equations (L-harmonic 'm 'k))
   proposed-solution)
  't))

;; Show the residual for an implicit function x(t)
(show-expression
 (((Lagrange-equations (L-harmonic 'm 'k))
   (literal-function 'x))
  't))

