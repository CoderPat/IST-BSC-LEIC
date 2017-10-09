;;; this is your solution file
(load "SolF3.lisp")

(defun states-to-list (stts)
  (loop for st in stts
;	  collect (format nil "POS: ~a VEL: ~a ACT: ~a COST: ~a Best-Heuristic ~a Good-Heuristic ~a ~&"
;	  (state-pos st)  (state-vel st)  (state-action st)  (state-cost st) (best-heuristic st) (good-heuristic st))))
	  collect (format nil "POS: ~a VEL: ~a ACT: ~a COST: ~a~&"
	  (state-pos st)  (state-vel st)  (state-action st)  (state-cost st))))

(defun initial-state (track)
  (make-state :pos (track-startpos track) :vel (make-vel 0 0) :action nil :cost 0 :track track))

(defvar *t1* nil)
(defvar *p1* nil)

(setf *t1* (loadtrack "track0.txt"))

(format t "~&Exercise 3.1 - Heuristic~&")
(with-open-file (str "out3.1.txt"
		 :direction :input)
;  (print (list (compute-heuristic (initial-state *t1*)) (compute-heuristic (make-state :pos '(1 6)  :track track)) (compute-heuristic (make-state :pos '(2 8)  :track track))))
  (format t "~% Solution is correct? ~a~&" (equal (list (compute-heuristic (initial-state *t1*)) (compute-heuristic (make-state :pos '(1 6)  :track track)) (compute-heuristic (make-state :pos '(2 8)  :track track))) (read str))))


(setf *p1* (make-problem :initial-state (initial-state *t1*)  :fn-isGoal #'isGoalp	  :fn-nextstates #'nextStates	  :fn-h #'compute-heuristic))
			  
(format t "~&Exercise 3.2 - A*~&")
 (let ((real1 (get-internal-real-time)))
		 (with-open-file (str "out3.2.txt" :direction :input)
;		 (format t "~{~a~^~}" (states-to-list (a* *p1*)))
;		 (terpri)
	  (time (a* *p1*))
	  ;(format t "~{~a~^~}" (states-to-list (a* *p1*)))
	   (format t "~% Solution is correct? ~a~&" (string= (format nil "~{~a~^~}" (states-to-list (a* *p1*))) (read str))))
      (let ((real2 (get-internal-real-time)))
	  (format t "~%Computation took: ~f seconds of real time~%" (/ (- real2 real1) internal-time-units-per-second))))
		
(defvar *t2* nil)
(defvar *p2* nil)

(setf *t2* (loadtrack "track9.txt"))

(format t "~&Exercise 3.1b - Heuristic~&")
(with-open-file (str "out3.1b.txt"
		 :direction :input)
  (format t "~% Solution is correct? ~a~&" (equal (list (compute-heuristic (initial-state *t2*)) (compute-heuristic (make-state :pos '(3 6)  :track track )) (compute-heuristic (make-state :pos '(3 8)  :track track ))) (read str))))
 
 
(setf *p2* (make-problem :initial-state (initial-state *t2*)  :fn-isGoal #'isGoalp	  :fn-nextstates #'nextStates	  :fn-h #'compute-heuristic))
			  
(format t "~&Exercise 3.2b - A*~&")
 (let ((real1 (get-internal-real-time)))
		 (with-open-file (str "out3.2b.txt" :direction :input)
;		 (format t "~{~a~^~}" (states-to-list (a* *p2*)))
;		 (terpri)
	   (time (a* *p2*))
	;(format t "~{~a~^~}" (states-to-list (a* *p2*)))
	   (format t "~% Solution is correct? ~a~&" (string= (format nil "~{~a~^~}" (states-to-list (a* *p2*))) (read str))))
      (let ((real2 (get-internal-real-time)))
	  (format t "~%Computation took: ~f seconds of real time~%" (/ (- real2 real1) internal-time-units-per-second))))

(format t "~&Exercise 42.0 - A*~&")
(defvar *t0* nil)
(defvar *p0* nil)
(setf *t0* (loadtrack "track_edge_case.txt"))
(setf *p0* (make-problem :initial-state (initial-state *t0*)  :fn-isGoal #'isGoalp	  :fn-nextstates #'nextStates	  :fn-h #'compute-heuristic))
(defvar *t3* nil)
(defvar *p3* nil)
(setf *t3* (loadtrack "ntrack1.txt"))
(setf *p3* (make-problem :initial-state (initial-state *t3*)  :fn-isGoal #'isGoalp	  :fn-nextstates #'nextStates	  :fn-h #'compute-heuristic))
(defvar *t4* nil)
(defvar *p4* nil)
(setf *t4* (loadtrack "track10000.txt"))
(setf *p4* (make-problem :initial-state (initial-state *t4*)  :fn-isGoal #'isGoalp	  :fn-nextstates #'nextStates	  :fn-h #'compute-heuristic))
(defvar *thack* nil)
(defvar *phack* nil)
(setf *thack* (loadtrack "track_hack.txt"))
(setf *phack* (make-problem :initial-state (initial-state *thack*)  :fn-isGoal #'isGoalp	  :fn-nextstates #'nextStates	  :fn-h #'compute-heuristic))
(defvar *thard* nil)
(defvar *phard* nil)
(setf *thard* (loadtrack "track_hard.txt"))
(setf *phard* (make-problem :initial-state (initial-state *thard*)  :fn-isGoal #'isGoalp	  :fn-nextstates #'nextStates	  :fn-h #'compute-heuristic))
(defvar *t007* nil)
(defvar *p007* nil)
(setf *t007* (loadtrack "track007.txt"))
(setf *p007* (make-problem :initial-state (initial-state *t007*)  :fn-isGoal #'isGoalp	  :fn-nextstates #'nextStates	  :fn-h #'compute-heuristic))



(time (a* *p0*))
(format t "~{~a~^~}" (states-to-list (a* *p0*)))


(print "Best search p0:")
(time (best-search *p0*))
(format t "~{~a~^~}" (states-to-list (best-search *p0*)))

(print "Best search p1:")
(time (best-search *p1*))
(format t "~{~a~^~}" (states-to-list (best-search *p1*)))

(print "Best search p2:")
(time (best-search *p2*))
(format t "~{~a~^~}" (states-to-list (best-search *p2*)))

(print "Best search thack:")
(time (best-search *phack*))
(format t "~{~a~^~}" (states-to-list (best-search *phack*)))

(print "Best search p007:")
(time (best-search *p007*))
(format t "~{~a~^~}" (states-to-list (best-search *p007*)))

(print "Best search thard:")
(time (best-search *phard*))
(format t "~{~a~^~}" (states-to-list (best-search *phard*)))


(print "Best search p3:")
(time (best-search *p3*))
(format t "~{~a~^~}" (states-to-list (best-search *p3*)))

(print "Best search p4:")
(time (best-search *p4*))
(format t "~{~a~^~}" (states-to-list (best-search *p4*)))

(quit)

(format t "~&Exercise 69.1 - A*~&")

(time (a* *p3*))
(format t "~{~a~^~}" (states-to-list (a* *p3*)))


(format t "~&Exercise 69.1 - A*~&")

(time (a* *p4*))
(format t "~{~a~^~}" (states-to-list (a* *p4*)))
