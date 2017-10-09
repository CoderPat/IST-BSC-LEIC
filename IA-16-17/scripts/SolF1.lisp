
;;; These functions, and any other ones needed must be implemented

;;; Utilizar estes includes para os testes na versao local
;;; comentar antes de submeter
(load "datastructures.lisp")
(load "auxfuncs.lisp")

;;; Utilizar estes includes para a versao a submeter
; tirar o comentario antes de submeter
;(load "datastructures.fas")
;(load "auxfuncs.fas")


(defun isObstaclep (pos track) 
  "check if there is an obstacle at position pos of the track"
  (if (not (or (minusp (first pos)) (minusp (second pos))))
  		(not (nth (second pos) (nth (first pos) (track-env track))))
  		T
  )
)


(defun isGoalp (st) 
  "check if st is a goal state"
  (let ((pos (state-pos st))
  	    (goal-states (track-endpositions (state-track st))))
  	   (dolist (goal_pos goal-states NIL) 
  	   		(if (and (eql (first pos) (first goal_pos)) (eql (second pos) (second goal_pos)))
  	   			(return T)
  	   		)
  	   	)
  )
)

(defun nextState (st act)
 "generate the nextState after state st and action act"
 (let* ((next_state (make-state :action act :track (state-track st)))
 	   	(next_vel (list (+ (first act) (first (state-vel st))) (+ (second act) (second (state-vel st)))))
 	   	(next_pos (list (+ (first next_vel) (first (state-pos st))) (+ (second next_vel) (second (state-pos st))))))
 	   (if (isObstaclep next_pos (state-track st)) 
 	   		(progn (setf (state-pos next_state) (state-pos st)) 
 	   			   (setf (state-vel next_state) '(0 0))
 	   			   (setf (state-cost next_state) 20)
 	   			   next_state
 	   		)
 	   		(progn (setf (state-pos next_state) next_pos) 
 	   			   (setf (state-vel next_state) next_vel)
 	   		       (if (isGoalp next_state)
 	   		 			(setf (state-cost next_state) -100)
 	   		 			(setf (state-cost next_state) 1)
 	   		 	   )
 	   			   next_state
 	   		)
 	   	)
 )
)


  	


