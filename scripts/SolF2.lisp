(load "datastructures.fas")
(load "auxfuncs.fas")

;;Useless functions:
;;; TAI position
(defun make-pos (c l)
  (list c l))
(defun pos-l (pos)
  (first pos))
(defun pos-c (pos)
  (second pos))
  ;;; TAI acceleration
(defun make-acce (c l)
  (list c l))
(defun acce-l (pos)
  (first pos))
(defun acce-c (pos)
  (second pos))
;;; TAI velocity
(defun make-vel (c l)
  (list c l))
(defun vel-l (pos)
  (first pos))
(defun vel-c (pos)
  (second pos))

;;Actual code that does not use useless functions

(defun addPair (a b)
  "Adds two lists on length 2"
  (list (+ (car a) (car b)) (+ (cadr a) (cadr b)))
)

(defun isObstaclep (pos track) 
  "check if there is an obstacle at position pos of the track"
  (when (or (minusp (car pos)) (minusp (cadr pos))) (return-from isObstaclep t))
  (when (or (>= (car pos) (car (track-size track))) (>= (cadr pos) (cadr (track-size track)))) (return-from isObstaclep t))
  (not (nth (cadr pos) (nth (car pos) (track-env track)))))

(defun isGoalp (st) 
  "check if st is a solution of the problem"
  (and (member (state-pos st) (track-endpositions (state-track st)) :test #'equalp)
  T))

(defun nextState (st act)
  "generate the nextState after state st and action act"
  (let ((vel (addPair (state-vel st) act)))
    (if (isObstaclep (addPair (state-pos st) vel) (state-track st))
    (make-STATE :POS (state-pos st)
	      :VEL '(0 0)
	      :ACTION act
	      :COST 20
        :TRACK (state-track st))
    (let ((ns (make-STATE :POS (addPair (state-pos st) vel)
	                       :VEL vel
                         :ACTION act
                         :COST 1
                         :TRACK (state-track st))))
        (if (isGoalp ns) (setf (state-cost ns ) -100) t)
        ns   
))))


(defun nextStates (st)
  "Returns a list of all the possible states next to a given state"
  (list (nextState st '(-1 -1))
        (nextState st '(1 1))
        (nextState st '(-1 1))
        (nextState st '(1 -1))
        (nextState st '(0 -1))
        (nextState st '(-1 0))
        (nextState st '(1 0))
        (nextState st '(0 1))
        (nextState st '(0 0))))


(defstruct ds
  fn-nextStates
  fn-isGoal
  path
  cutted)

(defun dfsi (dd state path lim) 
  "Does a dfs with a cutoff length"
  (when (funcall (ds-fn-isGoal dd) state) 
    (setf (cdr path) nil)
    (return-from dfsi t))
  (when (<= lim 0) 
    (setf (ds-cutted dd) t) 
    (return-from dfsi nil))
  (loop for st in (funcall (ds-fn-nextStates dd) state)
    do 
      (setf (cadr path) st)
      (when (dfsi dd st (cdr path) (- lim 1)) (return-from dfsi t))
  )
  (return-from dfsi nil)
)

;;; limdepthfirstsearch 
(defun limdepthfirstsearch (problem lim)
  "limited depth first search
     problem - problem information
     lim - depth limit"
  (let ((ans (make-ds 
                :fn-nextStates (problem-fn-nextStates problem) 
                :fn-isGoal (problem-fn-isGoal problem) 
                :path (cons (problem-initial-state problem) (make-list lim)) 
                :cutted nil)))
	  (when (dfsi ans (problem-initial-state problem) (ds-path ans) lim) (return-from limdepthfirstsearch (ds-path ans)))
    (if (ds-cutted ans) :corte nil)
  ))


;iterlimdepthfirstsearch
(defun iterlimdepthfirstsearch (problem &key (lim most-positive-fixnum))
  "limited depth first search
     st - initial state
     problem - problem information
     lim - limit of depth iterations"
  (loop for n from 1 to lim do
	 (let ((x (limdepthfirstsearch problem n)))
    (when (null x) (return-from iterlimdepthfirstsearch nil))
    (when (not (eq x :corte)) (return-from iterlimdepthfirstsearch x)) 
)))