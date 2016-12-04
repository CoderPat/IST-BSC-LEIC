(load "datastructures.lisp")
(load "auxfuncs.lisp")


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


;; Solution of phase 1
(defun addPair (a b)
  "Adds two lists on length 2"
  (list (+ (car a) (car b)) (+ (cadr a) (cadr b)))
)

(defun makePair(a b)
  (list a b))

(defun isObstaclep (pos track) 
  "check if there is an obstacle at position pos of the track"
  (when (or (minusp (car pos)) (minusp (cadr pos))) (return-from isObstaclep t))
  (when (or (>= (car pos) (car (track-size track))) (>= (cadr pos) (cadr (track-size track)))) (return-from isObstaclep t))
  (not (nth (cadr pos) (nth (car pos) (track-env track)))))

(defun isGoalp (st)
  "check if st is a goal state"
  (loop for e in (track-endpositions (state-track st))
    do (when (and 
              (= (car e) (car (state-pos st))) 
              (= (cadr e) (cadr (state-pos st)))
           )
           (return t)
       )
  )
)

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

;; Solution of phase 2
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


;; Solution of phase 3

;; Heuristic
;; ofc this is not an admissible heuristic
(defun compute-heuristic (st)
  (when (isObstaclep (state-pos st) (state-track st)) (return-from compute-heuristic MOST-POSITIVE-FIXNUM))
  (let ( (q (list (make-state
                    :POS (state-pos st)
                    :VEL '(0 0)
                    :ACTION '(0 0)
                    :COST 0
                    :TRACK (state-track st)
         )))
         (nq (list))
         (done (make-array (list (car (track-size (state-track st))) (cadr (track-size (state-track st)))) :INITIAL-ELEMENT t))
         (ans 0)
       )
       (setf (aref done (car (state-pos st)) (cadr (state-pos st))) nil)
       (loop while q
         do
           (setf nq q)
           (setf q nil)
           (loop for w in nq
            do
              (when (isGoalp w) (return-from compute-heuristic ans))
              (loop for n in (nextStates w)
               do
                 (when (aref done (car (state-pos n)) (cadr (state-pos n)))
                       (setf (state-vel n) '(0 0))
                       (setf q (cons n q))
                       (setf (aref done (car (state-pos n)) (cadr (state-pos n))) nil))
           ))
           (incf ans)
       ) 
  )
  MOST-POSITIVE-FIXNUM)

;;; state_compressed:
; state_id: ((posx, posy) (velx, vely))
; state_v: (g_score, f_score, (fromx, fromy))
;;;

;;; pqueue:
; val: f_score
; state_id: state_id
; l, r: pqueue
;;;
;(defstruct skey
; px
; py
; vx
; vy) => int with compressed shit
(defconstant MAX-VX 15)
(defconstant MAX-VXS (1+ (* 2 MAX-VX)))
(defconstant MAX-VY 15)
(defconstant MAX-VYS (1+ (* 2 MAX-VY)))
(defconstant MAX-PX 102) ;maximum pos is this-1
(defun compress-coord (px py vx vy)
  (+ (+ vx MAX-VX) (* MAX-VXS (+ (+ vy MAX-VY) (* MAX-VYS (+ px (* MAX-PX py)))))))
(defun decompress-coord-vx (coord)
  (- (mod coord MAX-VXS) MAX-VX))
(defun decompress-coord-vy (coord)
  (- (mod (floor coord MAX-VXS) MAX-VYS) MAX-VY))
(defun decompress-coord-px (coord)
  (mod (floor (floor coord MAX-VXS) MAX-VYS) MAX-PX))
(defun decompress-coord-py (coord)
  (floor (floor (floor coord MAX-VXS) MAX-VYS) MAX-PX))


;State indexes are implicit
(defstruct fullstate
  state
  f
  g
  pqid
  parent)
;TODO: define table somewhere

(defclass pq ()
  ((arr
   :initform nil)
   (upd8)
   (cmp)))

(defmethod initialize-instance :after ((q pq) &key updater comparator)
  (setf (slot-value arr q) (make-array 1 :adjustable t :fill-pointer 0))
  (setf (slot-value upd8 q) updater)
  (setf (slot-value cmp q) comparator))

(defmethod pq-insert ((q pq) id)
     (with-slots (arr) q
          (vector-push-extend id arr (ceiling (* 0.5  (array-total-size arr))))
          (up8 id (1- (fill-pointer arr)))
          (pq-fixup q (1- (fill-pointer arr)))))

(defmethod pq-popmin ((q pq)) 
  (with-slots (arr upd8) q
    (when (= (fill-pointer arr) 0) (return-from pq-popmin nil))
    (let ((ret (aref arr 0)))
      (rotatef (aref arr 0) (aref arr (1- (fill-pointer arr))))
      (upd8 (aref arr 0) 0)
      (upd8 (aref arr (1- (fill-pointer arr))) -1)
      (decf (fill-pointer arr))
      (pq-fixdown q 0)
      ret
    )))

(defmethod pq-fixup ((q pq) ind)
  (with-slots (arr upd8) q
  (do ((p (floor (1- ind) 2)))
      (when (= ind 0) (return nil))
      (when (not (cmp ind p)) (return nil))
      (rotatef (aref arr ind) (aref arr p))
      (upd8 (aref arr ind) ind)
      (upd8 (aref arr p) p)
      (rotatef arr ind))))

(defmethod pq-fixdown ((q pq) ind)
  (with-slots (arr upd8) q
    (do ((l (+ 1 (* ind 2)))
         (r (+ 2 (* ind 2))))
        (cond ((>= l (length arr)) (return-from pq-fixdown nil))
              ((and (>= r (fill-pointer arr)) (cmp l ind)) t)
              ((>= r (fill-pointer arr)) (return-from pq-fixdown nil))
              ((and (cmp l r) (cmp l ind)) t)
              ((and (cmp r l) (cmp r ind)) (setf l r))
              (t (return-from pq-fixdown nil)))
        (rotatef (aref arr ind) (aref arr l))
        (upd8 (aref arr ind) ind)
        (setf ind l)
        (upd8 (aref arr ind) ind))))

(defun state-to-new-fullstate (st parent)
  (make-fullstate state most-positive-fixnum most-positive-fixnum nil parent)

(defun state-to-coord (st)
  (compress-coord (car (state-pos st)) (cadr (state-pos st)) (car (state-vel st)) (cadr (state-vel st))))

(defun coord-to-state (coord)
  (fullstate-state (gethash coord table)))

(defun updater1 (coord ind)
  (setf (fullstate-pqid (gethash coord table)) ind))

(defun cmp1 (coord1 coord2)
  (<= (fullstate-f (gethash coord1 table)) (fullstate-f (gethash coord2 table))))

(defun rebuil-path (coord)
  (let ((ret (list)))
    (loop while coord do
      (setf ret (cons (coord-to-state coord) ret))
      (setf coord (fullstate-parent (gethash coord table)))      
    )))

;;; A*
(defun a* (problem)
  ;Default heuristic -> 
  (defparameter table (make-hash-table))
  (defparameter done (make-hash-table))
  (let ((q (make-instance 'pq :updater update1 :comparator cmp1))
        (curr nil)
        (heuristic (problem-heuristic problem)))
    (setf 
      (gethash (state-to-coord (problem-initial-state problem)) table)
      (state-to-new-fullstate (problem-initial-state problem) nil))
    (setf (fullstate-g (gethash (state-to-coord (problem-initial-state problem)) table)) 0)
    (setf 
      (fullstate-f (gethash (state-to-coord (problem-initial-state problem)) table)) 
      (funcall heuristic (problem-initial-state problem)))
    (pq-insert q (state-to-coord (problem-initial-state problem)))
    (loop while (setf curr (pq-popmin q)) do
      (block continue1
        (setf (gethash curr done) t)
        (when (isGoalp (coord-to-state curr)) (return-from a* (rebuil-path curr)))
        (loop for st in (funcall (problem-fn-nextStates problem) (coord-to-state curr)) do
          (block continue2
            (let ((added (+ (fullstate-g (gethash curr table)) (st-cost st)))
                  (prox (state-to-coord st))
                  (aux 0))
              (when (gethash prox done) (return-from continue2))
              (when (nil (gethash prox table)) 
                (setf (gethash prox table) (state-to-new-fullstate st curr))
                (setf (fullstate-g (gethash prox table)) added)
                (setf (fullstate-f (gethash prox table)) (+ added (pro)))
                (pq-insert q prox))
              (when (>= added (fullstate-g (gethash prox table))) (return-from continue2))
              (setf aux (fullstate-pqid (gethash prox table)))
              (setf (gethash prox table) (state-to-new-fullstate st curr))
              (setf (fullstate-pqid (gethash prox table)) aux)
              (setf (fullstate-g (gethash prox table)) added)
              (setf (fullstate-f (gethash prox table)) (+ added (pro)))
              (pq-fixup q)
              )))
)))
nil)
