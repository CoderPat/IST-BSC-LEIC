;Grupo al-105
;81191 Patrick Fernandes
;81861 Afonso Tinoco
(load "datastructures.fas")
(load "auxfuncs.fas")


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

;; caching structures
;; We assume that tracks are loaded from files and are not modified during runtime,
;;  and therefore we use eq to compare just the object-ids. It seemed a reasonable assumption 
;;  taken from the handout. In case this is not a valid assumption, ou isObstaclep and 
;;  compute-heuristic will fail and changes are needed to them.
;; We decided to keep the code as is as the evaluator provided sugested this is acceptable.
(defparameter heuristic-track nil)
(defparameter heuristic-arr nil)
(defparameter obstacle-track nil)
(defparameter obstacle-arr nil)

(defun isObstaclep (pos track)
  "check if there is an obstacle at position pos of the track"
  (when (or (minusp (car pos)) (minusp (cadr pos))) (return-from isObstaclep t))
  (when (or (>= (car pos) (car (track-size track))) (>= (cadr pos) (cadr (track-size track)))) (return-from isObstaclep t))
  ;check for cache validity
  (when (eq obstacle-track track) 
     (return-from isObstaclep (not (aref obstacle-arr (car pos) (cadr pos)))))
  ;fills the cache
  (setf obstacle-track track)
  (setf obstacle-arr (make-array (list (length (track-env track))
                                       (length (first (track-env track))))
                                       :initial-contents (track-env track)))
  (not (aref obstacle-arr (car pos) (cadr pos))))

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
(defparameter table nil)
(defparameter done nil)


;; Heuristic
;; ofc this is not an admissible heuristic
(defun compute-heuristic (st)
  ;check for cache validity
  (when (eq heuristic-track (state-track st)) 
     (return-from compute-heuristic (aref heuristic-arr (1+ (car (state-pos st))) (1+ (cadr (state-pos st))))))

  (defparameter heuristic-track (state-track st))
  (let* ((W (car (track-size (state-track st))))
         (H (cadr (track-size (state-track st))))
         (q nil)
         (nq nil)
         (endst (cons (1+ (car (state-pos st))) (1+ (cadr (state-pos st)))))
         (ans 1))
       (defparameter heuristic-arr (make-array (list (+ 2 W) (+ 2 H)) :initial-element MOST-POSITIVE-FIXNUM))
       (loop for i from 1 to (- W 1) 
             for x in (track-env (state-track st)) do
           (loop for j from 1 to (- H 1) 
                 for y in x do
               (setf (aref heuristic-arr i j) -1)
               (when (null y) (setf (aref heuristic-arr i j) MOST-POSITIVE-FIXNUM))
           )
       )
       
       (loop for p in (track-endpositions (state-track st)) do
           (setf (aref heuristic-arr (1+ (car p)) (1+ (cadr p))) 0)
           (setf q (cons (cons (1+ (car p)) (1+ (cadr p))) q))
       )
       (loop while q
         do
           (setf nq q)
           (setf q nil)
           (loop for w in nq
            do
              (loop for i in (list (car w) (car w) (car w) (1+ (car w)) (1+ (car w)) (1+ (car w)) (1- (car w)) (1- (car w)) (1- (car w)))
                    for j in (list (cdr w) (1+ (cdr w)) (1- (cdr w)) (cdr w) (1+ (cdr w)) (1- (cdr w)) (cdr w) (1+ (cdr w)) (1- (cdr w)))
               do
               (block continue
                 (when (not (= -1 (aref heuristic-arr i j))) (return-from continue))
                 (setf (aref heuristic-arr i j) ans)
                 (setf q (cons (cons i j) q)))))
           (incf ans)
       )
  (aref heuristic-arr (car endst) (cdr endst))
))



;--------- priority queue methods--------

(defun upd8 (coord ind)
  (setf (fullstate-pqid (gethash coord table)) ind))

(defun cmp (coord1 coord2)
  (<=  (fullstate-f (gethash coord1 table)) (fullstate-f (gethash coord2 table))))

(defun pq-insert (arr id)
  (vector-push-extend id arr (ceiling (* 0.5  (array-total-size arr))))
  (upd8 id (1- (fill-pointer arr)))
  (pq-fixup arr (1- (fill-pointer arr))))

(defun pq-popmin (arr) 
  (when (= (fill-pointer arr) 0) (return-from pq-popmin nil))
  (let ((ret (aref arr 0)))
    (rotatef (aref arr 0) (aref arr (1- (fill-pointer arr))))
    (upd8 (aref arr 0) 0)
    (upd8 (aref arr (1- (fill-pointer arr))) -1)
    (decf (fill-pointer arr))
    (pq-fixdown arr 0)
    ret
  ))

(defun pq-fixup (arr ind)
  (do ((p (truncate (1- ind) 2) (truncate (1- ind) 2)))
      ((or (= ind 0) (not (cmp (aref arr ind) (aref arr p)))))
      (rotatef (aref arr ind) (aref arr p))
      (upd8 (aref arr ind) ind)
      (upd8 (aref arr p) p)
      (rotatef p ind)))

(defun pq-fixdown (arr ind)
  (do ((l (+ 1 (* ind 2)) (+ 1 (* ind 2)))
        (r (+ 2 (* ind 2)) (+ 2 (* ind 2))))
      (nil)
      (cond ((>= l (length arr)) (return-from pq-fixdown nil))
            ((and (>= r (fill-pointer arr)) (cmp (aref arr l) (aref arr ind))) t)
            ((>= r (fill-pointer arr)) (return-from pq-fixdown nil))
            ((and (cmp (aref arr l) (aref arr r)) (cmp (aref arr l) (aref arr ind))) t)
            ((and (cmp (aref arr r) (aref arr l)) (cmp (aref arr r) (aref arr ind))) (setf l r))
            (t (return-from pq-fixdown nil)))
      (rotatef (aref arr ind) (aref arr l))
      (upd8 (aref arr ind) ind)
      (setf ind l)
      (upd8 (aref arr ind) ind)))


;--------------fullstate and methods ----------------
(defparameter MAX-PX nil)
(defparameter MAX-VX nil)
(defparameter MAX-VXS nil)
(defparameter MAX-VY nil)
(defparameter MAX-VYS nil)

(defun compress-coord (px py vx vy)
  (+ (+ vx MAX-VX) (* MAX-VXS (+ (+ vy MAX-VY) (* MAX-VYS (+ px (* MAX-PX py)))))))
(defun decompress-coord-vx (coord)
  (- (mod coord MAX-VXS) MAX-VX))
(defun decompress-coord-vy (coord)
  (- (mod (truncate coord MAX-VXS) MAX-VYS) MAX-VY))
(defun decompress-coord-px (coord)
  (mod (truncate (truncate coord MAX-VXS) MAX-VYS) MAX-PX))
(defun decompress-coord-py (coord)
  (truncate (truncate (truncate coord MAX-VXS) MAX-VYS) MAX-PX))

;State indexes are implicit
(defstruct fullstate
  state
  f
  g
  pqid
  parent)


(defun state-to-new-fullstate (st parent)
  (make-fullstate 
    :STATE st 
    :F most-positive-fixnum
    :G most-positive-fixnum
    :PQID nil
    :PARENT parent))

(defun state-to-coord (st)
  (compress-coord (car (state-pos st)) (cadr (state-pos st)) (car (state-vel st)) (cadr (state-vel st))))

(defun coord-to-state (coord)
  (fullstate-state (gethash coord table)))


;-------------- A* and auxiliary functions ----------------
(defun rebuil-path (coord)
  (let ((ret (list)))
    (loop while coord do
      (setf ret (cons (coord-to-state coord) ret))
      (setf coord (fullstate-parent (gethash coord table)))      
    )
    ret))


;;; A*
(defun a* (problem)
 
  (defparameter MAX-PX (+ 2 (car (track-size (state-track (problem-initial-state problem))))))
  (defparameter MAX-VX (+ 2 (truncate (isqrt (* 8 MAX-PX)) 2)))
  (defparameter MAX-VXS (1+ (* 2 MAX-VX)))
  (defparameter MAX-VY (+ 2 (truncate (isqrt (* 8 (+ 2 (cadr (track-size (state-track (problem-initial-state problem))))))) 2)))
  (defparameter MAX-VYS (1+ (* 2 MAX-VY)))

  (defparameter table (make-hash-table))
  (defparameter done (make-hash-table))

  ;Initial variable setting
  (let ((q (make-array 1 :adjustable t :fill-pointer 0))
        (curr nil)
        (heuristic (problem-fn-h problem)))
    (setf (gethash (state-to-coord (problem-initial-state problem)) table) (state-to-new-fullstate (problem-initial-state problem) nil))
    (setf (fullstate-g (gethash (state-to-coord (problem-initial-state problem)) table)) 0)
    (setf (fullstate-f (gethash (state-to-coord (problem-initial-state problem)) table)) (funcall heuristic (problem-initial-state problem)))
    (pq-insert q (state-to-coord (problem-initial-state problem)))

    (loop while (setf curr (pq-popmin q)) do
      (block continue1
        (setf (gethash curr done) t)

        ;check for solution
        (when (isGoalp (coord-to-state curr)) 
            (defparameter heuristic-done nil)
            (return-from a* (rebuil-path curr)))

        ;explore child nodes
        (loop for st in (funcall (problem-fn-nextStates problem) (coord-to-state curr)) do
          (block continue2
            ;next state variables
            (let ((added (+ (fullstate-g (gethash curr table)) (state-cost st)))
                  (prox (state-to-coord st))
                  (aux 0))
              ;fullstate was never generated before 
              (when (null (gethash prox table)) 
                    (setf (gethash prox table) (state-to-new-fullstate st curr))
                    (setf (fullstate-g (gethash prox table)) added)
                    (setf (fullstate-f (gethash prox table)) (+ added (funcall heuristic st)))
                    (pq-insert q prox))
              ;fullstate was generated and visited (so is no longer in pqueue) se we might need to insert it back
              (when (and (= -1 (fullstate-pqid (gethash prox table)))
                         (< added (fullstate-g (gethash prox table)))
                    (setf (fullstate-state (gethash prox table)) st)
                    (setf (fullstate-parent (gethash prox table)) curr)
                    (setf (fullstate-f (gethash prox table)) (+ (- added (fullstate-g (gethash prox table))) (fullstate-f (gethash prox table))))
                    (setf (fullstate-g (gethash prox table)) added)
                    (pq-insert q prox)))
              ;fullstate was generated and is still in pqueue so might need updating
              (when (>= added (fullstate-g (gethash prox table))) (return-from continue2))
                    (setf aux (fullstate-pqid (gethash prox table)))
                    (setf (gethash prox table) (state-to-new-fullstate st curr))
                    (setf (fullstate-pqid (gethash prox table)) aux)
                    (setf (fullstate-f (gethash prox table)) (+ (- added (fullstate-g (gethash prox table))) (fullstate-f (gethash prox table))))
                    (setf (fullstate-g (gethash prox table)) added)
                    (pq-fixup q aux)
    )))))
  )
  nil
)

(defun sqrtvelaux (pos1 pos2 vel)
  (max 0 
    (- (truncate (isqrt (+ 1 (* 4 (car vel) (- (car vel) 1)) (* 8 (abs (- (car pos2) (car pos1)))))) 2) (1+ (car vel)))
    (- (truncate (isqrt (+ 1 (* 4 (cadr vel) (- (cadr vel) 1)) (* 8 (abs (- (cadr pos2) (cadr pos1)))))) 2) (1+ (cadr vel))))
)

(defun sqrtvelaux-better (pos1 pos2 vel)
  (let ((sa (signum (- (car pos2) (car pos1))))
        (sb (signum (- (cadr pos2) (cadr pos1))))
       )
    (max 0
    (+ 0 (ceiling (- 
      (isqrt (+ (* (1+ (* 2 (car vel) sa)) (1+ (* 2 (car vel) sa))) (* 8 sa (- (car pos2) (car pos1))))) 
      (1+ (* 2 (car vel) sa))) 2))
    (+ 0 (ceiling (- 
      (isqrt (+ (* (1+ (* 2 (cadr vel) sb)) (1+ (* 2 (cadr vel) sb))) (* 8 sb (- (cadr pos2) (cadr pos1))))) 
      (1+ (* 2 (cadr vel) sb))) 2))
  ))
)

(defun best-heuristic (st)
    (when (isGoalp st) (return-from best-heuristic 101))
    (reduce #'min (mapcar (lambda (epos) (sqrtvelaux-better (state-pos st) epos (state-vel st))) (track-endpositions (state-track st))))
)

(defun good-heuristic (st)
    (reduce #'min (mapcar (lambda (epos) (sqrtvelaux (state-pos st) epos (state-vel st))) (track-endpositions (state-track st))))
)

(defun admissible-heuristic (st)
  (min 
    (reduce #'min (mapcar (lambda (epos) (sqrtvelaux (state-pos st) epos (state-vel st))) (track-endpositions (state-track st))))
    (reduce #'min (mapcar (lambda (epos) (sqrtvelaux-better (state-pos st) epos (state-vel st))) (track-endpositions (state-track st))))
  )
)

(defun retzero (st)
  (when (isGoalp st) (return-from retzero 110))
  1
)

;-------------- best search and its heuristic ----------------
(defun best-search (problem)
  (setf (problem-fn-h problem) #'best-heuristic) ;TODO: Change this for an admissible heuristic
  (a* problem)
)