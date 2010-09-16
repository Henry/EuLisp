;;; Copyright 1994-2010 Fraunhofer ISST
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'Eu2C'
;;;-----------------------------------------------------------------------------
;;
;;  Eu2C is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title:
;;;  Description:
;;;  Authors: Horst Friedrich
;;;-----------------------------------------------------------------------------

#module lzs-to-mzs-fun
(import ((except (format
                  reverse)
                 level-0)
         dynamic
         simple-programming
         accessors
         debugging
         LZS
         lzs-mop
         MZS
         context
         analyse-h
         vector ; make-vector and vector-ref
         type-propagation
         type-inference
         ti-descrs
         side-effects
         apply-funs
         (only (mapc
                format
                terpri
                length
                append
                reverse)
               common-lisp)
         name-of-fun
         )
 syntax (level-0
         dynamic)
 export (lzs2mzs-fun
         indent-counter
         add-function-call
         add-closure-var))

;; pass = 0 - the function is not analysed
;; pass = 1 - side-effecs are analysed
;; pass = 2 - the analyse is started
;; pass = 3 - the analysed is finished


(defun lzs2mzs-fun (fun)
  (if (defined-generic-fun? fun) (setq fun (?discriminating-fun fun)) ())
  (cond ((or (null? (or (global-fun? fun)
                       (local-fun? fun)
                       (defined-generic-fun? fun)
                       ))
             (null? (?params fun))) fun)
        ;; !!!!!!!!!!! hack
        ;; add global-generic-fun and local-generic-fun
        ;; ((and  (print (list 'function (?identifier fun))) ()) ())
        ;; !!!!!!!!!!! debug
        ((eq (?pass fun) 1)
         (if (global-fun? fun)
             (dynamic-let ((closure ())
                           ;;(local-funs ())
                           )
                          (lzs2mzs-fun1 fun ())
                          ;;                       (format t " Closure ~A "
                          ;;                               (dynamic closure))
                          )
           ;;          (if (global-generic-fun? fun)
           ;;            (dynamic-let ((closure ())
           ;;           ;;(local-funs ())
           ;;                          )
           ;;                         (setf (?pass fun) 2)
           ;;                         (lzs2mzs-fun1 (?discriminating-fun fun) ())
           ;;                         (lzs2mzs-methods ....
           ;;          ;;                       (format t " Closure ~A "
           ;;          ;;                               (dynamic closure))
           ;;                         )
           (progn ;(dynamic-setq local-funs (cons fun
             ;;                              (dynamic local-funs)))
             (lzs2mzs-fun1 fun (if (eq (?function-type fun) $closure)
                                   (make <local-static>
                                         :identifier ^closure )
                                 ()))))) ;)
        ((eq (?pass fun) 2)
         (unless (member fun (dynamic *started-and-not-finished-functions*))
                 (setf (dynamic *started-and-not-finished-functions*)
                       (cons fun (dynamic *started-and-not-finished-functions*))))
         fun)
        (t fun)))

(deflocal indent-counter 0)

(defun print-space (n)
  (if  (<= n 0) ()
    (progn (format t " ")
           (print-space (- n 1))))
  )

(defun print-start-analyse (fun)
  ;;  (terpri)
  ;;  (print-space indent-counter)
  ;;  (format t ">>> Start analysing ~A:~A" (funtype-of fun) (name-of fun))
  (format t "*")
  (start-analyse-fun fun) ; *ak* see debugging
  (setq indent-counter (+ indent-counter 2))
  ;;  (terpri) *ak*
  )

(defun print-end-analyse (fun)
  ;;  (terpri)
  ;;  (setq indent-counter (- indent-counter 2))
  ;;  (print-space indent-counter)
  ;;  (format t "<<< Finished ~A:~A" (funtype-of fun) (name-of fun))
  (end-analyse-fun fun) ; *ak* see debugging
  ;;  (terpri) *ak*
  )

(defun lzs2mzs-fun1 (fun cl-var)
  (print-start-analyse fun)
  ;;       (ti::ti-error)
  ;; (format t " Start analyse ~A " (?identifier fun)) (terpri)
  (let* ((funlabel (make <function-label>
                         :function fun
                         :end-blocks ()))
         (sblock (make <block> :in-label funlabel
                       :body () :interface () :result
                       (dynamic *void-context*)))
         zykl-env)
    (setf (?start-block funlabel) sblock)
    (dynamic-let
     ((closure (if cl-var (cons (cons cl-var
                                      (cdr (car (dynamic closure))))
                                (cdr (dynamic closure)))
                 ())))
     (dynamic-let ((env (set-default-fun-annotation-and-get-env
                         (?params fun) fun sblock cl-var))
                   (pathes (list (list sblock)))
                   (block sblock)
                   (rec-calls ())
                   (calls ())
                   (tests ())
                   (moves ())
                   (get-slot-value ())
                   (set-slot-value ())
                   (typepathes (list (?type-descr fun))))
                  (setf (?pass fun) 2)
                  ;; (setf (?function-type fun) $normal)
                  (setq zykl-env (clear-closure-vars (dynamic env) sblock))
                  (dynamic-setq env zykl-env)

                  (l2m-a funlabel (?body fun))
                  (setf (?pass fun) 3)
                  ;;  (link-result fun)
                  (setf (?function-label fun) funlabel)
                  (setf (?calls fun) (dynamic calls))
                  ;; tai-recursion handling !
                  (let ((rcalls (dynamic rec-calls)))
                    (setf (?rec-calls fun) rcalls)
                    (if rcalls
                        (let* ((label (make <zykl-label> :out-block sblock
                                            :type-descr-s ()
                                            :env-level zykl-env))
                               (newblock (make <block> :in-label funlabel
                                               :out-label label
                                               :body () :interface ()
                                               :result (dynamic *void-context*)))
                               (var-vec (?var-vec (?var-descr fun))))
                          (setf (?in-block label) (cons newblock
                                                        (get-blocks-and-link
                                                         rcalls label)))
                          (setf (?in-label sblock) label)
                          ;; (setf (?in-label newblock) funlabel)
                          (setf (?start-block funlabel) newblock)
                          (let* ((lvvec (length var-vec))
                                 (no-zykl-vars (join-variables
                                                1 lvvec
                                                var-vec  rcalls fun label)))
                            (insert-new-parameters 1 lvvec var-vec no-zykl-vars
                                                   newblock
                                                   fun
                                                   (?type-descr fun)) ; add 27.05 *hf*
                            (linearize-par-moves (?in-block label))

                            (simpl-zycl-optimation
                             (reverse (dynamic calls)) rcalls
                             no-zykl-vars
                             newblock)))
                      ()))
                  ;; end tai-recursion handling !
                  (setf (?tests fun) (dynamic tests))
                  (setf (?moves fun) (dynamic moves))
                  (setf (?get-slot-value fun) (dynamic get-slot-value))
                  (setf (?set-slot-value fun) (dynamic set-slot-value))
                  ;; add the signature (!!! hack)
                  (if (?signature fun) ()
                    (set-signature fun (?type-descr-s fun)))
                  (print-end-analyse fun)
                  fun)))
  )

(defun clear-closure-vars (env-list block)
  (if (null? env-list) ()
    (let ((var (car (car env-list))))
      (if (and (local-static? var)
               (?closure var))
          (progn
            (add-closure-var (car (car env-list)) block)
            (clear-closure-vars (cdr env-list) block))
        (cons (car env-list)
              (clear-closure-vars (cdr env-list) block)))))
  )


(defun add-closure-var (var block)
  (let ((result (add-function-call %closure-push
                                   block 2 var
                                   (if (dynamic closure)
                                       (car (car (dynamic closure))) ())
                                   ())))
    ;; extension of the closure (description)
    (dynamic-setq closure
                  (cons (cons result var)
                        (dynamic closure))))
  )

(defun add-function-call (fun block argnum arg1 arg2 arg3)
  ;; ---- analyse of the used function first -----
  (init-side-effecs-fun fun)
  (lzs2mzs-fun fun)
  ;; ---- end analyse of the function ------------
  (let* ((rglocs (if (?fread-gloc fun)
                     (?glocs (?fread-gloc fun))
                   ()))
         (wglocs (if (?fwrite-gloc fun)
                     (?glocs (?fwrite-gloc fun))
                   ()))
         (result (make <tempvar> :tnr (a-number)))
         (var-vec (make-vector (+ argnum 1)))
         (call (make <call> :function fun
                     :read-glocs rglocs
                     :write-glocs wglocs)))
    (setf (vector-ref var-vec  0) result)
    (setf (?arg-num call) argnum)
    (setf (?var-descr call) (make <var-descr>
                                  :var-vec var-vec
                                  :constant-counter 0))
    (setf (?type-descr call) (general-var-actual-descr argnum))
    (setf (?type-descr-s call) ())
    ;; fill the var-descr
    (setf (vector-ref var-vec  1) arg1)
    (setf (vector-ref var-vec  2) arg2)
    (if (eq argnum 3)
        (setf (vector-ref var-vec 3) arg3)
      ())
    ;; fill type-descr-s
    (let ((typedescrs
           (make-actual-type-descr (dynamic typepathes)
                                   call
                                   (?var-descr call)
                                   argnum
                                   ()))
          )
      ;; add annotations to called function
      (setf (?applications fun)
            (cons call (?applications fun)))
      ;; make a type - inference
      (setq typedescrs
            (inference fun typedescrs))
      ;; add the type-descriptors
      (setf (?type-descr-s call) typedescrs)
      (setf (dynamic typepathes) typedescrs))
    ;; link variable
    (link-var-vec var-vec call argnum)
    (setf (?block call) block)
    (setf (?body block)
          (append-stat (?body block) call))
    ;; add annotation to the function
    (setf (dynamic calls)
          (cons call (dynamic calls)))
    result)
  ;;  (list var)
  )

(defun linearize-par-moves (bl-list)
  (if bl-list
      (progn (linearize-par-moves1 (car bl-list))
             (linearize-par-moves (cdr bl-list)))
    ()))

(defun linearize-par-moves1 (block)
  (let ((moves (?interface block)))
    (if (and (cons? moves) (cdr moves))
        (setf (?interface block ) (lin-par-moves moves))
      ())))

(defun lin-par-moves (moves)
  (if (one-var-first-set-and-then-used moves)
      (lin-par-moves1 (car moves) (cdr moves))
    moves))

(defun one-var-first-set-and-then-used (moves)
  (if (cdr moves)
      (if (one-var-first-set-and-then-used1 (car moves) (cdr moves))
          t
        (one-var-first-set-and-then-used (cdr moves)))
    ()))

(defun one-var-first-set-and-then-used1 (move rest)
  (let* ((vec (?var-vec (?var-descr move)))
         (result (vector-ref vec 0)))
    (if (used-var result rest) t
      (if (cdr rest) (one-var-first-set-and-then-used1 (car rest) (cdr rest))
        ()))))

(defun used-var (var moves)
  (if moves
      (if (eq (vector-ref (?var-vec (?var-descr (car moves))) 1) var) t
        (used-var var (cdr moves)))
    ()))

(defun lin-par-moves1 (move rest)
  (if rest
      (let* ((vec (?var-vec (?var-descr move)))
             (setvar (vector-ref vec 0))
             (usedvar (vector-ref vec 1))
             (td (if (?type-descr-s move)
                     (car (?type-descr-s move))
                   (?type-descr move)))
             (test (test-change-or-tempvar setvar (list usedvar) rest)))
        (if test
            (if (cons? test)
                ;; change moves
                (let ((temp (car test)))
                  (setf (car test) move)
                  (lin-par-moves1 temp rest))
              ;; insert tempvar
              (let* ((tempvar (make <tempvar>
                                    :tnr (a-number)
                                    :link ()))
                     (new-var-vec (make-vector 2))
                     (new-tdr (empty-formal-descr 1))
                     (new-move (make <move>
                                     :block (?block move)
                                     :var-descr (make <var-descr>
                                                      :var-vec new-var-vec)
                                     :type-descr new-tdr
                                     :type-descr-s ())))
                ;;  hier fehlten typdescriptoren 02.06 *hf*
                (setf (?t-descr-before new-tdr) td)
                (setf (?type-vars new-tdr) (ti-copy-subs (?type-vars td)))
                (set-descr-type new-tdr 0 (get-descr-type td 0))
                (set-descr-type new-tdr 1 (get-descr-type td 0))
                (setf (dynamic moves) (cons new-move (dynamic moves)))
                (setf (vector-ref new-var-vec 0) setvar)
                (setf (vector-ref new-var-vec 1) tempvar)
                (setf (vector-ref vec 0) tempvar)
                (setf (?link tempvar) (list (cons new-move 1)
                                            (cons move 0)))
                (subst-link (?link setvar) move new-move)
                ;; !!!! insert new move !!!
                (setq rest (insert-new-move new-move test rest))
                (cons move (lin-par-moves1 (car rest) (cdr rest)))))
          (cons move (lin-par-moves1 (car rest) (cdr rest)))))
    (list move))
  )

(defun insert-new-move (new after list)
  (if (eq after (car list))
      (cons after (cons new (cdr list)))
    (cons (car list) (insert-new-move new after (cdr list)))))

(defun test-change-or-tempvar (var usedvar-s moves)
  ;; result () - no use of /var/ after set
  ;; result (list-of /move/ ...) - change the moves and all is o.k.
  ;; result /move/ - insert a temporary variable
  (if moves
      (let* ((move (car moves))
             (var-vec (?var-vec (?var-descr move)))
             (setvar (vector-ref var-vec 0))
             (usedvar (vector-ref var-vec 1)))
        (if (eq usedvar var) ; you must do thomething
            (if (member setvar usedvar-s)
                move
              moves)
          (test-change-or-tempvar var (cons usedvar usedvar-s) (cdr moves))))
    ()))



(defun insert-new-parameters (in to var-vec nzvars block fun t-descr)
  (if (>= in to) ()
    (let ((var (vector-ref var-vec in)))
      (if (member var nzvars)
          (insert-new-parameters (+ in 1) to var-vec nzvars block fun t-descr)
        (let* ((newvar (make  <local-static>
                              :place (?place var)
                              :identifier (?identifier var)
                              ;; !!!!         :source (?source var)
                              ))
               (mvar-vec (make-vector 2))
               (td (empty-actual-descr 1)) ; [0] und [1] setzen mit type von t-descr[in]
               (move (make <move>
                           :block block
                           :var-descr (make <var-descr>
                                            :var-vec mvar-vec)
                           :type-descr td
                           :type-descr-s ())))
          (setf (?stat td) move)
          (set-descr-type td 0 (get-descr-type t-descr in))
          (set-descr-type td 1 (get-descr-type t-descr in))
          (setf (?t-descr-before td) t-descr)
          (get-previous-subs td) ; *hf* 27.05
          (setf (dynamic moves) (cons move (dynamic moves)))
          (setf (vector-ref mvar-vec 0) var)
          (subst-link (?link var) fun move)
          (setf (vector-ref mvar-vec 1) newvar)
          (setf (vector-ref var-vec in) newvar)
          (setf (?link newvar)
                (list (cons move 1)
                      (cons fun in)))
          (setf (?interface block)
                (append-stat (?interface block) move))
          (insert-new-parameters (+ in 1) to var-vec nzvars block fun
                                 t-descr)
          )))))

(defun subst-link (link fun block)
  (if link
      (if (eq (car (car link)) fun)
          (progn (setf (car (car link)) block)
                 (setf (cdr (car link)) 0))
        (subst-link (cdr link) fun block))
    ()))



(defun simpl-zycl-optimation (calls gotos nzvar block)
  ;; 1. last-call wurde vorgezogen
  ;; 2. seiteneffekte stimmen nicht (fehlen von Daten !!!)
  ;;  (if calls
  ;;    (let ((call (car calls)))
  ;;      (if (or (?write-glocs call)
  ;;              (?read-glocs call)
  ;;              (other-arg call nzvar))
  ;;        (simpl-zycl-optimation (cdr calls) gotos nzvar block)
  ;;        (let ((var (vector-ref (?var-vec (?var-descr call)) 0)))
  ;;          (setf (?body block)
  ;;                (append-stat (?body block) call))
  ;;          (delete-stat call (?block call))
  ;;          (setf (?block call) block)
  ;;          (setf (?link var ) (add-zykl-links gotos (?link var)))
  ;;          (simpl-zycl-optimation (cdr calls) gotos
  ;;                                 (cons var nzvar) block))
  ;;        ))
  ;;    ()
  ;;))
  ()) ; ausgeschaltet am 04.06 *hf*

(defun add-zykl-links (gotos links)
  (if (null? gotos) links
    (add-zykl-links (cdr gotos) (cons (cons (car gotos) 3 ;magic-number
                                            ) links))))

(defun other-arg (call nzvar)
  (let ((l (?arg-num call))
        (var-vec (?var-vec (?var-descr call))))
    (other-arg1 1 l var-vec nzvar)))

(defun other-arg1 (from to var-vec nzvar)
  (if (> from to) ()
    (let ((arg (vector-ref var-vec from)))
      (if (or (tempvar? arg)
              (var? arg))
          (if (member arg nzvar)
              (other-arg1 (+ from 1) to var-vec nzvar)
            t)
        (other-arg1 (+ from 1) to var-vec nzvar)))
    ))


(defun delete-stat (stat block)
  (setf (?body block)
        (delete-stat1 stat (?body block))))

(defun delete-stat1 (stat body)
  (if body
      (if (eq (car body) stat)
          (cdr body)
        (cons (car body)
              (delete-stat1 stat (cdr body))))
    ()
    ))


(defun join-variables (n arg-number var-vec gotos fun label)
  ;; analysed all formal parameters of the fun (variables), stored in /var-vec/
  ;; from /n/ = 1 to /arg-number/,
  ;; whether the are changed in the recursiv calls (/gotos/).
  ;; the result is a list of all no-zycl-variables
  ;; example:
  ;; (defun memb (item list)
  ;;    (if list
  ;;        (if (eq (car list) item) list
  ;;            (member item (cdr list)))
  ;;        ()))
  ;; the variable /item/ is the no-zycl-variable and the
  ;; variable /list/ is the zycl-variable.
  (if (>= n arg-number) ()
    (let* ((resvar (vector-ref var-vec n))
           (diffnr (diff-vars n resvar gotos)))
      ;; diff-vars analysed one variable
      (if (= diffnr 0)
          (cons resvar (join-variables (+ n 1)
                                       arg-number var-vec gotos
                                       fun label))
        (let* ((l (length gotos))
               (ftd (?type-descr fun))
               (ntd (empty-actual-descr (+ l 1)))
               (newtype (general-type)))
          (setf (?t-descr-before ntd) (list ftd))
          (setf (?stat ntd) label)
          (set-descr-type ntd 0 (get-descr-type ftd n))
          (set-descr-type ntd 1 newtype)
          (setf (?type-descr-s label)
                (append
                 (add-moves-and-type-descr-s
                  resvar
                  n 2 gotos (list ntd) ())
                 (?type-descr-s label)))
          (set-descr-type ftd n newtype)
          (join-variables (+ n 1) arg-number var-vec gotos fun label))))))

(defun add-moves-and-type-descr-s (var n m gotos ntd sumtd)
  ;; the type of the variable of the position n should be inserted in all
  ;; type-descr's of ntd in the position m. Each new type-path make a copy of
  ;; the type-descr's of ntd and adds this type-decr's to sumtd.
  (if (null? gotos) ntd
    (let* ((goto (car gotos))
           (jvar (vector-ref (?var-vec (?var-descr goto)) n))
           (block (?block goto))
           (tpathes (?t-path block))
           (newsum ;;(if tpathes ;;entfernt, da Indexfehler
            ;; (fill-types-of-a-variable-in-td
            ;;   jvar
            ;;   (get-arg-type jvar (car tpathes))
            ;;   (cdr tpathes) m ntd sumtd)..)
            ntd)
           (var-vec (make-vector 2))
           (move (make <move>
                       :block block
                       :var-descr (make <var-descr>
                                        :var-vec var-vec)
                       :type-descr (general-var-actual-descr 1)))
           (type-descr-s (make-move-tds tpathes jvar move)) ; *hf* 27.05
           )
      (setf (?type-descr-s move) type-descr-s)
      ;; add move
      (setf (dynamic moves) (cons move (dynamic moves)))
      (setf (?interface block)
            (cons move
                  (?interface block)))
      (setf (vector-ref var-vec 0) var)
      (setf (?link var) (cons (cons move 0) (?link var)))
      (setf (vector-ref var-vec 1) jvar)
      (if (or (local-static? jvar)
              (tempvar? jvar))
          (setf (?link jvar) (cons (cons move 1) (?link jvar)))
        ())
      (add-moves-and-type-descr-s var n (+ m 1) (cdr gotos) newsum ())
      )))

(defun fill-types-of-a-variable-in-td (var typ tpathes m ntd sumtd)
  (if tpathes
      (let* ((newntd (fill-types typ m ntd (car tpathes)))
             (newsumtd (copy-td newntd sumtd m)))
        (fill-types-of-a-variable-in-td
         var
         (get-arg-type var (car tpathes))
         (cdr tpathes)
         m newntd newsumtd))
    (append (fill-types typ m ntd (car tpathes)) sumtd)))

(defun fill-types (typ m ntd path)
  (if ntd
      (let ((curtd (car ntd)))
        (setf (vector-ref (?type-vec curtd) m) typ)
        (setf (?t-descr-before curtd) (cons path (?t-descr-before curtd)))
        (fill-types typ m (cdr ntd) path) ntd)
    ())
  ntd
  )

(defun copy-td (new sum m)
  (if new
      (let* ((curtd (car new))
             (td (copy-descr-up-to curtd m)))
        (setf (?stat td) (?stat curtd))
        (setf (?t-descr-before td) (?t-descr-before curtd))
        (copy-td (cdr new) (cons td sum) m))
    sum))

;;(defun fill-vector-from-to (anf end from to)
;;  (if (> anf end) ()
;;      (progn (setf (vector-ref to anf)
;;                   (vector-ref from anf))
;;             (fill-vector-from-to (+ anf 1) end from to))))

(defun diff-vars (n var gotos)
  (if (null? gotos) 0
    (if (eq var (vector-ref (?var-vec (?var-descr (car gotos))) n))
        (diff-vars n var (cdr gotos))
      (+ 1 (diff-vars n var (cdr gotos))))))

(defun get-blocks-and-link (rcalls label)
  (if rcalls
      (let* ((goto (car rcalls))
             (bl (?block goto)))
        (setf (?out-label bl) label)
        (setf (?label goto) label)
        (cons bl
              (get-blocks-and-link (cdr rcalls) label)))
    ()))


(defun set-default-fun-annotation-and-get-env (para fun block cl-var)
  ;; set the /type-descr-s/, /type-descr/, /var-descr/, /arg-num/,
  ;; /actual/ slot and set the /link/ slots to the variables
  (let* ((new-env (get-para2env-and-link (?var-list para)
                                         (?rest para)
                                         block fun 1 cl-var))
         (arity (length new-env))
         (var-vector (make-vector (+ arity 1)))
         (result (make <tempvar>
                       :tnr (a-number)
                       :link (list (cons fun 0)))))
    (setf (?arg-num fun) (if (?rest para) (- 0 arity) arity))
    ;; set the /var-descr/
    (fill-var-vector 1 var-vector new-env)
    (setf (vector-ref var-vector 0) result)
    (setf (?var-descr fun) (make <var-descr>
                                 :var-vec var-vector
                                 :constant-counter 0))
    ;; set the /type-descr/
    (let ((type-descr (if (?range-and-domain fun)
                          (range&domain-descr fun)
                        (%object-var-formal-descr arity))))
      (setf (?stat type-descr) fun)
      (setf (?type-descr fun) type-descr)
      (setf (?type-descr-s fun) ()))
    new-env
    ))

(defun get-para2env-and-link (var-list rest block fun count cl-var)
  (if (null? var-list)
      (cond ((and rest cl-var)
             (setf (?link rest) (list (cons fun count)))
             (setf (?link cl-var) (list (cons fun (+ count 1))))
             (list (cons rest rest) (cons cl-var cl-var)))
            (cl-var
             (setf (?link cl-var) (list (cons fun count)))
             (list (cons cl-var cl-var)))
            (rest (setf (?link rest) (list (cons fun count)))
                  (list (cons rest rest)))
            (t ()))
    (let ((var (car var-list)))
      (setf (?link var) (list (cons fun count)))
      (cons (cons var var)
            (get-para2env-and-link
             (cdr var-list) rest block fun (+ count 1) cl-var))))
  )

(defun fill-var-vector (num vect env)
  (cond ((null? env) ())
        (t (setf (vector-ref vect num) (car (car env)))
           (fill-var-vector (+ num 1) vect (cdr env)))))

;;(defun fill-type-vector (num vect type)
;;(setf (vector-ref vect num) type)
;;(if (= num 0) ()
;;    (fill-type-vector (- num 1) vect type)))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
