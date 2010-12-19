;;; Copyright 2010 Stefan Israelsson Tampe and Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;
;;  Youtoo is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: smatch programmable pattern matcher
;;;  Library: smatch
;;;  Authors: Alex Shinn, Stefan Israelsson Tampe and Henry G. Weller
;;;  Maintainers: Stefan Israelsson Tampe and Henry G. Weller
;;;  Description:
;;;    See README.org or README.html
;;;-----------------------------------------------------------------------------
;; This code is based on smatch.em -- portable hygienic pattern matcher
;; written by Alex Shinn, extended by Stefan Israelsson Tampe and converted to
;; EuLisp by Stefan Israelsson Tampe.
;;
;; This is a full superset of the popular MATCH package by Andrew
;; Wright by written in itself and hence requires bootstrapping.
;;
;; This is a simple generative pattern matcher - each pattern is
;; expanded into the required tests, calling a failure continuation if
;; the tests fail.  This makes the logic easy to follow and extend,
;; but produces sub-optimal code in cases where you have many similar
;; clauses due to repeating the same tests.  Nonetheless a smart
;; compiler should be able to remove the redundant tests.  For
;; MATCH-LET and DESTRUCTURING-BIND type uses there is no performance
;; hit.
;;
;; The original version was written on 2006/11/29 and described in the
;; following Usenet post:
;;   http://groups.google.com/group/comp.lang.scheme/msg/0941234de7112ffd
;; and is still available at
;;   http://synthcode.com/scheme/match-simple.scm
;; It's just 80 lines for the core MATCH, and an extra 40 lines for
;; MATCH-LET, MATCH-LAMBDA and other syntactic sugar.
;;
;; A variant of this file which uses COND-EXPAND in a few places for
;; performance can be found at
;;   http://synthcode.com/scheme/match-cond-expand.scm
;;
;; 2009/11/25 - adding `***' tree search patterns
;; 2008/03/20 - fixing bug where (a ...) matched non-lists
;; 2008/03/15 - removing redundant check in vector patterns
;; 2008/03/06 - you can use `...' portably now (thanks to Taylor Campbell)
;; 2007/09/04 - fixing quasiquote patterns
;; 2007/07/21 - allowing ellipse patterns in non-final list positions
;; 2007/04/10 - fixing potential hygiene issue in match-check-ellipse
;;              (thanks to Taylor Campbell)
;; 2007/04/08 - clean up, commenting
;; 2006/12/24 - bugfixes
;; 2006/12/01 - non-linear patterns, shared variables in OR, get/set
;;
;; 2010/11/10
;; Various improvements Implemented by Stefan Israelsson Tampe among others
;; Translated the code using the EuLisp defsyntax.
;;
;; phd e.g. customable API for list processing
;; <z>  matcher system
;; $    matcher added with some more features on the = construct
;; cond , like or but the first successful match is only used
;; ,    inside non quasiquote mode will insert outer defined variable
;;      e.g.  (let ((A 1)) (smatch 2 (,A t))) will produce t
;;;-----------------------------------------------------------------------------

(defmodule smatch
  (syntax (syntax-0)
   import (level-0)
   export (smatch))


(defsyntax smatch X
  (smatch0 X
    (()     (error <condition> "missing match expression"))
    ((atom) (error <condition> "no match clauses"))
    (('-abs abs '-phd p . l)
     `(match* (,abs ,p) ,@l))
    (('-phd p '-abs abs . l)
     `(match* (,abs ,p) ,@l))
    (('-abs abs . l)
     `(match* (,abs ((car cdr cons? null? binary=) ())) ,@l))
    (('-phd p . l)
     `(match* (() ,p) ,@l))
    ((l ...)
     `(match* (() ((car cdr cons? null? binary=) ())) ,@l))))


(defsyntax match* X
  (smatch0 X
    ((abs (app ...) (and a (pat . body)) ...)
     (let ((v (gensym "v")))
       `(let ((,v ,app))
          (match-next ,abs ,v (,app (setq ,app)) ,@a))))

    ((abs (and a #(vec ...)) (and b (pat . body)) ...)
     (let ((v (gensym "v")))
       `(let ((,v ,a))
          (match-next ,abs ,v (,v (setq ,v) ) ,@b))))

    ((abs atom b ...)
     (let ((v (gensym "v")))
       `(let ((,v ,atom))
          (match-next ,abs ,v (,atom (setq ,atom)) ,@b))))))


(defsyntax match-next X
  (smatch0 X
    ;; no more clauses, the match failed
    ((abs v g+s)
     `(error <condition> "no matching pattern"))

    ;; named failure continuation
    ((abs v g+s (pat ('=> failure) . body) . rest)
     `(let ((,failure (lambda () (match-next ,abs ,v ,g+s ,@rest))))
        ;; match-one analyzes the pattern for us
        (match-one ,abs ,v ,pat ,g+s
                   (match-drop-ids (progn ,@body))
                   (match-drop-ids (,failure)) ())))

    ;; anonymous failure continuation, give it a dummy name
    ((abs v g+s (pat . body) . rest)
     (let ((failure (gensym "fail")))
       `(match-next ,abs ,v ,g+s (,pat (=> ,failure) ,@body) ,@rest)))))


(defsyntax abs-drop x
  (smatch0 x
    ((a k        ) k)
    ((a (k ...) v) (append k `(,v)))))


(defsyntax match-one x
  ;; If it's a list of two or more values, check to see if the
  ;; second one is an ellipse and handle accordingly, otherwise go
  ;; to MATCH-TWO.
  ;(print `(match-one ,x) nl)
  (smatch0 x
    ((abs v (p ('.. m) . r) g+s sk fk i)
     `(match-extract-vars
       ,abs ,p (abs-drop (match-gen-ellipses-n
                          ,abs ,m ,v ,p ,r  ,g+s ,sk ,fk ,i)) ,i ()))
    ((abs v (p ('__ m) . r) g+s sk fk i)
     `(match-extract-vars
       ,abs ,p (abs-drop (match-gen-ellipses-n
                          ,abs ,m ,v ,p ,r  ,g+s ,sk ,fk ,i)) ,i ()))

    ((abs v (p q . r) g+s sk fk i)
     `(match-check-ellipse
       ,q
       (match-extract-vars
        ,abs ,p (abs-drop (match-gen-ellipses
                           ,abs ,v ,p ,r  ,g+s ,sk ,fk ,i)) ,i ())
       (match-two ,abs ,v (,p ,q ,@r) ,g+s ,sk ,fk ,i)))
    ;; Go directly to MATCH-TWO.
    ( x
      `(match-two ,@x))))


(defsyntax insert-abs x
  (smatch0 x
    ((abs ('progn . l)) `('progn ,@l))
    ((abs (x))          `(,x))
    ((abs (n nn ...))  (append `(,n ,abs) nn))))


(defsyntax recur x
  (smatch0 x
    ((n ((a i) ...) code ...)
     `(letfuns ((,n ,a ,@code)) (,n ,@i)))))


(defsyntax match-two x
  (smatch0 x
    (((and a (qabs ((qcar qcdr qcons? qnull? qbinary=) pp)))
      v
      () g+s (sk ...) fk i)
     `(if (,qnull? ,v)
          (insert-abs ,a (,@sk ,i))
        (insert-abs ,a ,fk)))

    (((and a (qabs ((qcar qcdr qcons? qnull? qbinary=) pp)))
      v
      ('quote p) g+s (sk ...) fk i)
     `(if (,qbinary= ,v (quote ,p))
          (insert-abs ,a (,@sk ,i))
        (insert-abs ,a ,fk)))

    (( (and a (qabs ((qcar qcdr qcons? qnull? qbinary=) pp)))
       v
       ('unquote p)  g+s (sk ...) fk i)
     `(if (,qbinary= ,v ,p)
          (insert-abs ,a (,@sk ,i))
        (insert-abs ,a ,fk)))

    (((and a (abs ((ccar ccdr pcons? qnull? qbinary=) rr)))
      v
      (('unquote-splicing p) . ps)  g+s sk fk i)
     (let ((loop  (gensym "loop"))
           (vv    (gensym "v"))
           (pp    (gensym "p")))
       `(recur ,loop ((,vv ,v)
                      (,pp ,p))
               (if (cons? ,pp)
                   (if (and (,pcons? ,vv) (,qbinary= (,ccar ,vv) (car ,pp)))
                       (,loop (,ccdr ,vv) (cdr ,pp))
                     (insert-abs ,a ,fk))
                 (match-one ,a ,vv ,ps ,g+s ,sk ,fk ,i)))))

    ((abs v ('quasiquote p) . x)
     `(match-quasiquote ,abs ,v ,p ,@x))

    ((abs v ('and) g+s (sk ...) fk i)
     `(insert-abs ,abs (,@sk ,i)))

    ((abs v ('and p q ...) g+s sk fk i)
     `(match-one ,abs ,v ,p ,g+s (match-one ,v (and ,@q) ,g+s ,sk ,fk) ,fk ,i))

    ((abs v ('or) g+s sk fk i) `(insert-abs ,abs ,fk))
    ((abs v ('or p) . x)
     `(match-one ,abs ,v ,p ,@x))

    ((abs v ('or p ...) g+s sk fk i)
     `(match-extract-vars
       ,abs (or ,@p) (abs-drop (match-gen-or
                                ,abs ,v ,p ,g+s ,sk ,fk ,i)) ,i ()))

    ((abs v ('cond) g+s sk fk i) `(insert-abs ,abs ,fk))
    ((abs v ('cond p) . x)
     `(match-one ,abs ,v ,p ,@x))
    ((abs v ('cond p ps ...) g+s sk fk i)
     `(match-one
       ,abs ,v ,p ,g+s ,sk (abs-drop (match-one
                                      ,abs ,v (cond ,@ps) ,g+s ,sk ,fk ,i)) ,i))

    ((abs v ('not p) g+s (sk ...) (fk fkk ...) i)
     `(match-one ,abs ,v ,p ,g+s (match-drop-ids (,fk ,abs ,@fkk)) (,@sk i) i))
    ((abs v ('get getter) (g s) (sk ...) fk i)
     `(let ((,getter (lambda () ,g))) (insert-abs ,abs (,@sk ,i))))
    ((abs v ('set setter) (g (s ...)) (sk ...) fk i)
     (let ((x      (gensym "x")))
       `(let ((,setter (lambda (,x) (,@s ,x)))) (insert-abs ,abs (,@sk ,i)))))
    ((abs v ('? pred . p) g+s sk fk i)
     `(if (,pred ,v) (match-one
                      ,abs ,v (and ,@p) ,g+s ,sk ,fk ,i) (insert-abs ,abs ,fk)))

    ;; stis, added $ support!
    ((abs v ('$$ n) g-s sk fk i)
     `(if (,(concatenate n '?) ,v)
          (insert-abs ,abs ,sk)
        (insert-abs ,abs ,fk)))

    ((abs v ('$$ nn p ...) g+s sk fk i)
     (if (symbol? nn)
         (progn
           `(if (,(concatenate nn '?) ,v)
                (match-$$ ,abs (and) ,nn ,p ,v ,sk ,fk ,i)
              (insert-abs ,abs ,fk)))
       (error <condition> "only symbols in $")))

    ((abs v ('$ nn p ...) g+s sk fk i)
     `(match-$ ,abs (and) ,nn ,p ,v ,sk ,fk ,i))

    ;; stis, added the possibility to use set and get on classes
    ((abs v ('= 0 m p) g+s sk fk i)
     (let ((w  (gensym "w")))
       `(let ((,w  (,m ,v)))
          (match-one
           ,abs ,w ,p ((,m ,v) ((setter ,m) ,v)) ,sk ,fk ,i))))

    ((abs v ('= g s p) g+s sk fk i)
     (let ((w  (gensym "w")))
       `(let ((,w (,g ,v))) (match-one
                             ,abs ,w ,p ((,g ,v) (,s ,v)) ,sk ,fk ,i))))

    ((abs v ('= proc p) g+s . x)
     (let ((w  (gensym "w")))
       `(let ((,w (,proc ,v))) (match-one ,abs ,w ,p () ,@x))))

    ((abs v (('<> f p) . l) g+s sk fk i)
     (let ((res (gensym "res")))
       `(let ((,res (,f ,v)))
          (if ,res
              (match-one ,abs (car ,res) ,p ,g+s
                         (match-one (cdr ,res) ,l ,g+s ,sk ,fk)
                         ,fk ,i)
            (insert-abs ,abs ,fk)))))

    ((abs v (p '___ . r) g+s sk fk i)
     `(match-extract-vars
       ,abs ,p (abs-drop (match-gen-ellipses
                          ,abs ,v ,p ,r ,g+s ,sk ,fk ,i) ,i ())))
    (((abs phd) v p       g+s sk fk i)
     `(match-abstract () ,abs ,phd ,v ,p ,g+s ,sk ,fk ,i))))


(defsyntax match-gen-or x
  (smatch0 x
    ((abs v p g+s (sk ...) fk (i ...) ((id id-ls) ...))
     (let ((sk2 (gensym "sk2")))
       `(let ((,sk2 (lambda ,id  (insert-abs ,abs (,@sk  (,@i ,@id))))))
          (match-gen-or-step
           ,abs ,v ,p ,g+s (match-drop-ids (,sk2 ,@id)) ,fk i))))))


(defsyntax match-gen-or-step x
  (smatch0 x
    ((abs v () g+s sk fk . x)
     ;; no OR clauses, call the failure continuation
     `(insert-abs ,abs ,fk))
    ((abs v (p) . x)
     ;; last (or only) OR clause, just expand normally
     `(match-one ,abs ,v ,p ,@x))
    ((abs v (p . q) g+s sk fk i)
     ;; match one and try the remaining on failure
     `(match-one
       ,abs ,v ,p ,g+s ,sk (match-gen-or-step ,v ,q ,g+s ,sk ,fk ,i) ,i))))


(defsyntax match-three x
  (smatch0 x
    (((and a (abs ((qcar qcdr qcons? qnull?) rr))) v (p) g+s sk fk i)
     (let ((w (gensym "w")))
       `(if (and (,qcons? ,v) (,qnull? (,qcdr ,v)))
            (let ((,w (,qcar ,v)))
              (match-one ,a ,w ,p ((,qcar ,v) ((setter car) ,v)) ,sk ,fk ,i))
          ,fk)))

    ((abs v (p '*** q) g+s sk fk i)
     `(match-extract-vars
       ,abs ,p (match-gen-search ,v ,p ,q ,g+s ,sk ,fk ,i) ,i ()))

    ((abs v (p '*** . q) g+s sk fk i)
     `(error <condition> (fmt "invalid use of *** ~a" (,p *** ,@q))))

    (((and a (abs ((qcar qcdr qcons? qnull? qbinary=) pp)))
      v
      (p . q) g+s sk fk i)
     (let ((w (gensym "w"))
           (x (gensym "x")))
       `(if (,qcons? ,v)
            (let ((,w (,qcar ,v)) (,x (,qcdr ,v)))
              (match-one ,a ,w ,p ((,qcar ,v) ((setter car) ,v))
                         (match-one ,x ,q ((,qcdr ,v) ((setter cdr) ,v)) ,sk ,fk)
                         ,fk
                         ,i))
          (insert-abs ,a ,fk))))

    ((abs v #(p ...) g+s . x)
     `(match-vector ,abs ,v 0 () ,p ,@x))

    ((abs v '_ g+s (sk ...) fk i) `(insert-abs ,abs (,@sk ,i)))

    ;; Not a pair or vector or special literal, test to see if it's a
    ;; new symbol, in which case we just bind it, or if it's an
    ;; already bound symbol or some other literal, in which case we
    ;; compare it with BINARY=.
    (((and a
           (qabs ((qcar qcdr qcons? qnull? binary=) qpp)))
      v
      x
      g+s (sk ...) fk (id ...))

     (if (and (symbol? x) (not (member x id)))
         `(let ((,x ,v))
            (insert-abs ,a (,@sk (,@id ,x))))
       `(if (,binary= ,v ,x)
            (insert-abs ,a (,@sk ,id))
          (insert-abs ,a ,fk))))))

;;;-----------------------------------------------------------------------------
;;; Vector patterns
;;    are just more of the same, with the slight exception that we pass around
;;    the current vector index being matched.
;;;-----------------------------------------------------------------------------

(defsyntax match-vector x
  ;(print `(match-vector ,x) nl)
  (smatch0 x
    ((abs v n pats (p q) . x)
     `(match-check-ellipse
       ,q
       (match-gen-vector-ellipses ,abs ,v ,n ,pats ,p ,@x)
       (match-vector-two ,abs ,v ,n ,pats (,p ,q) ,@x)))
    ((abs v n pats (p '___) sk fk i)
     `(match-gen-vector-ellipses ,abs ,v ,n ,pats ,p ,sk ,fk ,i))
    (x
     `(match-vector-two ,@x))))


(defsyntax match-vector-two x
  ;(print `(match-vector-tow ,x) nl)
  (smatch0 x
    ((abs v n (and a ((pat index) ...)) () sk fk i)
     (let ((len (gensym "len")))
       `(if (vector? ,v)
            (let ((,len (vector-size ,v)))
              (if (= ,len ,n)
                  (match-vector-step ,abs ,v ,a ,sk ,fk ,i)
                (insert-abs ,abs ,fk)))
          (insert-abs ,abs ,fk))))
    ((abs v n (pats ...) (p . q) . x)
     `(match-vector ,abs ,v (+ ,n 1) (,@pats (,p ,n)) ,q ,@x))))


(defsyntax match-vector-step x
  ;(print `(match-vector-step ,x) nl)
  (smatch0 x
    ((abs v () (sk ...) fk i) `(insert-abs ,abs (,@sk ,i)))
    ((abs v ((pat index) . rest) sk fk i)
     (let ((w (gensym "w")))
       `(let ((,w (vector-ref ,v ,index)))
          (match-one ,abs ,w ,pat ((vector-ref ,v ,index)
                                   ((setter vector-ref) ,v ,index))
                     (match-vector-step ,v ,rest ,sk ,fk)
                     ,fk ,i))))))


;; With a vector ellipse pattern we first check to see if the vector
;; size is at least the required size.

(defsyntax match-gen-vector-ellipses x
  ;(print `(match-vector-ellipses ,x) nl)
  (smatch0 x
    ((abs v n (and a ((pat index) ...)) p sk fk i)
     (let ((len (gensym "len")))
       `(if (vector? ,v)
            (let ((,len (vector-size ,v)))
              (if (>= ,len ,n)
                  (match-vector-step ,abs ,v ,a
                                     (match-vector-tail ,v ,p ,n ,len ,sk ,fk)
                                     ,fk ,i)
                (insert-abs ,abs ,fk)))
          (insert-abs ,abs ,fk))))))


(defsyntax match-vector-tail x
  ;(print `(match-vector-tail ,x) nl)
  (smatch0 x
    ((abs v p n len sk fk i)
     `(match-extract-vars
       ,abs ,p (match-vector-tail-two ,v ,p ,n ,len ,sk ,fk ,i) ,i ()))))


(defsyntax match-vector-tail-two x
  ;(print `(match-vector-tail-two ,x) nl)
  (smatch0 x
    ((abs v p n len (sk ...) fk i ((id id-ls) ...))
     (let ((loop  (gensym "loop"))
           (j     (gensym "j"))
           (w     (gensym "w")))
       `(recur ,loop ((,j ,n) ,@(map (lambda (id-ls) `(,id-ls '())) id-ls))
               (if (>= ,j ,len)
                   (let ,(map (lambda (id id-ls)
                                `(,id (reverse ,id-ls))) id id-ls)
                     (insert-abs ,abs (,@sk ,i)))
                 (let ((,w (vector-ref ,v ,j)))
                   (match-one ,abs ,w ,p ((vector-ref ,v ,j)
                                          ((setter vector-ref) ,v ,j))
                              (match-drop-ids
                               (,loop (+ ,j 1)
                                      ,@(map (lambda (id id-ls)
                                               `(cons ,id ,id-ls))
                                             id id-ls)))
                              ,fk ,i))))))))


(defsyntax match-abstract x
  (smatch0 x
    ((x () phd          y p               . l)
     `(match-phd () ,phd ,x ,y ,p ,@l))

    (((x ...) ((a) us ...) phd y ((b bs ...) . ps) g+s sk fk i)
     (if (eq a b)
         (let ((ret (gensym "ret")))
           `(let ((,ret ((,a ,@bs) ,y)))
              (if ,ret
                  (match-one
                   (((,a) ,@us ,@x) ,phd) (cdr ,ret) ,ps ,g+s ,sk ,fk ,i)
                (insert-abs (((,a) ,@us ,@x) ,phd) ,fk))))
       `(match-abstract
         ((,a) ,@x) ,us ,phd ,y ((,b ,@bs) ,@ps) ,g+s ,sk ,fk ,i)))

    (((x ...) ((a aa as ...) us ...) phd y ((b  bs ...) . ps) g+s sk fk i)
     (if (eq a b)
         (let ((ret (gensym "ret")))
           `(let ((,ret ((,a ,@bs) ,y)))
              (if ,ret
                  (let ((,aa (car ,ret)))
                    (match-one
                     (((,a ,@as) ,@us ,@x) ,phd)
                     (cdr ,ret) ,ps ,g+s ,sk ,fk (,aa ,@i)))
                (insert-abs (((,a ,@as) ,@us ,@x) ,phd) ,fk))))
       `(match-abstract
         ((,a ,aa ,@as) ,@x) ,us ,phd ,y ((,b ,@bs) ,@ps) ,g+s ,sk ,fk ,i)))

    (((x ...) ((a) us ...) phd y (b . ps) g+s sk fk i)
     (if (eq a b)
         (let ((ret (gensym "ret")))
           `(let ((,ret (,a ,y)))
              (if ,ret
                  (match-one
                   (((,a) ,@us ,@x) ,phd) (cdr ,ret) ,ps ,g+s ,sk ,fk ,i)
                (insert-abs (((,a) ,@us ,@x) ,phd) ,fk))))
       `(match-abstract ((,a) ,@x) ,us ,phd ,y (,b ,@ps) ,g+s ,sk ,fk ,i)))

    (((x ...) ((a aa as ...) us ...) phd y (b . ps) g+s sk fk i)
     (if (eq a b)
         (let ((ret (gensym "ret")))
           `(let ((,ret (,a ,y)))
              (if ,ret
                  (let ((,aa  (car ,ret)))
                    (match-one
                     (((,a ,@as) ,@us ,@x) ,phd)
                     (cdr ,ret) ,ps ,g+s ,sk ,fk (,aa ,@i)))
                (insert-abs (((,a ,@as) ,@us ,@x) ,phd) ,fk))))
       `(match-abstract
         ((,a ,aa ,@as) ,@x) ,us ,phd ,y (,b ,@ps) ,g+s ,sk ,fk ,i)))

    ((() abs phd y p g+s sk fk i)
     `(match-phd () ,phd ,abs ,y ,p ,g+s ,sk ,fk ,i))))


(defsyntax match-phd x
  (smatch0 x
    ((phd (c (            )) abs . l) `(match-three (,abs (,c ,phd)) ,@l))
    (((phd ...) (c ((h a) hh ...)) abs v (h2 . l) g+s sk fk i)
     (if (eq h h2)
         `(match-one
           (,abs (,a ((,h ,a) ,@hh ,@phd)))
           ,v ,l ,g+s (set-phd-sk ,c ,sk) (set-phd-fk ,c ,fk) ,i)
       `(match-phd ((,h ,a) ,@phd) (,c ,hh) ,abs ,v (,h2 ,@l) ,g+s ,sk ,fk ,i)))
    ((() phd abs . l)
     `(match-three (,abs ,phd) ,@l))))


(defsyntax set-phd-fk x
  (smatch0 x
    ((abs          cc ('progn . l))  `(progn ,@l))
    ((abs          cc (fk))          `(,fk))
    (((abs (c pp)) cc (fk fkk ...))  `(fk (,abs (,cc ,pp)) ,@fkk))))


(defsyntax set-phd-sk x
  (smatch0 x
    ((abs          cc ('progn . l)  i ...)  `(progn ,@l))
    ((abs          cc (fk)          i ...)  `(,fk))
    (((abs (c pp)) cc (fk fkk ...)  i ...)  `(,fk (,abs (,cc ,pp)) ,@fkk ,@i))))


(defsyntax match-$$ x
  (smatch0 x
    ((abs (a ...) n (p1 p2 ...) . v)
     (if (symbol? p1)
         (let ((acc (concatenate n '- p1)))
           `(match-$$ ,abs (,@a (= ,acc (setter ,acc) ,p1)) ,n ,p2 ,@v))
       (error <condition> "$$ matchers should be constituated of symbols")))
    ((abs newpat  m ()            v kt ke i)
     `(match-one ,abs ,v ,newpat () ,kt ,ke ,i))))

;; (defsyntax match-$ x
;;   (smatch0 x
;;     ((abs (a ...) n (p1 p2 ...) . v)
;;      `(match-$ ,abs (,@a (= ,(car n) ((setter ,(car n)) ,p1)))
;;                ,(cdr n) ,p2 ,@v))
;;     ((abs newpat  m ()            v kt ke i)
;;      `(match-one ,abs ,v ,newpat () ,kt ,ke ,i))))

(defsyntax match-$ x
  (smatch0 x
    ((abs (a ...) n (p1 p2 ...)  . v)
     (let ((wcar (gensym "wcar"))
           (wcdr (gensym "wcdr")))
       `(if (cons? ,n)
            (let ((,wcar (car ,n))
                  (,wcdr (cdr ,n)))
              (match-$ ,abs (,@a (= (car ,wcar) (cdr ,wcar) ,p1))
                        ,wcdr ,p2 ,@v))
          (error "$ matcher has too many patterns"))))

    ((abs newpat  m ()            v kt ke i)
     `(match-one ,abs ,v ,newpat () ,kt ,ke ,i))))


;; ... algorithms is implemented here, note this x ... y ... is not allowed and
;; phd is not implemented for this need to extend phd API with list? ...
(defsyntax match-gen-ellipses x
  (smatch0 x
    ((abs v p () g+s (sk ...) fk i ((id id-ls) ...))
     (let ((ls (gensym "ls"))
           (w (gensym "w")))
       `(match-check-identifier
         ,p
         ;; simplest case equivalent to (p ...), just bind the list
         (let ((,p ,v))
           (if (list? ,p)
               (insert-abs ,abs (,@sk ,i))
             (insert-abs ,abs ,fk)))
         ;; simple case, match all elements of the list
         (recur loop ((,ls ,v) ,@(map (lambda (id-ls) `(,id-ls '())) id-ls))
                (cond
                  ((null? ,ls)
                   (let ,(map (lambda (id id-ls)
                                `(,id (reverse ,id-ls))) id id-ls)
                     (insert-abs ,abs (,@sk ,i))))
                  ((cons? ,ls)
                   (let ((,w (car ,ls)))
                     (match-one ,abs ,w ,p ((car ,ls) ((setter car) ,ls))
                                (match-drop-ids
                                 (loop (cdr ,ls)
                                       ,@(map (lambda (id id-ls)
                                                `(cons ,id ,id-ls))
                                              id id-ls)))
                                ,fk ,i)))
                  (else
                   (insert-abs ,abs ,fk)))))))

    ((abs v p r g+s (sk ...) fk i ((id id-ls) ...))
     ;; general case, trailing patterns to match, keep track of the
     ;; remaining list size so we don't need any backtracking
     (let ((tail-len (gensym "tail-len"))
           (len      (gensym "len"     ))
           (n        (gensym "n"       ))
           (ls       (gensym "ls"      ))
           (w        (gensym "w"       )))

       `(match-verify-no-ellipses
         ,r
         (let* ((,tail-len (size (quote ,r)))
                (,ls       ,v)
                (,len      (size ,ls)))
           (if (< ,len ,tail-len)
               (insert-abs ,abs ,fk)
             (recur loop ((,ls ,ls) (,n ,len)
                          ,@(map (lambda (id-ls)
                                   `(,id-ls '())) id-ls))
                    (cond
                      ((= ,n ,tail-len)
                       (let ,(map (lambda (id id-ls)
                                    `(,id (reverse ,id-ls))) id id-ls)
                         (match-one ,abs ,ls ,r (() ()) ,sk  ,fk ,i)))
                      ((cons? ,ls)
                       (let ((,w (car ,ls)))
                         (match-one ,abs ,w ,p ((car ,ls) ((setter car) ,ls))
                                    (match-drop-ids
                                     (loop (cdr ,ls) (- ,n 1)
                                           ,@(map (lambda (id id-ls)
                                                    `(cons ,id ,id-ls))
                                                  id id-ls)))
                                    ,fk
                                    ,i)))
                      (else (insert-abs ,abs ,fk)))))))))))

(defsyntax match-gen-ellipses-n x
  (smatch0 x
    ((abs n v p () g+s (sk ...) fk i ((id id-ls) ...))
     (let ((ls (gensym "ls"))
           (in (gensym "in"))
           (w (gensym "w")))
       `(match-check-identifier
         ,p
         ;; simplest case equivalent to (p ...), just bind the list
         (let ((,p ,v))
           (if (list? ,p)
               (insert-abs ,abs (,@sk ,i))
             (insert-abs ,abs ,fk)))
         ;; simple case, match all elements of the list
         (recur loop ((,ls ,v) (,in ,n) ,@(map (lambda (id-ls) `(,id-ls '())) id-ls))
                (cond
                  ((null? ,ls)
                   (if (> ,in 0)
                       (insert-abs ,abs ,fk)
                     (let ,(map (lambda (id id-ls)
                                  `(,id (reverse ,id-ls))) id id-ls)
                       (insert-abs ,abs (,@sk ,i)))))
                  ((cons? ,ls)
                   (let ((,w (car ,ls)))
                     (match-one ,abs ,w ,p ((car ,ls) ((setter car) ,ls))
                                (match-drop-ids
                                 (loop (cdr ,ls) (- ,in 1)
                                       ,@(map (lambda (id id-ls)
                                                `(cons ,id ,id-ls))
                                              id id-ls)))
                                ,fk ,i)))
                  (else
                   (insert-abs ,abs ,fk)))))))

    ((abs m v p r g+s (sk ...) fk i ((id id-ls) ...))
     ;; general case, trailing patterns to match, keep track of the
     ;; remaining list size so we don't need any backtracking
     (let ((tail-len (gensym "tail-len"))
           (len      (gensym "len"     ))
           (n        (gensym "n"       ))
           (ls       (gensym "ls"      ))
           (im       (gensym "im"      ))
           (w        (gensym "w"       )))

       `(match-verify-no-ellipses
         ,r
         (let* ((,tail-len (size (quote ,r)))
                (,ls       ,v)
                (,len      (size ,ls)))
           (if (< ,len ,tail-len)
               (insert-abs ,abs ,fk)
             (recur loop ((,ls ,ls) (,n ,len) (,im ,m)
                          ,@(map (lambda (id-ls)
                                   `(,id-ls '())) id-ls))
                    (cond
                      ((= ,n ,tail-len)
                       (if (> ,im 0)
                           (insert-abs ,abs ,fk)
                         (let ,(map (lambda (id id-ls)
                                      `(,id (reverse ,id-ls))) id id-ls)
                           (match-one ,abs ,ls ,r (() ()) ,sk  ,fk ,i))))
                      ((cons? ,ls)
                       (let ((,w (car ,ls)))
                         (match-one ,abs ,w ,p ((car ,ls) ((setter car) ,ls))
                                    (match-drop-ids
                                     (loop (cdr ,ls) (- ,n 1) (- ,im 1)
                                           ,@(map (lambda (id id-ls)
                                                    `(cons ,id ,id-ls))
                                                  id id-ls)))
                                    ,fk
                                    ,i)))
                      (else (insert-abs ,abs ,fk)))))))))))


(defsyntax match-verify-no-ellipses x
  (smatch0 x
    (((x . y) sk)
     `(match-check-ellipse
       ,x
       (error <condition> "multiple ellipse patterns not allowed at same level")
       (match-verify-no-ellipses ,y ,sk)))
    ((() sk)
     sk)
    ((x sk)
     `(error <condition> (fmt "dotted tail not allowed after ellipse ~a" ,x)))))


(defsyntax match-drop-ids x
  (smatch0 x
    ((expr            ) expr)
    ((abs expr ids ...) expr)))


;;;-----------------------------------------------------------------------------
;;; Tree matching
;;;-----------------------------------------------------------------------------
(defsyntax match-gen-search x
  (smatch0 x
    ((abs v p q g+s sk fk i ((id id-ls) ...))
     (let ((try  (gensym "try"))
           (next (gensym "next"))
           (ls   (gensym "ls"))
           (w    (gensym "w"))
           (loop (gensym "loop"))
           (u    (gensym "u"))
           (fail (gensym "fail")))

       (let ((ret `(letfuns ((,try (,w ,fail ,@id-ls)
                                   (match-one ,abs ,w ,q ,g+s
                                              (match-drop-ids
                                    (let ,(map (lambda (id id-ls)
                                                 `(,id (reverse ,id-ls)))
                                               id id-ls)
                                      ,sk))
                                   (match-drop-ids
                                    (,next ,w ,fail ,@id-ls)) ,i))
                  (,next (,w ,fail ,@id-ls)
                         (if (not (cons? ,w))
                             (,fail)
                           (let ((,u (car ,w)))
                             (match-one
                              ,abs ,u ,p ((car ,w) ((setter car) ,w))
                              (match-drop-ids
                               ;; accumulate the head variables from
                               ;; the p pattern, and loop over the tail
                               (let ,(map (lambda (id id-ls)
                                            `(,id-ls (cons ,id ,id-ls)))
                                            id id-ls)
                                 (recur ,loop ((,ls (cdr ,w)))
                                        (if (cons? ,ls)
                                            (,try (car ,ls)
                                                  (lambda () (,loop
                                                              (cdr ,ls)))
                                                  ,@id-ls)
                                          (,fail)))))
                              (match-drop-ids (,fail)) ,i)))))
          ;; the initial id-ls binding here is a dummy to get the right
          ;; number of '()s
          (let ,(map (lambda (id-ls) `(,id-ls '())) id-ls)
            (,try ,v (lambda () (insert-abs ,abs ,fk)) ,@id-ls)))))
                  #;(print ret) ret)))))


(defsyntax match-quasiquote x
  (smatch0 x
    ((abs v ('unquote p) g+s sk fk i)
     `(match-one ,abs ,v ,p ,g+s ,sk ,fk ,i))
    (((and a (abs ((qcar qcdr qcons? qnull? qbinary=) pp)))
      v
      (('unquote-splicing p) . rest) g+s sk fk i)
     (let ((tmp (gensym "tmp")))
       `(if (,qcons? ,v)
            (match-one ,a ,v
                       (,p ,@tmp)
                       (match-quasiquote ,tmp ,rest ,g+s ,sk ,fk)
                       ,fk
                       ,i)
          (insert-abs ,a ,fk))))

    ((abs v ('quasiquote p) g+s sk fk i . depth)
     `(match-quasiquote ,abs ,v ,p ,g+s ,sk ,fk ,i ,() ,@depth))
    ((abs v ('unquote p) g+s sk fk i x . depth)
     `(match-quasiquote ,abs ,v ,p ,g+s ,sk ,fk ,i ,@depth))
    ((abs v ('unquote-splicing p) g+s sk fk i x . depth)
     `(match-quasiquote ,abs ,v ,p ,g+s ,sk ,fk ,i ,@depth))

    (((and a (abs ((qcar qcdr qcons? qnull? qbinary=) pp)))
      v
      (p . q) g+s sk fk i . depth)
     (let ((w (gensym "w")))
       `(if (,qcons? ,v)
            (let ((,w (,qcar ,v)) (,x (,qcdr ,v)))
              (match-quasiquote
               ,a ,w ,p ,g+s
               (match-quasiquote-step ,x ,q ,g+s ,sk ,fk ,depth)
               ,fk ,i ,@depth))
          (insert-abs ,a ,fk))))
    ((abs v #(elt ...) g+s sk fk i . depth)
     (let ((ls (gensym "ls")))
       `(if (vector? ,v)
            (let ((,ls (convert ,v <list>)))
              (match-quasiquote ,abs ,ls ,elt ,g+s ,sk ,fk ,i ,@depth))
          (insert-abs ,abs ,fk))))
    ((abs v x g+s sk fk i . depth)
     `(match-one ,abs ,v (quote ,x) ,g+s ,sk ,fk ,i))))


(defsyntax match-quasiquote-step x
  (smatch0 x
    ((abs x q g+s sk fk depth i)
     `(match-quasiquote ,abs ,x ,q ,g+s ,sk ,fk ,i ,@depth))))


(defsyntax match-extract-vars x
  (smatch0 x
    ((abs ('? pred . p) . x)
     `(match-extract-vars ,abs ,p ,@x))
    ((abs ('$ rec . p) . x)
     `(match-extract-vars ,abs ,p ,@x))
    ((abs ('= proc p) . x)
     `(match-extract-vars ,abs ,p ,@x))
    ((abs ('= u m p) . x)
     `(match-extract-vars ,abs ,p ,@x))
    ((abs ('quote x) (k kk ...) i v)
     `(,k ,abs ,@kk ,v))
    ((abs ('unquote x) (k kk ...) i v)
     `(,k ,abs ,@kk ,v))
    ((abs ('unquote-splicing x) (k kk ...) i v)
     `(,k ,abs ,@kk ,v))
    ((abs ('quasiquote x) k i v)
     `(match-extract-quasiquote-vars ,abs ,x ,k ,i ,v (t)))
    ((abs ('and . p) . x)
     `(match-extract-vars ,abs ,p ,@x))
    ((abs ('or . p) . x)
     `(match-extract-vars ,abs ,p ,@x))
    ((abs ('not . p) . x)
     `(match-extract-vars ,abs ,p ,@x))

    ;; A non-keyword pair, expand the CAR with a continuation to
    ;; expand the CDR.
    ((abs ('<> f p) k i v)
     `(match-extract-vars ,abs ,p ,k ,i ,v))
    (((abs phd) p k i v)
     `(abs-extract-vars () ,abs ,phd ,p ,k ,i ,v))))


(defsyntax match-extract-vars2 x
  (smatch0 x
    ((abs (p q . r) k i v)
     `(match-check-ellipse
       ,q
       (match-extract-vars ,abs (,p ,@r) ,k ,i ,v)
       (match-extract-vars
        ,abs ,p (match-extract-vars-step (,q ,@r) ,k ,i ,v) ,i ())))
    ((abs (p . q) k i v)
     `(match-extract-vars
       ,abs ,p (match-extract-vars-step ,q ,k ,i ,v) ,i ()))
    ((abs #(p ...) . x)
     `(match-extract-vars ,abs ,p ,@x))
    ((abs '_   (k kk ...) i v)  `(k ,abs ,@kk ,v))
    ((abs '___ (k kk ...) i v)  `(k ,abs ,@kk ,v))
    ((abs ('.. m) (k kk ...) i v)  `(k ,abs ,@kk ,v))
    ((abs ('__ m) (k kk ...) i v)  `(k ,abs ,@kk ,v))
    ((abs '*** (k kk ...) i v)  `(k ,abs ,@kk ,v))

    ;; This is the main part, the only place where we might add a new var if
    ;; it's an unbound symbol.
    ((abs p (k kk ...) (i ...) v)
     (let ((p-ls (gensym "p-ls")))
       (if (or (member p i) (not (symbol? p)))
           `(,k ,abs ,@kk ,v)
         `(,k ,abs ,@kk ((,p ,p-ls) ,@v)))))))


(defsyntax abs-extract-vars x
  (smatch0 x
    ((abs () phd p . l) `(match-extract-phd () ,phd ,abs ,p ,@l))
    (((abs ...) ((a x . xs) us ...) phd ((b bs ...) w ...) k i v)
     (if (eq a b)
         (let ((x-ls (gensym "x-ls")))
           `(match-extract-vars
             (((,a ,@xs) ,@us ,@abs) ,phd) ,w ,k ,i ((,x ,x-ls) ,@v)))
       `(abs-extract-vars
         ((,a ,x ,@xs) ,@abs) ,us ,phd ((,b ,@bs) ,@w) ,k ,i ,v)))

    (((abs ...) ((a) us ...) phd ((b bs ...) w ...) k i v)
     (if (eq a b)
         `(match-extract-vars
           (((,a) ,@us ,@abs) ,phd) ,w ,k ,i ,v)
       `(abs-extract-vars
         ((,a) ,@abs) ,us ,phd ((,b ,@bs) ,@w) ,k ,i ,v)))

    (((abs ...) ((a x . xs) us ...) phd (b w ...) k i v)
     (if (eq a b)
         (let ((x-ls (gensym "x-ls")))
           `(match-extract-vars
             (((,a ,@xs) ,@us ,@abs) ,phd) ,w ,k ,i ((,x ,x-ls) ,@v)))
       `(abs-extract-vars
         ((,a ,x ,@xs) ,@abs) ,us ,phd (,b ,@w) ,k ,i ,v)))

    (((abs ...) ((a) us ...) phd (b w ...) k i v)
     (if (eq a b)
         `(match-extract-vars
           (((,a) ,@us ,@abs) ,phd) ,w ,k ,i ,v)
       `(abs-extract-vars
         ((,a) ,@abs) ,us ,phd (,b ,@w) ,k ,i ,v)))
    ((() a phd p k i v)
     `(match-extract-phd () ,phd ,a ,p ,k ,i ,v))))


(defsyntax match-extract-phd x
  (smatch0 x
    ((_ phd abs . l)
     `(match-extract-vars2 (,abs ,phd) ,@l))))


(defsyntax match-extract-vars-step x
  (smatch0 x
    ((abs p k i v (and a ((v2 v2-ls) ...)))
     `(match-extract-vars ,abs ,p ,k (,@v2 ,@i) (,@a ,@v)))))


(defsyntax match-extract-quasiquote-vars x
  (smatch0 x
    ((abs ('quasiquote x) k i v d)
     `(match-extract-quasiquote-vars ,abs ,x ,k ,i ,v (t ,@d)))
    ((abs ((or 'unquote 'unquote-splicing) x) k i v d)
     `(match-extract-quasiquote-vars ,abs ('uunquote ,x) ,k ,i ,v ,d))
    ((abs ('uunquote x) k i v (t))
     `(match-extract-vars ,abs ,x ,k ,i ,v))
    ((abs ('uunquote x) k i v (t . d))
     `(match-extract-quasiquote-vars ,abs ,x ,k ,i ,v ,d))
    ((abs (x . y) k i v (t . d))
     `(match-extract-quasiquote-vars
       ,abs
       ,x
       (match-extract-quasiquote-vars-step ,y ,k ,i ,v ,d) ,i ()))
    ((abs #(x ...) k i v (t . d))
     `(match-extract-quasiquote-vars ,abs ,x ,k ,i ,v ,d))
    ((abs x (k kk ...) i v (t . d))
     `(,k ,abs ,@kk ,v))))


(defsyntax match-extract-quasiquote-vars-step x
  (smatch0 x
    ((_ abs x k i v d (and a ((v2 v2-ls) ...)))
     `(match-extract-quasiquote-vars ,abs ,x ,k (,@v2 ,@i) (,@a ,@v) ,d))))


(defsyntax match-check-ellipse x
  (smatch0 x
    ;; these two aren't necessary but provide fast-case failures
    (((a . b)  success-k failure-k) failure-k)
    ((#(a ...) success-k failure-k) failure-k)
    ;; matching an atom
    ((id success-k failure-k)
     (if (or (binary= id '...)  (binary= id '___)) success-k failure-k))))


(defsyntax match-check-identifier x
  (smatch0 x
    ;; fast-case failures, lists and vectors are not identifiers
    (((x . y) success-k failure-k) failure-k)
    ((#(x ...) success-k failure-k) failure-k)
    ;; x is an atom
    ((x success-k failure-k)
     (if (symbol? x) success-k failure-k))))


;;;-----------------------------------------------------------------------------
;;; Utility functions
;;;-----------------------------------------------------------------------------
(defsyntax defmatchfun args
  (let ((name (car args))
        (arg (gensym "arg"))
        (matchers (cdr args)))
    (if (symbol? name)
        `(deflocal ,name
           (named-lambda ,name (,arg) (smatch ,arg ,@matchers)))
      (if (eq (car name) 'setter)
          `((setter setter) ,(cadr name)
            (named-lambda ,name (,arg) (smatch ,arg ,@matchers)))
        (error <condition> "bad defmatchfun syntax")))))

(defsyntax match-lambda matchers
  (let ((x (gensym "arg")))
    `(lambda (,x) (smatch ,x ,@matchers))))

(defsyntax match-lambda* matchers
  (let ((x (gensym "arg")))
    `(lambda ,x (smatch ,x ,@matchers))))

(defsyntax match-let x
  (smatch0 x
    (((vars ...) . body)
     `(match-let/helper let () () ,vars ,@body))
    ((loop . rest)
     `(match-named-let loop () ,@rest))))

(defsyntax match-letfuns x
  (smatch0 x
    ((vars . body) `(match-let/helper letfuns () () ,vars ,@body))))

(defsyntax match-let/helper x
  (smatch0 x
    ((let (and a ((var expr) ...)) () () . body)
     `(,let ,a ,@body))
    ((let (and a ((var expr) ...)) (and b ((pat tmp) ...)) () . body)
     `(,let ,a
        (match-let* ,b ,@body)))
    ((let (v ...) (p ...) (((a . b) expr) . rest) . body)
     (let ((tmp (gensym "tmp")))
       `(match-let/helper
         let (,@v (,tmp ,expr)) (,@p ((,a ,@b) ,tmp)) ,rest ,@body)))
    ((let (v ...) (p ...) ((#(a ...) expr) . rest) . body)
     (let ((tmp (gensym "tmp")))
       `(match-let/helper
         let (,@v (,tmp ,expr)) (,@p (#(,@a) ,tmp)) ,rest ,@body)))
    ((let (v ...) (p ...) ((a expr) . rest) . body)
     `(match-let/helper ,let (,@v (,a ,expr)) ,p ,rest ,@body))))

(defsyntax match-named-let x
  (smatch0 x
    ((loop ((pat expr var) ...) () . body)
     `(recur loop ,(map (lambda (var expr) `(,var ,expr)) var expr)
             (match-let ,(map (lambda (pat var) `(,pat ,var)) pat var)
               ,@body)))
    ((_ loop (v ...) ((pat expr) . rest) . body)
     (let ((tmp (gensym "tmp")))
       `(match-named-let ,loop (,@v (,pat ,expr ,tmp)) ,rest ,@body)))))

(defsyntax match-let* x
  (smatch0 x
    ((() . body)
     `(progn ,@body))
    ((((pat expr) . rest) . body)
     `(smatch ,expr (,pat (match-let* ,rest ,@body))))))


;;;-----------------------------------------------------------------------------
)  ;; End of module smatch
;;;-----------------------------------------------------------------------------
