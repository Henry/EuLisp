;; Merge sort.
;; Copyright Juha Heinanen 1988
;; This code may be freely distributed.
;; Modified to run under euscheme Tracy Gardner 1995
(defmodule merge
    (syntax (macros macros-tag)
     import (level1 basic)
     export (merge-sort))

  (print "### merge")

  (defun merge! (list1 list2 previous less?)
     ; Merges list1 and list2 into cdr of previous
    (if (null? list1)
        ((setter cdr) previous list2)
      (if (null? list2)
          ((setter cdr) previous list1)
        (if (less? (car list2) (car list1))
            (progn
              ((setter cdr) previous list2)
              (merge! list1 (cdr list2) list2 less?))
          (progn
            ((setter cdr) previous list1)
            (merge! (cdr list1) list2 list1 less?))))))

  (defun merge-pass! (list-of-lists previous less?)
    ; Two way merges lists in list-of-lists into cdr of previous
    ; merge-pass!
    (when (not (null? list-of-lists))
       (if (null? (cdr list-of-lists))
           ((setter cdr) previous list-of-lists)
         (let ((sorted-list (list '*header-node*)))
           (merge! (car list-of-lists) (cadr list-of-lists) sorted-list less?)
           ((setter cdr) previous (list (cdr sorted-list)))
           (merge-pass! (cddr list-of-lists) (cdr previous) less?)))))

  (defun merge-sort (value-list less?)
  ; Sorts value-list according to less?
    ; merge-sort!
    (let ((merged-lists (list '*header-node* '())))
      (let loop ((list-of-lists (map list value-list)))
           (merge-pass! list-of-lists merged-lists less?)
           (if (null? (cddr merged-lists))
               (cadr merged-lists)
             (loop (cdr merged-lists))))))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
