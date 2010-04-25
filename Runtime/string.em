;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;; Description: string processing
;;;-----------------------------------------------------------------------------
(defmodule string
  (syntax (_telos0)
   import (telos convert copy collect compare fpi)
   export (<character-sequence>
           <string> stringp string-data string-size string-ref
           substring tailstring string-compare string-equal string-append
           string-as-int string-empty-p member1-string
           do1-string map1-string listify-string
           eul_list_as_eul_string))

;;;-----------------------------------------------------------------------------
;;; Class <string>
;;;-----------------------------------------------------------------------------
  (defclass <character-sequence> (<sequence>) () abstractp: t)

  (defprimclass <string> string-class (<character-sequence>)
    ((data accessor: string-data)))

  (defmethod initialize ((str <string>) inits)
    (call-next-method)
    (let ((n (init-list-ref inits size: 0))
          (c (init-list-ref inits fill-value: #\ )))
      (eul_init_string str n c)))

  (defextern eul_init_string (ptr <int> <character>) ptr)

;;;-----------------------------------------------------------------------------
;;; String access
;;;-----------------------------------------------------------------------------
  (defun substring (str i j)
    (let ((ii (or i 0))
          (jj (or j (string-size str))))
      (substring1 str ii jj)))

  (defmethod element ((str <string>) (i <int>))
    (string-ref str i))

  (defmethod size ((str <string>))
    (string-size str))

;;;-----------------------------------------------------------------------------
;;; Concatenation
;;;-----------------------------------------------------------------------------
  (defmethod concatenate ((str <string>) . cs)
    (labels
     ((loop (ccs)
            (if (null ccs) str
              (progn
                (setq str
                      (eul_str_append str (convert (car ccs) <string>)))
                (loop (cdr ccs))))))
     (loop cs)))

  (defun string-append strs
    (eul_list_as_eul_string strs))

  (defextern eul_str_append (<string> <string>) <string>)
  (defextern eul_list_as_eul_string (ptr) ptr)

;;;-----------------------------------------------------------------------------
;;; Predicates
;;;-----------------------------------------------------------------------------
  (defun string-empty-p (str) (int-binary= (string-size str) 0))
  (declare-inline string-empty-p)

  (defmethod emptyp ((str <string>)) (string-empty-p str))

  (defmethod equal ((str1 <string>) (str2 <string>))
    (string-equal str1 str2))

  (defun string-equal (str1 str2)
    (int-binary= (string-compare str1 str2) 0))
  (declare-inline string-equal)

  (defmethod binary< ((str1 <string>) (str2 <string>))
    (int-binary< (string-compare str1 str2) 0))

  (defextern string-compare (<string> <string>) <int> "strcmp")

;;;-----------------------------------------------------------------------------
;;; Mapping
;;;-----------------------------------------------------------------------------
  (defmethod do ((fun <function>) (str <string>) . cs)
    (if (null cs)
        (do1-string fun str)
      (call-next-method)))

  (defun do1-string (fun str)
    (let ((n (string-size str))
          (i 0))
      (labels
       ((loop ()
              (if (int-binary< i n)
                  (progn
                    (fun (string-ref str i))
                    (setq i (int-binary+ i 1))
                    (loop))
                ())))
       (loop))))

  (defmethod map ((fun <function>) (str <string>) . cs)
    (if (null cs)
        (map1-string fun str)
      (call-next-method)))

  (defun map1-string (fun str)
    (let* ((n (string-size str))
           (res (make <string> size: n))
           (i 0))
      (labels
       ((loop ()
              (if (int-binary< i n)
                  (progn
                    ((setter string-ref) res i (fun (string-ref str i)))
                    (setq i (int-binary+ i 1))
                    (loop))
                res)))
       (loop))))

;;;-----------------------------------------------------------------------------
;;; Member
;;;-----------------------------------------------------------------------------
  (defmethod member (x (str <string>) . preds)
    (if (null preds)
        (member1-string x str)
      (let ((pred (car preds))
            (n (string-size str))
            (i 0))
        (labels
         ((loop ()
                (and (int-binary< i n)
                     (if (eql x (string-ref str i))
                         i
                       (progn
                         (setq i (int-binary+ i 1))
                         (loop))))))
         (loop)))))

;;;-----------------------------------------------------------------------------
;;; Anyp
;;;-----------------------------------------------------------------------------
  (defmethod anyp ((fun <function>) (str <string>) . cs)
    (if (null cs)
        (anyp1-string fun str)
      (call-next-method)))

  (defun anyp1-string (fun str)
    (let ((n (string-size str))
          (i 0))
      (labels
       ((loop ()
              (and (int-binary< i n)
                   (or (fun (string-ref str i))
                       (progn
                         (setq i (int-binary+ i 1))
                         (loop))))))
       (loop))))

;;;-----------------------------------------------------------------------------
;;; Allp
;;;-----------------------------------------------------------------------------
  (defmethod allp ((fun <function>) (str <string>) . cs)
    (if (null cs)
        (allp1-string fun str)
      (call-next-method)))

  (defun allp1-string (fun str)
    (let ((n (string-size str))
          (i 0))
      (labels
       ((loop ()
              (if (int-binary< i n)
                  (and (fun (string-ref str i))
                       (progn
                         (setq i (int-binary+ i 1))
                         (loop)))
                str)))
       (loop))))

;;;-----------------------------------------------------------------------------
;;; Reverse string
;;;-----------------------------------------------------------------------------
  (defmethod reverse ((str <string>))
    (reverse-string str))
  (defextern reverse-string (<string>) <string> "eul_reverse_str")

;;;-----------------------------------------------------------------------------
;;; Accumulate
;;;-----------------------------------------------------------------------------
  (defmethod accumulate ((fun <function>) init (str <string>))
    (accumulate-string fun init str))

  (defun accumulate-string (fun init str)
    (let ((n (string-size str))
          (i 0))
      (labels
       ((loop ()
              (if (int-binary< i n)
                  (progn
                    (setq init (fun init (string-ref str i)))
                    (setq i (int-binary+ i 1))
                    (loop))
                init)))
       (loop))))

  (defmethod accumulate1 ((fun <function>) (str <string>))
    (accumulate1-string fun str))

  (defun accumulate1-string (fun str)
    (if (int-binary= (string-size str) 0) ()
      (accumulate-string fun (string-ref str 0) (tailstring str 1))))

;;;-----------------------------------------------------------------------------
;;; Copy
;;;-----------------------------------------------------------------------------
  (defmethod shallow-copy ((str <string>))
    (copy-string str))

  (defmethod deep-copy ((str <string>))
    (copy-string str))

  (defextern copy-string (<string>) <string> "eul_str_copy")

;;;-----------------------------------------------------------------------------
;;; Conversion
;;;-----------------------------------------------------------------------------
  (defgeneric (converter <string>) (x))

  (defextern string-as-int (<string>) <int> "atoi")

  (defun listify-string (str . separators)
    (let ((c (if separators (car separators) #\ )))
      (labels
       ((loop (s res)
              (let* ((i (member c s))
                     (x (make <symbol> name: (substring s () i))))
                (if (and i (int-binary< i (int-binary- (string-size s) 1)))
                    (loop (substring s (int-binary+ i 1) ()) (cons x res))
                  (reverse-list (cons x res))))))
       (if (eql c (string-ref str 0))
           (loop (substring str 1 ()) ())
         (loop str ())))))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
