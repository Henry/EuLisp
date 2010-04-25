(defmodule extras
  (syntax (macros)
   import (level1)
   export (end-of-stream-p))

  ;; It's a lot simple to do (until (end-of-stream-p s) (read s))
  ;; than to use exceptions or the eof-object argument to catch end of file.
  (defgeneric end-of-stream-p (stream))

  (defmethod end-of-stream-p ((s <stream>))
    (let ((scb (stream-source s)))
      (and (null (control-block-buffer scb))
           (int-binary= (fill-buffer s) 0))))

  (defmethod end-of-stream-p ((ss <string-stream>))
    (let ((scb (stream-source ss)))
      (and (int-binary= (control-block-buffer-cnt scb)
                        (string-size (control-block-buffer scb)))
           (int-binary= (fill-buffer ss) 0)
           t)))

  (defmethod end-of-stream-p ((fs <file-stream>))
    (let ((fcb (stream-source fs)))
      (and (int-binary= (control-block-buffer-cnt fcb) 0)
           (int-binary= (fill-buffer fs) 0)
           t)))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;; Testing
;;;-----------------------------------------------------------------------------

(deflocal ss (make <string-stream> string: "this"))
(deflocal scb (stream-source ss))
(control-block-buffer-cnt scb)
(control-block-buffer-pos scb)
(string-size (control-block-buffer scb))
(end-of-stream-p ss)
(deflocal fs (make <file-stream> file-name: "/dev/null" mode: 'r))
(with-output-file (f "/tmp/end-of-stream-test.dat")
                  (print "this" f))
(deflocal fs (make <file-stream> file-name: "/tmp/end-of-stream-test.dat"))
(let loop ((i 0)) (cond ((< i 5) (read-char fs) (loop (+ i 1)))))
(end-of-stream-p fs)

;;;-----------------------------------------------------------------------------
