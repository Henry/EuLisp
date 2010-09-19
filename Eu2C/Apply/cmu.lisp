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
;;; Title: CMUCL compiler modifications to support EuLisp
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: of CMUCL and Henry G. Weller
;;;-----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;;; Rewrite the exponential formatting functions in CMUCL: d->e
;; -----------------------------------------------------------------------------
(setf (ext:package-lock (find-package "FORMAT")) nil)
(setf (ext:package-definition-lock (find-package "FORMAT")) nil)
(in-package "FORMAT")

(defun format-exponent-marker (number)
  (if (typep number *read-default-float-format*)
      #\e
    (typecase number
      (single-float #\f)
      (double-float #\e)
      (short-float #\s)
      (long-float #\l)
      #+double-double
      (double-double-float #\w))))

(setf (ext:package-lock (find-package "LISP")) nil)
(setf (ext:package-definition-lock (find-package "LISP")) nil)
(in-package "LISP")

(defun print-float-exponent (x exp stream)
  (declare (float x) (integer exp) (stream stream))
  (let ((*print-radix* nil))
    (if (typep x *read-default-float-format*)
        (unless (eql exp 0)
          (format stream "e~D" exp))
      (format stream "~C~D"
              (etypecase x
                (single-float #\f)
                (double-float #\e)
                (short-float #\s)
                (long-float #\L)
                #+double-double
                (double-double-float #\w))
              exp))))

;; -----------------------------------------------------------------------------
;;; Add support for reading symbols ending with ":" to CMUCL
;; -----------------------------------------------------------------------------
(setf (ext:package-lock (find-package "LISP")) nil)
(setf (ext:package-definition-lock (find-package "LISP")) nil)
(in-package "LISP")

(defmacro symbol-colon (stream)
  "Check if the current symbol being read contains a valid post-fix :
as opposed to the : being part of a package scoping operator."
  `(let ((char (peek-char nil ,stream nil :eof)))
     (if (and char
              (eq (char-class char *readtable*) #.delimiter))
         t
       ())
     ))

(defvar old-read-token #'read-token)

(defun read-token (stream firstchar)
  "This new function is just an fsm that recognizes numbers and symbols."
  ;; Check explicitly whether FIRSTCHAR has an entry for
  ;; NON-TERMINATING in CHARACTER-ATTRIBUTE-TABLE and
  ;; READ-DOT-NUMBER-SYMBOL in CMT. Report an error if these are
  ;; violated. (If we called this, we want something that is a
  ;; legitimate token!) Read in the longest possible string satisfying
  ;; the Backus-Naur form for "unqualified-token". Leave the result in
  ;; the *READ-BUFFER*. Return next char after token (last char read).
  (when *read-suppress*
    (internal-read-extended-token stream firstchar nil)
    (return-from read-token nil))
  (let ((package-designator nil)
        (colons 0)
        (possibly-rational t)
        (seen-digit-or-expt nil)
        (possibly-float t)
        (was-possibly-float nil)
        (escapes ())
        (seen-multiple-escapes nil))
    (reset-read-buffer)
    (prog ((char firstchar))
          (case (char-class3 char *readtable*)
            (#.constituent-sign (go SIGN))
            (#.constituent-digit (go LEFTDIGIT))
            (#.constituent-digit-or-expt
             (setq seen-digit-or-expt t)
             (go LEFTDIGIT))
            (#.constituent-decimal-digit (go LEFTDECIMALDIGIT))
            (#.constituent-dot (go FRONTDOT))
            (#.escape (go ESCAPE))
            (#.package-delimiter (go COLON))
            (#.multiple-escape (go MULT-ESCAPE))
            (#.constituent-invalid (%reader-error stream "invalid constituent"))
            ;; can't have eof, whitespace, or terminating macro as first char!
            (t (go SYMBOL)))
          SIGN ; saw "sign"
          (ouch-read-buffer char)
          (setq char (read-char stream nil nil))
          (unless char (go RETURN-SYMBOL))
          (setq possibly-rational t
                possibly-float t)
          (case (char-class3 char *readtable*)
            (#.constituent-digit (go LEFTDIGIT))
            (#.constituent-digit-or-expt
             (setq seen-digit-or-expt t)
             (go LEFTDIGIT))
            (#.constituent-decimal-digit (go LEFTDECIMALDIGIT))
            (#.constituent-dot (go SIGNDOT))
            (#.escape (go ESCAPE))
            (#.package-delimiter (go COLON))
            (#.multiple-escape (go MULT-ESCAPE))
            (#.delimiter (unread-char char stream) (go RETURN-SYMBOL))
            (t (go SYMBOL)))
          LEFTDIGIT ; saw "[sign] {digit}+"
          (ouch-read-buffer char)
          (setq char (read-char stream nil nil))
          (unless char (return (make-integer)))
          (setq was-possibly-float possibly-float)
          (case (char-class3 char *readtable*)
            (#.constituent-digit (go LEFTDIGIT))
            (#.constituent-decimal-digit (if possibly-float
                                             (go LEFTDECIMALDIGIT)
                                           (go SYMBOL)))
            (#.constituent-dot (if possibly-float
                                   (go MIDDLEDOT)
                                 (go SYMBOL)))
            (#.constituent-digit-or-expt
             (if (or seen-digit-or-expt (not was-possibly-float))
                 (progn (setq seen-digit-or-expt t) (go LEFTDIGIT))
               (progn (setq seen-digit-or-expt t) (go LEFTDIGIT-OR-EXPT))))
            (#.constituent-expt
             (if was-possibly-float
                 (go EXPONENT)
               (go SYMBOL)))
            (#.constituent-slash (if possibly-rational
                                     (go RATIO)
                                   (go SYMBOL)))
            (#.delimiter (unread-char char stream)
                         (return (make-integer)))
            (#.escape (go ESCAPE))
            (#.multiple-escape (go MULT-ESCAPE))
            (#.package-delimiter (go COLON))
            (t (go SYMBOL)))
          LEFTDIGIT-OR-EXPT
          (ouch-read-buffer char)
          (setq char (read-char stream nil nil))
          (unless char (return (make-integer)))
          (case (char-class3 char *readtable*)
            (#.constituent-digit (go LEFTDIGIT))
            (#.constituent-decimal-digit (error "impossible!"))
            (#.constituent-dot (go SYMBOL))
            (#.constituent-digit-or-expt (go LEFTDIGIT))
            (#.constituent-expt (go SYMBOL))
            (#.constituent-sign (go EXPTSIGN))
            (#.constituent-slash (if possibly-rational
                                     (go RATIO)
                                   (go SYMBOL)))
            (#.delimiter (unread-char char stream)
                         (return (make-integer)))
            (#.escape (go ESCAPE))
            (#.multiple-escape (go MULT-ESCAPE))
            (#.package-delimiter (go COLON))
            (t (go SYMBOL)))
          LEFTDECIMALDIGIT ; saw "[sign] {decimal-digit}+"
          (assert possibly-float)
          (ouch-read-buffer char)
          (setq char (read-char stream nil nil))
          (unless char (go RETURN-SYMBOL))
          (case (char-class char *readtable*)
            (#.constituent-digit (go LEFTDECIMALDIGIT))
            (#.constituent-dot (go MIDDLEDOT))
            (#.constituent-expt (go EXPONENT))
            (#.constituent-slash (assert (not possibly-rational))
                                 (go SYMBOL))
            (#.delimiter (unread-char char stream)
                         (go RETURN-SYMBOL))
            (#.escape (go ESCAPE))
            (#.multiple-escape (go MULT-ESCAPE))
            (#.package-delimiter (go COLON))
            (t (go SYMBOL)))
          MIDDLEDOT ; saw "[sign] {digit}+ dot"
          (ouch-read-buffer char)
          (setq char (read-char stream nil nil))
          (unless char (return (let ((*read-base* 10))
                                 (make-integer))))
          (case (char-class char *readtable*)
            (#.constituent-digit (go RIGHTDIGIT))
            (#.constituent-expt (go EXPONENT))
            (#.delimiter
             (unread-char char stream)
             (return (let ((*read-base* 10))
                       (make-integer))))
            (#.escape (go ESCAPE))
            (#.multiple-escape (go MULT-ESCAPE))
            (#.package-delimiter (go COLON))
            (t (go SYMBOL)))
          RIGHTDIGIT ; saw "[sign] {decimal-digit}* dot {digit}+"
          (ouch-read-buffer char)
          (setq char (read-char stream nil nil))
          (unless char (return (make-float stream)))
          (case (char-class char *readtable*)
            (#.constituent-digit (go RIGHTDIGIT))
            (#.constituent-expt (go EXPONENT))
            (#.delimiter
             (unread-char char stream)
             (return (make-float stream)))
            (#.escape (go ESCAPE))
            (#.multiple-escape (go MULT-ESCAPE))
            (#.package-delimiter (go COLON))
            (t (go SYMBOL)))
          SIGNDOT ; saw "[sign] dot"
          (ouch-read-buffer char)
          (setq char (read-char stream nil nil))
          (unless char (go RETURN-SYMBOL))
          (case (char-class char *readtable*)
            (#.constituent-digit (go RIGHTDIGIT))
            (#.delimiter (unread-char char stream) (go RETURN-SYMBOL))
            (#.escape (go ESCAPE))
            (#.multiple-escape (go MULT-ESCAPE))
            (t (go SYMBOL)))
          FRONTDOT ; saw "dot"
          (ouch-read-buffer char)
          (setq char (read-char stream nil nil))
          (unless char (%reader-error stream "dot context error"))
          (case (char-class char *readtable*)
            (#.constituent-digit (go RIGHTDIGIT))
            (#.constituent-dot (go DOTS))
            (#.delimiter  (%reader-error stream "dot context error"))
            (#.escape (go ESCAPE))
            (#.multiple-escape (go MULT-ESCAPE))
            (#.package-delimiter (go COLON))
            (t (go SYMBOL)))
          EXPONENT
          (ouch-read-buffer char)
          (setq char (read-char stream nil nil))
          (unless char (go RETURN-SYMBOL))
          (setq possibly-float t)
          (case (char-class char *readtable*)
            (#.constituent-sign (go EXPTSIGN))
            (#.constituent-digit (go EXPTDIGIT))
            (#.delimiter (unread-char char stream) (go RETURN-SYMBOL))
            (#.escape (go ESCAPE))
            (#.multiple-escape (go MULT-ESCAPE))
            (#.package-delimiter (go COLON))
            (t (go SYMBOL)))
          EXPTSIGN ; got to EXPONENT, and saw a sign character
          (ouch-read-buffer char)
          (setq char (read-char stream nil nil))
          (unless char (go RETURN-SYMBOL))
          (case (char-class char *readtable*)
            (#.constituent-digit (go EXPTDIGIT))
            (#.delimiter (unread-char char stream) (go RETURN-SYMBOL))
            (#.escape (go ESCAPE))
            (#.multiple-escape (go MULT-ESCAPE))
            (#.package-delimiter (go COLON))
            (t (go SYMBOL)))
          EXPTDIGIT ; got to EXPONENT, saw "[sign] {digit}+"
          (ouch-read-buffer char)
          (setq char (read-char stream nil nil))
          (unless char (return (make-float stream)))
          (case (char-class char *readtable*)
            (#.constituent-digit (go EXPTDIGIT))
            (#.delimiter
             (unread-char char stream)
             (return (make-float stream)))
            (#.escape (go ESCAPE))
            (#.multiple-escape (go MULT-ESCAPE))
            (#.package-delimiter (go COLON))
            (t (go SYMBOL)))
          RATIO ; saw "[sign] {digit}+ slash"
          (ouch-read-buffer char)
          (setq char (read-char stream nil nil))
          (unless char (go RETURN-SYMBOL))
          (case (char-class2 char *readtable*)
            (#.constituent-digit (go RATIODIGIT))
            (#.delimiter (unread-char char stream) (go RETURN-SYMBOL))
            (#.escape (go ESCAPE))
            (#.multiple-escape (go MULT-ESCAPE))
            (#.package-delimiter (go COLON))
            (t (go SYMBOL)))
          RATIODIGIT ; saw "[sign] {digit}+ slash {digit}+"
          (ouch-read-buffer char)
          (setq char (read-char stream nil nil))
          (unless char (return (make-ratio stream)))
          (case (char-class2 char *readtable*)
            (#.constituent-digit (go RATIODIGIT))
            (#.delimiter
             (unread-char char stream)
             (return (make-ratio stream)))
            (#.escape (go ESCAPE))
            (#.multiple-escape (go MULT-ESCAPE))
            (#.package-delimiter (go COLON))
            (t (go SYMBOL)))
          DOTS ; saw "dot {dot}+"
          (ouch-read-buffer char)
          (setq char (read-char stream nil nil))
          (unless char (%reader-error stream "too many dots"))
          (case (char-class char *readtable*)
            (#.constituent-dot (go DOTS))
            (#.delimiter
             (unread-char char stream)
             (%reader-error stream "too many dots"))
            (#.escape (go ESCAPE))
            (#.multiple-escape (go MULT-ESCAPE))
            (#.package-delimiter (go COLON))
            (t (go SYMBOL)))
          SYMBOL ; not a dot, dots, or number
          (let ((stream (in-synonym-of stream)))
            (stream-dispatch
             stream
             ;; simple-stream
             (prog ()
                   SYMBOL-LOOP
                   (ouch-read-buffer char)
                   (setq char (stream::%read-char stream nil nil t t))
                   (unless char (go RETURN-SYMBOL))
                   (case (char-class char *readtable*)
                     (#.escape (go ESCAPE))
                     (#.delimiter (stream::%unread-char stream char)
                                  (go RETURN-SYMBOL))
                     (#.multiple-escape (go MULT-ESCAPE))
                     ;; ***HGW Modification to support symbols with : post-fix
                     (#.package-delimiter (if (symbol-colon stream)
                                              (go SYMBOL-LOOP)
                                            (go COLON)))
                     (t (go SYMBOL-LOOP))))
             ;; lisp stream
              (prog ()
                    SYMBOL-LOOP
                    (ouch-read-buffer char)
                    (setq char (read-char stream nil :eof))
                    (unless char (go RETURN-SYMBOL))
                    (case (char-class char *readtable*)
                      (#.escape (go ESCAPE))
                      (#.delimiter (unread-char char stream)
                                   (go RETURN-SYMBOL))
                      (#.multiple-escape (go MULT-ESCAPE))
                      ;; ***HGW Modification to support symbols with : post-fix
                      (#.package-delimiter (if (symbol-colon stream)
                                               (go SYMBOL-LOOP)
                                             (go COLON)))
                      (t (go SYMBOL-LOOP))))
             ;; fundamental-stream
             (prog ()
                   SYMBOL-LOOP
                   (ouch-read-buffer char)
                   (setq char (read-char stream nil :eof))
                   (when (eq char :eof) (go RETURN-SYMBOL))
                   (case (char-class char *readtable*)
                     (#.escape (go ESCAPE))
                     (#.delimiter (unread-char char stream)
                                  (go RETURN-SYMBOL))
                     (#.multiple-escape (go MULT-ESCAPE))
                     ;; ***HGW Modification to support symbols with : post-fix
                     (#.package-delimiter (if (symbol-colon stream)
                                              (go SYMBOL-LOOP)
                                            (go COLON)))
                     (t (go SYMBOL-LOOP))))))
          ESCAPE ; saw an escape
          ;; Don't put the escape in the read buffer.
          ;; READ-NEXT CHAR, put in buffer (no case conversion).
          (let ((nextchar (read-char stream nil nil)))
            (unless nextchar
              (reader-eof-error stream "after escape character"))
            (push *ouch-ptr* escapes)
            (ouch-read-buffer nextchar))
          (setq char (read-char stream nil nil))
          (unless char (go RETURN-SYMBOL))
          (case (char-class char *readtable*)
            (#.delimiter (unread-char char stream) (go RETURN-SYMBOL))
            (#.escape (go ESCAPE))
            (#.multiple-escape (go MULT-ESCAPE))
            (#.package-delimiter (go COLON))
            (t (go SYMBOL)))
          MULT-ESCAPE
          (setq seen-multiple-escapes t)
          (do ((char (read-char stream t) (read-char stream t)))
              ((multiple-escape-p char))
            (if (escapep char) (setq char (read-char stream t)))
            (push *ouch-ptr* escapes)
            (ouch-read-buffer char))
          (setq char (read-char stream nil nil))
          (unless char (go RETURN-SYMBOL))
          (case (char-class char *readtable*)
            (#.delimiter (unread-char char stream) (go RETURN-SYMBOL))
            (#.escape (go ESCAPE))
            (#.multiple-escape (go MULT-ESCAPE))
            (#.package-delimiter (go COLON))
            (t (go SYMBOL)))
          COLON
          (casify-read-buffer escapes)
          (unless (zerop colons)
            (%reader-error stream "too many colons in ~S"
                           (read-buffer-to-string)))
          (setq colons 1)
          (setq package-designator
                (if (plusp *ouch-ptr*)
                    ;; FIXME: It seems inefficient to cons up a package
                    ;; designator string every time we read a symbol with an
                    ;; explicit package prefix. Perhaps we could implement
                    ;; a FIND-PACKAGE* function analogous to INTERN*
                    ;; and friends?
                    (read-buffer-to-string)
                  (if seen-multiple-escapes

                      (read-buffer-to-string)
                    *keyword-package*)))
          (reset-read-buffer)
          (setq escapes ())
          (setq char (read-char stream nil nil))
          (unless char (reader-eof-error stream "after reading a colon"))
          (case (char-class char *readtable*)
            (#.delimiter
             (unread-char char stream)
             (%reader-error stream
                           "illegal terminating character after a colon: ~S"
                           char))
            (#.escape (go ESCAPE))
            (#.multiple-escape (go MULT-ESCAPE))
            (#.package-delimiter (go INTERN))
            (t (go SYMBOL)))
          INTERN
          (setq colons 2)
          (setq char (read-char stream nil nil))
          (unless char
            (reader-eof-error stream "after reading a colon"))
          (case (char-class char *readtable*)
            (#.delimiter
             (unread-char char stream)
             (%reader-error stream
                           "illegal terminating character after a colon: ~S"
                           char))
            (#.escape (go ESCAPE))
            (#.multiple-escape (go MULT-ESCAPE))
            (#.package-delimiter
             (%reader-error stream
                            "too many colons after ~S name"
                            package-designator))
            (t (go SYMBOL)))
          RETURN-SYMBOL
          (casify-read-buffer escapes)
          (let ((found (if package-designator
                           (find-package package-designator)
                         *package*)))
            (unless found
              (error 'reader-package-error :stream stream
                     :format-arguments (list package-designator)
                     :format-control "package ~S not found"))

            (if (or (zerop colons) (= colons 2) (eq found *keyword-package*))
                (return (intern* *read-buffer* *ouch-ptr* found))
              (multiple-value-bind (symbol test)
                  (find-symbol* *read-buffer* *ouch-ptr* found)
                (when (eq test :external) (return symbol))
                (let ((name (read-buffer-to-string)))
                  (with-simple-restart
                   (continue "Use symbol anyway.")
                   (error 'reader-package-error :stream stream
                          :format-arguments (list name (package-name found))
                          :format-control
                          (if test
                              "The symbol ~S is not external in the ~A package."
                            "Symbol ~S not found in the ~A package.")))
                  (return (intern name found)))))))))

(progn
  (set-cmt-entry #\\ #'read-token)
  (set-cmt-entry #\: #'read-token)
  (set-cmt-entry #\| #'read-token)
  ;;all constituents
  (do ((ichar 0 (1+ ichar))
       (len #+unicode-bootstrap #o200
            #-unicode-bootstrap char-code-limit))
      ((= ichar len))
    (let ((char (code-char ichar)))
      ;; Update the `read-token' function for all constituents for which the
      ;; `read-token' function is set
      (if (eq (get-cmt-entry char *readtable*) old-read-token)
          (set-cmt-entry char #'read-token))))
  )

;; -----------------------------------------------------------------------------
;;; Add support for reading C character digrams to CMUCL
;;  \a \b \d \f \l \n \r \t \v
;; -----------------------------------------------------------------------------
(macrolet
    ((frob (char-names-list)
           (collect
            ((results))
            (dolist (code char-names-list)
              (destructuring-bind (ccode names)
                  code
                (dolist (name names)
                  (results (cons name (code-char ccode))))))
            `(defparameter char-name-alist ',(results)
               "This is the alist of (character-name . character) for characters with
  long names.  The first name in this list for a given character is used
  on typeout and is the preferred form for input."))))
  ;; Note: the char-name listed here should be what string-capitalize
  ;; would produce.  This is needed to match what format ~:C would
  ;; produce.
  (frob ((#x00 ("Null" "^@" "NUL"))
         (#x01 ("^A" "SOH"))
         (#x02 ("^B" "STX"))
         (#x03 ("^C" "ETX"))
         (#x04 ("^D" "EOT"))
         (#x05 ("^E" "ENQ"))
         (#x06 ("^F" "ACK"))
         (#x07 ("Bell" "^g" "BEL" "\\a"))
         (#x08 ("Backspace" "^h" "BS" "\\b"))
         (#x09 ("Tab" "^i" "HT" "\\t"))
         (#x0A ("Newline" "Linefeed" "^j" "LF" "NL" "NLL" "\\n" "\\l"))
         (#x0B ("Vt" "^k" "\\v"))
         (#x0C ("Page" "^l" "Form" "Formfeed" "FF" "NP" "\\f"))
         (#x0D ("Return" "^m" "RET" "CR" "\\r"))
         (#x0E ("^N" "SO"))
         (#x0F ("^O" "SI"))
         (#x10 ("^P" "DLE"))
         (#x11 ("^Q" "DC1"))
         (#x12 ("^R" "DC2"))
         (#x13 ("^S" "DC3"))
         (#x14 ("^T" "DC4"))
         (#x15 ("^U" "NAK"))
         (#x16 ("^V" "SYN"))
         (#x17 ("^W" "ETB"))
         (#x18 ("^X" "CAN"))
         (#x19 ("^Y" "EM" "EOM"))
         (#x1A ("^Z" "SUB"))
         (#x1B ("Escape" "^[" "Altmode" "ESC" "Alt"))
         (#x1C ("Is4" "FS" "^\\"))
         (#x1D ("Is3" "GS" "^]"))
         (#x1E ("Is2" "RS" "^^"))
         (#x1F ("Is1" "US" "^_"))
         (#x20 ("Space" "SP" "SPC"))
         (#x7f ("Rubout" "Delete" "DEL" "\\d")))))

;; -----------------------------------------------------------------------------
;;; Return to the user-package
;; -----------------------------------------------------------------------------
(in-package #:cl-user)

;; -----------------------------------------------------------------------------
;;; End of cmu.lisp
;; -----------------------------------------------------------------------------
