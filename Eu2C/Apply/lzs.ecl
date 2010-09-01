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
;;;  Title:
;;;  Description:
;;    Annotations
;;    ===========
;;    exported: * this is a mixin-annotation for global objects
;;    * its value is used as a flag in most cases
;;    * for code generation it holds a pair
;;    (<lisp identifier for export> . <exporting module>)
;;    * the value is initialized with () and set by mark-as-exported
;;    for objects exported by the compilation unit
;;;  Requires:
;;;  Notes:
;;;  Problems:
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;; deflzs stuetzt sich nicht auf defstruct sondern auf defclass, weil
;; - in defclass fuer Slots eine Typspezifikation angegeben werden kann,
;;   ohne dass eine Initform angegeben werden muss,
;; - die Accessor-Funktionen generisch sind, und somit fuer verschiedene
;;   Klassen gleiche Namen haben koennen.
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;; Deflzs was not based on defstruct but on defclass, because
;; - defclass supports type specification for slots,
;;   without which an Initform must be specified,
;; - The accessor functions are generic, and thus for different classes they
;;   can have identical names.
;;;-----------------------------------------------------------------------------

#module lzs

(import (eulisp0
         apply-standard  ; only make-eulisp-class-id
         lzs-syntax      ; make-structure-and-annotation-slots
         ;; make-predicate-name
         ;; <lzs-object>
         ;; lzs-object?
         accessors)
 syntax (eulisp0
         apply-standard) ; only the macro defstandardclass
 syntax (lzs-syntax)    ; only the macro def-lzs-object
 export (def-lzs-object ;macro from lzs-syntax
          make-structure-and-annotation-slots
          make-predicate-name)
 export (imported? named? global?) ;mixin predicates
 export (<lzs-object> lzs-object?) ; from lzs-syntax
 export (?unexpanded ?symtab-initfun) ;should be exported automatically
 expose (accessors))

;;;-----------------------------------------------------------------------------
;; Definition der Strukturen, die die Knoten der Zwischensprache
;; repraesentieren.
;; Zuerst werden die Slots genannt, die den Strukturteil des Knotens beschreiben
;; und dann, gefolgt von :Annotations der Annotationsteil.
;;;-----------------------------------------------------------------------------

(def-lzs-object lzs-object+type (lzs-object)
  :Annotations
  ;;-----------
  (type :initform ()))

;;;-----------------------------------------------------------------------------
(def-lzs-object module (:named lzs-object)
  (fun-list :initform ())
  (class-def-list :initform ())
  (named-const-list :initform ())
  (var-list :initform ())
  (sym-list :initform ())
  (toplevel-forms :initform ())
  :Annotations
  ;;-----------
  (lex-env :initform ())
  (syntax-env :initform ())
  (exports :initform ())
  (syntax-exports :initform ())
  (used-runtime-modules :initform ())
  (used-syntax-modules :initform ())
  (c-imports :initform ())
  (symtab-initfun :initform ()) ; the function which puts all statically created
  ;; symbols into the symbol table
  )


;;;-----------------------------------------------------------------------------
;; Superklasse aller Variablen.
;;;-----------------------------------------------------------------------------
(def-lzs-object var (:named lzs-object+type) ; now with type slot
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
;; Statische Variable
;;;-----------------------------------------------------------------------------
(def-lzs-object static (var) ; wh type slot from lzs-object+type
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
;; Definierte statische Variable
;;;-----------------------------------------------------------------------------
(def-lzs-object defined-static (static)
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
;; Lokal definierte statische Variable
;;;-----------------------------------------------------------------------------
(def-lzs-object local-static (defined-static)
  :Annotations
  ;;-----------
  (link :initform '())  ;; Verbindung zur Verwendung der
  ;; Variablen
  (place :initform ())  ;; Adressausdruck
  (closure :initform ())              ; Kommt frei in einer Closure vor
  )

;;;-----------------------------------------------------------------------------
;; Global definierte statische Variable
;;;-----------------------------------------------------------------------------
(def-lzs-object global-static (:global defined-static)
  :Annotations
  ;;-----------
  (read-stats :initform ())            ; Lesen von dieser glob. Variablen
  (write-stats :initform ())           ; Beschreiben dieser glob. Variablen
  (place :initform ())   ;; Adressausdruck
  (initial-value :initform ^unknown)
  )

;;;-----------------------------------------------------------------------------
;; Importierte statische Variable
;;;-----------------------------------------------------------------------------
(def-lzs-object imported-static (:global :imported static)
  :Annotations
  ;;-----------
  (read-stats :initform ())            ; Lesen von dieser glob. Variablen
  (write-stats :initform ())           ; Beschreiben dieser glob. Variablen
  (place :initform ())  ;; Adressausdruck
  (initial-value :initform ^unknown)   ; used only to transmit initial value
  ;; for eval in .def-files
  )
;;;-----------------------------------------------------------------------------
;; Dynamische Variable.
;; Die Information, ob es sich um eine importierte und/oder exportierte Variable
;; handelt, kann aus dem Symbol ersehen werden.
;;;-----------------------------------------------------------------------------
(def-lzs-object dynamic (var)
  sym        ;; das zugehoerige Symbol
  :Annotations
  ;;-----------
  (read-stats :initform ())            ; Lesen von dieser glob. Variablen
  (write-stats :initform ())           ; Beschreiben dieser glob. Variablen
  (place :initform ())  ;; Adressausdruck
  )

;;;-----------------------------------------------------------------------------
;; Variablen Referenz
;;;-----------------------------------------------------------------------------
(def-lzs-object var-ref ()
  var
  :Annotations
  ;;----------
  (read-gloc :initform ())              ; access to a dynamic variable
  )


;;;-----------------------------------------------------------------------------
;; Benannte Konstanten von Eulisp
;;;-----------------------------------------------------------------------------
(def-lzs-object named-const (:global :named lzs-object+type)
  (value  ;; self-evaluating || 'unknown
   :initform ^unknown)
  :Annotations
  ;;-----------
  (place :initform ())  ;; Adressausdruck
  )
;;;-----------------------------------------------------------------------------
(def-lzs-object defined-named-const (named-const)
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
(def-lzs-object imported-named-const (:imported named-const)
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
;; Symbol
;; der Typ 'symbol' ist in CL reserviert.
;;;-----------------------------------------------------------------------------
(def-lzs-object sym (:global :named lzs-object+type)
  (constant-value        ;; for 'defconstant' in Common Lisp only
   :initform ^no-const)  ;; self-evaluating | 'unknown | 'no-const
  :Annotations
  ;;-----------
  (place :initform ())  ;; address expression
  (expanded-literal :initform ())     ; instance of <literal-instance>
  (syntactic :initform ())            ; form at the syntactic level, i.e.
  ;; before transformation to LZS
  ;; needed to fasten eval during syntax expansion
  )

;;;-----------------------------------------------------------------------------
;; Im Modul definiertes Symbol.
;; Der Slot 'package' ist notwendig, wenn mehrere Packages als ein Modul
;; uebersetzt werden.
;;;-----------------------------------------------------------------------------
(def-lzs-object defined-sym (sym)
  name
  package ;; Package-Name oder 'uninterned
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
;; Importiertes Symbol
;;;-----------------------------------------------------------------------------
(def-lzs-object imported-sym (:imported sym)
  :Annotations
  ;;-----------
  )


;;;-----------------------------------------------------------------------------
(def-lzs-object structured-literal (lzs-object+type)
  value
  :Annotations
  ;;-----------
  (place :initform ())   ;; Adressausdruck
  (expanded-literal :initform ())     ; instance of <literal-instance>
  (syntactic :initform ())            ; form at the syntactic level, i.e.
  ;; before transformation to LZS
  ;; needed to fasten eval and to guarantee
  ;; eq-equality during syntax expansion
  )
;;;-----------------------------------------------------------------------------
;; Eine zur Ubersetzungszeit bekannte Instanz einer Klasse.
;; Wird benoetigt zur Darstellung von #s(struct-name ..) in Common Lisp.
;;;-----------------------------------------------------------------------------
(def-lzs-object literal-instance (:named lzs-object+type)
  class
  value-list;; Werte der Slots als Liste von Literals
  :Annotations
  ;;-----------
  (place :initform ())  ;; Adressausdruck
  (expanded :initform ())              ; a flag, needed only to avoid recursion
  ;; during literal expansion
  (unexpanded :initform ^unknown)      ; the literal before expansion
  (gc-not-needed :initform ())
  )

;;;-----------------------------------------------------------------------------
;; Knoten zur Darstellung von definierenden und angewandten Vorkommen von
;; Klassen.
;; Die Klasse 'class' ist in CLOS schon vergeben, deshalb 'class-def'.
;;;-----------------------------------------------------------------------------
(def-lzs-object class-def (:global :named lzs-object+type)
  (supers :initform ())      ; list of class-def's
  (direct-slots :initform ()); list of slot-desc's
  (options :initform ())
  (converter :initform ())             ; a generic-fun
  class   ;; a metaclass
  :Annotations
  ;;-----------
  (lattice-type :initform ())          ; lattice type for type inference
  (place :initform ())  ;; Adressausdruck
  (effective-slots :initform ())
  (class-precedence-list :initform ())
  (expanded-literal :initform ())     ; instance of <literal-instance>
  (equal-pred :initform ())
  (copy-fun :initform ())
  (allocator :initform ^unknown)       ; fun which allocates instances
  (keywords :initform ())
  (type-identifier :initform ())
  (subclasses :initform ())
  representation
  gc-tracer
  )

;;;-----------------------------------------------------------------------------
(def-lzs-object defined-class (class-def)

  :Annotations
  ;;----------
  (constructors :initform ())
  (predicate :initform ())
  )

;;;-----------------------------------------------------------------------------
(def-lzs-object imported-class (:imported class-def)
  :Annotations
  ;;----------
  )

;;;-----------------------------------------------------------------------------
(def-lzs-object slot-desc (:named lzs-object+type)
  (default-function :initform ())          ; the function returning the initial
  ;; value
  (keyword :initform ())
  :Annotations
  ;;----------
  (initvalue :initform ^unknown)        ; set if the default is a constant
  ;; expression
  (reader :initform ())
  (writer :initform ())
  (accessor :initform ())
  (slot-of :initform ());; the class which defines the slot
  (specializes :initform ())           ; a slot-desc if the slot specializes
  ;; another one inherited from a
  ;; superclass
  offset
  )

;;;-----------------------------------------------------------------------------
;; Die formale Parameterliste einer Funktionsdefinition.
;; Dieser Knoten taucht nur als Slot in einer Funktionsdefinition auf, seine
;; Slots koennten also auch direkt in dem Knoten 'fun' angegeben werden.
;;;-----------------------------------------------------------------------------
(def-lzs-object params ()
  (var-list :initform ());; required Parameters
  (opt-list :initform ());; Liste von Instanzen von 'opt'
  (rest :initform ())   ;; &Rest Variable oder ()
  (key-list :initform ());; Liste von Instanzen von 'key'
  (allow-other-keys :initform ())
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
(def-lzs-object opt ()
  var
  init;; Init Value
  (suppl :initform ())  ;; Supplied Variable oder ()
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
(def-lzs-object key (opt)
  sym ;; Keyword Symbol
  :Annotations
  ;;-----------
  )


;;;-----------------------------------------------------------------------------
;; Funktion
;; Der Typ 'function' ist in CL reserviert.
;;;-----------------------------------------------------------------------------
(def-lzs-object fun ()
  params

  (setter :initform ()) ;; the setter function

  :Annotations
  ;;-----------
  (range-and-domain :initform ())       ; types of result and arguments
  ;; a vector #(result-type argtype...)
  (arg-num :initform ())  ;; Argumentanzahl
  (var-descr :initform ());; Variablendeskriptor
  (type-descr :initform ())              ; formaler Typedeskriptor
  (type-descr-s :initform ())            ; tmp formale Signatur
  (signature :initform ());; formale Signatur
  (actual  :initform 0)   ;; aktualitaet der Signatur
  (pass :initform 0)      ;; Bearbeitungszustand der Funktion
  (applications :initform ())            ; alle Aufrufe von fun
  (place :initform ())    ;; Aufrufadresse von fun
  (reduce :initform ())   ;; zum Vereinfachen von Funkt.aufr.
  (function-type :initform 0)            ; ist fun ein closure ?
  (read-glocs :initform ())              ; Lesen von glob. Variablen
  (write-glocs :initform ())             ; Schreiben von glob. Variablen
  (fread-gloc :initform ())              ; the result of all read-glocs
  (fwrite-gloc :initform ())             ; the result of all write-glocs
  (sys-glocs :initform ());; are fread-gloc and fwrite-gloc
  ;; changeable
  (inline :initform ())   ;; value: (), test or value
  (interpreter :initform () )            ; function in the compilation
  ;; environment to interpret calls of fun
  (expanded-literal :initform ())       ; instance of <literal-instance>
  )

;;;-----------------------------------------------------------------------------
;; Einfache Funktion (nicht generisch)
;;;-----------------------------------------------------------------------------
(def-lzs-object simple-fun (fun)
  (body :initform ^unknown) ; now with default 'unknown becuase of imported-fun
  :Annotations
  ;;-----------
  function-label          ;; zum Einsammeln der MZS-Bloecke
  calls;; alle Funktionsaufrufe in fun
  rec-calls;;; alle rekursiven Funktionsaufrufe von
  ;; fun
  tests
  moves

  get-slot-value
  set-slot-value

  )

;;;-----------------------------------------------------------------------------
;; Definierte einfache Funktion
;;;-----------------------------------------------------------------------------
(def-lzs-object defined-fun (simple-fun)
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
;; Globale definierte einfache Funktion
;;;-----------------------------------------------------------------------------
(def-lzs-object global-fun (:global :named defined-fun)
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
;; module initializing function
;;;-----------------------------------------------------------------------------
(def-lzs-object module-init-fun (global-fun)
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
;; lokale einfache Funktion
;; (Kann nicht importiert sein, das ist syntaktisch nicht moeglich.)
;;;-----------------------------------------------------------------------------
(def-lzs-object local-fun (:named defined-fun)
  :Annotations
  ;;-----------
  closure-vars            ;; closure-Variablen
  )

;;;-----------------------------------------------------------------------------
;; importierte einfache Funktion
;;;-----------------------------------------------------------------------------
(def-lzs-object imported-fun (:global :named :imported simple-fun)
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
;; Systemfunktion, die in gewissen Phasen des Compilers einer speziellen
;; Behandlung beduerfen.
;;;-----------------------------------------------------------------------------
(def-lzs-object special-sys-fun (imported-fun)
  :Annotations
  ;;-----------
  protocol-type
  match-list
  )

;;;-----------------------------------------------------------------------------
(def-lzs-object generic-fun (fun)
  (method-list :initform ())
  domain  ;; list of <class-def>
  class
  method-class

  :Annotations
  ;;-----------
  discriminating-fun
  method-lookup-fun
  discrimination-depth
  discrimination-arguments
  )

;;;-----------------------------------------------------------------------------
(def-lzs-object defined-generic-fun (generic-fun)
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
(def-lzs-object global-generic-fun (:global :named defined-generic-fun)
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
(def-lzs-object local-generic-fun (defined-generic-fun)
  :Annotations
  ;;-----------
  closure-vars            ;; closure-Variablen
  )

;;;-----------------------------------------------------------------------------
(def-lzs-object imported-generic-fun (:global :named :imported generic-fun)
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
(def-lzs-object method-def ()
  fun ;; the working function
  (domain :initform ())  ;; list of <class-def>
  class
  :Annotations
  ;;-----------
  generic-fun     ; the methods generic function
  (expanded-literal :initform ())     ; instance of <literal-instance>
  )

;;;-----------------------------------------------------------------------------
;; Eine Funktions-Anwendung;
;; 'form' ist ein 'fun' oder evaluiert zur Laufzeit zu einer Funktion.
;;;-----------------------------------------------------------------------------
(def-lzs-object app ()
  function
  (arg-list :initform ())
  :Annotations
  ;;-----------
  (read-glocs :initform ())              ; all global-reads in the arguments
  type-descr
  )

;;;-----------------------------------------------------------------------------
(def-lzs-object setq-form ()
  location
  form
  :Annotations
  ;;-----------
  type-descr
  (write-gloc :initform ())              ; setq to a global variable
  (read-gloc :initform ());; a read of a global variable
  )

;;;-----------------------------------------------------------------------------
(def-lzs-object progn-form ()
  form-list
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
(def-lzs-object if-form ()
  pred
  then
  else
  :Annotations
  ;;-----------
  (read-gloc :initform ()) ;; pred is an read access to a global
  ;; variable
  )

;;;-----------------------------------------------------------------------------
(def-lzs-object switch-form ()
  form
  case-list              ;; Liste von labeled-form
  otherwise              ;; Eine 'form'
  :Annotations
  ;;-----------
  )
;;;-----------------------------------------------------------------------------
(def-lzs-object labeled-form ()
  value   ;; sym oder simple-literal
  form
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
(def-lzs-object let*-form ()
  (var-list :initform ());; List of var
  (init-list :initform ())              ; List of initial values
  body
  :Annotations
  ;;-----------
  (type-list :initform ())
  read-gloc-list          ;; all read-glocs of the init-list
  write-gloc-list         ;; all write-glocs of the var-list
  )

;;;-----------------------------------------------------------------------------
(def-lzs-object labels-form ()
  (fun-list :initform ()) ;; List of fun
  body
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
(def-lzs-object let/cc-form ()
  cont
  body
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
(def-lzs-object cont (local-static)
  :Annotations
  ;;------------
  )

;;;-----------------------------------------------------------------------------
(def-lzs-object tagbody-form ()
  first-form             ;; Ausdruck vor dem 1. Tag, evtl. ()
  tagged-form-list
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
;; tagged-form repraesentiert zugleich das Tag und
;; alle zugehoerigen Go Ausdruecke
;;;-----------------------------------------------------------------------------
(def-lzs-object tagged-form ()
  form
  tagbody
  :Annotations
                ;;;----------
  label
  )
;;;-----------------------------------------------------------------------------
;; Konstrukt zum Ausdruecken der Special Forms multiple-value-call und
;; multiple-value-prog1 in der Zwischensprache.
;; Dieses Konstrukt ist sehr aehnlich zu dem Makro multiple-value-bind mit
;; der Ausnahme, dass eine beliebige Lambda-Liste angegeben werden kann.
;;;-----------------------------------------------------------------------------
(def-lzs-object mv-lambda ()
  params
  body
  arg ;; generiert evtl. multiple Werte
  :Annotations
  ;;-----------
  )
;;;-----------------------------------------------------------------------------
;; slot access
(def-lzs-object get-slot-value ()
  instance ; a form
  slot     ; a slot-desc
  :Annotations
  ;;-----------
  ;; annotations needed for the usage in the MZS
  block
  (arg-num;;; the number of arguments
   :initform 1)
  var-descr              ;; a variable descriptor
  type-descr             ;; the general type-descriptor
  type-descr-s           ;; list of type descriptors
  )

(def-lzs-object set-slot-value ()
  instance ; a form
  slot     ; a slot-desc
  value    ; a form
  :Annotations
  ;;-----------
  ;; annotations needed for the usage in the MZS
  block
  (arg-num;;; the number of arguments
   :initform 2)
  var-descr              ;; a variable descriptor
  type-descr             ;; the general type-descriptor
  type-descr-s           ;; list of type descriptors
  )

;;;-----------------------------------------------------------------------------
;;; extension of the LZS for EuLisp
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;; some specializations of <defined-class>
;;;-----------------------------------------------------------------------------
(def-lzs-object standard-class-def (defined-class)
  :Annotations
  ;;-----------
  )

(def-lzs-object abstract-class-def (standard-class-def)
  :Annotations
  ;;-----------
  )

(def-lzs-object tail-class-def (standard-class-def)
  :Annotations
  ;;-----------
  )

(def-lzs-object metaclass-def (standard-class-def)
  :Annotations
  ;;-----------
  )

;;;-----------------------------------------------------------------------------
;;; some specializations of <global-fun> for functions automatically created by
;;; the compiler
;;;-----------------------------------------------------------------------------
(def-lzs-object slot-accessor-fun (global-fun)
  ;; automatically created slot reader or slot writer function for a class
  :Annotations
  ;;-----------
  slot)

(def-lzs-object slot-init-fun (global-fun)
  ;; automatically created initialization function for a slot of a class
  :Annotations
  ;;-----------
  slot)

(def-lzs-object constructor-fun (global-fun)
  ;; automatically created constructor function for a class
  :Annotations
  ;;-----------
  keywords
  constructor-for)

(def-lzs-object predicate-fun (global-fun)
  ;; automatically created predicate function for a class
  :Annotations
  ;;-----------
  )

(def-lzs-object discriminating-fun (global-fun)
  ;; automatically created discriminating function of a generic function
  :Annotations
  ;;-----------
  )

;;;----------------------------------------------------------------------------
;;; predicates testing mixins
;;;----------------------------------------------------------------------------

(defgeneric imported? (object))

(defmethod imported? (object) ())

(defgeneric named? (object))

(defmethod named? (object)
  ;; this means that all 'global' object are also 'named'
  ;; 'global' is a submixin of 'named'
  (global? object))

(defgeneric global? (object))

(defmethod global? (object) ())

;;;-----------------------------------------------------------------------------
;;; compatibility to old annotation module-id
;;;-----------------------------------------------------------------------------

(export ?module-id)

(defun ?module-id (lzs-object)
  (if (?module lzs-object)
      (?identifier (?module lzs-object))
    ()))

#module-end

;;;end of module lzs
