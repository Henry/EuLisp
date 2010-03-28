(defmodule tk_graph_editor
  (syntax (macros)
   import (level1 tcl-tk)
   )
(deflocal *canvas* ())
(deflocal *table-nodes* (make <table>))
(deflocal *first-node* ())
(deflocal *pos-x* 0)
(deflocal *pos-y* 0)
;;--------------------------------------------------------------------------;;
;;                                                                          ;;
;;                             Callback Functions                           ;;
;;                                                                          ;;
;;--------------------------------------------------------------------------;;
;; The next class will store all the necessary information
;; for the node. That is: 
;;  item               that contains the node
;;  x                  the x coordinate of the node
;;  y                  the y coordinate of the node
;;  list-edge-first    List with the lines items that start in that node
;;  list-edge-second   List with tha lines items that finish in that node.
(defclass <node> ()
  ((item accessor: item)
   (x accessor: coordx)
   (y accessor: coordy)
   (list-edge-first accessor: l-edge-first default: ())
   (list-edge-second accessor: l-edge-second default: ())
   ))
(defun mkNode (x y)
 (tk-bell)
  (let ((X (convert x <int>))
        (Y (convert y <int>))
        (node (make <node>)))
    ((setter coordx) node X)
    ((setter coordy) node Y)
   
    ((setter item) node 
     (tk-add-oval-canvas *canvas* (- X 10) (- Y 10) (+ X 10) (+ Y 10)
                         outline: "black" fill: "white" tags: "node"))
    ((setter table-ref) *table-nodes* (tk-item-canvas-id (item node)) node)))
(defun fill-black ()
  (tk-conf-item-canvas *canvas* "current" fill: "black"))
(defun fill-white ()
  (tk-conf-item-canvas *canvas* "current" fill: "white"))
(defun update-first ()
  (let ((result-find (tk-find-canvas-withtag *canvas* "current")))
    (and result-find
         (setq *first-node* (car result-find)))))
(defun update-second ()
 (let ((cur-node (tk-find-canvas-withtag *canvas* "current")))
    (and cur-node
         (setq cur-node (car cur-node))
         *first-node*
         (mkEdge cur-node))))
(defun update-position (x y)
  (setq *pos-x* (convert x <int>))
  (setq *pos-y* (convert y <int>)))
(defconstant first car)
(defconstant second cadr)
(defconstant third caddr)
(defconstant fourth cadddr)
(defun moveNode (x y)
  (setq x (convert x <int>))
  (setq y (convert y <int>))
  (let ((id (tk-find-canvas-withtag *canvas* "current"))
        cur-node 
        (x-incr (- x *pos-x*))
        (y-incr (- y *pos-y*)))
    (if id
        (progn 
          (setq cur-node (table-ref *table-nodes* (car id)))
          (tk-move-item-canvas (item cur-node) x-incr y-incr)
          ((setter coordx) cur-node (+ (coordx cur-node) x-incr))
          ((setter coordy) cur-node (+ (coordy cur-node) y-incr))
          ((setter table-ref) *table-nodes* (car id) cur-node)
          
    ;; The next piece of code moves the edges that has the current node
    ;; as the first node.
          (do (lambda (el)
                (let ((coords (tk-coords-item-canvas el)))
                  (tk-coords-item-canvas el (coordx cur-node) (coordy cur-node) 
                                         (third coords) (fourth coords))))
              (l-edge-first cur-node))
    ;; The next piece of code moves the edges that has the current node
    ;; as the second node
          (do (lambda (el)
                (let ((coords (tk-coords-item-canvas el)))
                  (tk-coords-item-canvas el (first coords) (second coords) 
                                         (coordx cur-node) (coordy cur-node))))
              (l-edge-second cur-node))
          (setq *pos-x* x)
          (setq *pos-y* y))
      ())))
;; The next function creates a line between the specified nodes. The first one is store
;; in the global variable. The second one is given by argument.
(defun mkEdge (sec-node)
  (let* ((node1 (table-ref *table-nodes* *first-node*))
         (node2 (table-ref *table-nodes* sec-node))
         (new-edge (tk-add-line-canvas *canvas* (coordx node1) (coordy node1)
                        (coordx node2) (coordy node2))))
    ((setter l-edge-first) node1 (cons new-edge (l-edge-first node1)))
    ((setter l-edge-second) node2 (cons new-edge (l-edge-second node2)))
    ((setter table-ref) *table-nodes* *first-node* node1)
    ((setter table-ref) *table-nodes* sec-node node2)
    (tk-lower-item-canvas new-edge)))
;; The next example allows you to create circles and to join them with lines.
;;
;; Clic Button 1 to produce a circle.
;; Clic Key 1 to set the origin of the line.
;; Clic Key 2 to set the destination of the line.
;; Circles can be moved dragging with Button 2
;; ----------------------------------------------------------------------------
(defun xup (x a b c)
  (format t "x: ~s\n" x)
   (format t "a: ~s\n" a)
  (format t "b: ~s\n" b)
  (format t "c: ~s\n" c)
  (flush))
(defun say-hello (a b c)
  (format t "Hello\n")
  (format t "a: ~s\n" a)
  (format t "b: ~s\n" b)
  (format t "c: ~s\n" c)
  (flush))
(defun simple-graphic-editor ()
 
  (tk-wm "title" () "simple-graphic editor")
  (setq *canvas* (tk-make-canvas ()))
  (tk-bind *canvas* "<Button-1>" mkNode x: y:)
  (tk-bind *canvas* "<KeyPress-1>" update-first)
  (tk-bind *canvas* "<KeyPress-2>" update-second)
                       
  (tk-bind-item-canvas *canvas* "node" "<Any-Enter>" fill-black)
  (tk-bind-item-canvas *canvas* "node" "<Any-Leave>" fill-white)
  
  (tk-bind-item-canvas *canvas* "node" "<Button-2>" update-position x: y:) 
  (tk-bind-item-canvas *canvas* "node" "<B2-Motion>" moveNode x: y:)
;;;;;;;
  ;; The next two lines are not important to the program. They just show 
  ;; how binding functions can receive accessors and arguments.
  (tk-bind-item-canvas *canvas* "node" "<Button-3>" xup x: args: (list "1" "2" "3"))
  (tk-bind *canvas* "<KeyPress-3>" say-hello args: (list "3" "4" "5"))
;;;;;;;
  (tk-pack *canvas*)
  (tk-focus *canvas*)
 
(Tk_MainLoop))
(simple-graphic-editor)
)