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
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors:
;;;-----------------------------------------------------------------------------
;;;To access the export-list of an eulisp-module use the function module-exports
;;;(package el-modules). Usage: see importation from module el2lzs.

(in-package cl-user)

(import '(el-modules::module-exports ;(module-name)
          el-modules::module-syntax-exports ;(module-name)
          el-modules::describe-modules ;()
          el-modules::describe-module ;(module-name)
          el-modules::module-hierarchy ;(module-name [:used | :dependent])
          el-modules::find-eulisp-module ;(module-name)
          ))

(import (module-exports (find-eulisp-module 'el2lzs)))
;; this provides:
;;
;; (find-module module-id)
;; (find-lexical module-id binding-id)
;; (load-module [module-id | path | ""])    {"" on Mac only}
;; (pp-module module-id)
;; module-env; variable containing all modules of the recently compiled application
;;
;; !!!attention: the module-id and the function-id must be Eulisp-Symbols, i.e.
;; as constants they must be given as ^name

(shadowing-import (module-syntax-exports (find-eulisp-module ^dynamic)))
;; this provides the dynamic-Special-Forms of Eulisp

(import (module-exports (find-eulisp-module 'apply-compiler)))
