;;; Hy bootstrap macros
;;
;; Copyright (c) 2013 Nicolas Dandrimont <nicolas.dandrimont@crans.org>
;; Copyright (c) 2013 Paul Tagliamonte <paultag@debian.org>
;; Copyright (c) 2013 Konrad Hinsen <konrad.hinsen@fastmail.net>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
;;
;;; These macros are the essential hy macros.
;;; They are automatically required everywhere, even inside hy.core modules.


(import hy.models.list
        hy.models.string
        hy.macros
        collections
        uuid)


(defmacro if [&rest args]
  "if with elif"
  (setv n (len args))
  (if* n
       (if* (= n 1)
            (get args 0)
            `(if* ~(get args 0)
                  ~(get args 1)
                  (if ~@(cut args 2))))))

(defmacro macro-error [location reason]
  "error out properly within a macro"
  `(raise (hy.errors.HyMacroExpansionError ~location ~reason)))


(defmacro if-python2 [python2-form python3-form]
  "If running on python2, execute python2-form, else, execute python3-form"
  (import sys)
  (if (< (get sys.version_info 0) 3)
    python2-form
    python3-form))


(defmacro defn [name lambda-list &rest body]
  "define a function `name` with signature `lambda-list` and body `body`"
  (if (not (= (type name) HySymbol))
    (macro-error name "defn takes a name as first argument"))
  (if (not (isinstance lambda-list hy.models.list.HyList))
    (macro-error name "defn takes a parameter list as second argument"))
  `(setv ~name (fn ~lambda-list ~@body)))


(defmacro let [passed-forms &rest body &kwargs opts]
  "Create a new lexical scope"
  (setv compiler (.get opts "compiler"))

  ; Abstract
  ; ========
  ;
  ; This macro is a way bit intense, so, let's go ahead and talk a bit about
  ; what this thing does.
  ;
  ; First, we're going to go ahead and write out a new dictionary to a var
  ; (we use a (uuid.uuid4) since (gensym) isn't loaded yet. We'll use this
  ; to read-and-write rather than relying on Python scope. We'll always resort
  ; to using nonlocal-alike lookup, and mutating it.
  ;
  ; After that's all set, we'll go ahead and mutate the body to change any
  ; names we `let` to to a dictonary lookup.
  ;
  ; The trick there is to continue to `macroexpand` things in the body
  ; until we're done, so that sub-let forms can do their thang, and let
  ; anything that expands out to a name lookup to set the right stuff.

  (defn coll?* [el]
    (isinstance el list))

  (defn symbol?* [el]
    (isinstance el HySymbol))

  (defn generic-visit [visit node]
    (if (coll?* node)
        ((type node) (list-comp (visit x) [x node]))
        node))

  (defn form-names [forms]
    (for* [(, el _)  forms]
      (if (coll?* el)
        (for* [x el]
          (yield x))
        (yield el))))

  (defn replace-form-tuples [forms]
    (for* [(, name value) forms]
      (if (and (isinstance name HyExpression) (= (get name 0) ","))
          (yield [(hy.models.list.HyList (cut name 1)) value])
          (yield [name value]))))

  (defn do-find-replace [filter needle haystack found]
    (setv ret needle)
    (for* [el haystack]
      (if (filter el needle)
          (do (setv ret (found el needle))
              (break))))
    ret)

  (defn name-mangle [forms]
    (setv names (list (form-names forms)))

    (defn visit [node]
      (if (symbol?* node)
          (if (in node names)
              `(get ~closure-name ~(hy.models.string.HyString node))
              (do-find-replace
                (fn [name node] (node.startswith (+ name ".")))
                node names
                (fn [name node] (visit
                  `(. ~name ~(HySymbol (cut node (+ 1 (len name)))))))))
          (generic-visit visit node))))

    (defn create-assignments [forms]
      (setv form-stream
            (zip (list-comp (cut forms 0 i) [i (range 0 (len forms))])
                 (list-comp `(setv ~((name-mangle forms) name) ~value)
                            [(, name value) forms])))
      (list-comp ((name-mangle x) y) [(, x y) form-stream]))

  (setv body (list-comp (hy.macros.macroexpand x compiler) [x body]))
  (generic-visit (defn expander [node]
    (generic-visit expander (hy.macros.macroexpand node compiler))) body)

  (setv forms (list (apply zip (* [(iter passed-forms)] 2))))
  (setv closure-name (HySymbol (. (uuid.uuid4) hex)))
  (setv closure `(do (setv ~closure-name {})))
  (setv forms (list (replace-form-tuples forms)))

  `(do ~closure
       ~@(create-assignments forms)
       ~@(map (name-mangle forms) body)))
