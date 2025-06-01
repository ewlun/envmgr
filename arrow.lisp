(in-package #:envmgr)

(defmacro -> (x &rest forms)
  "Threads the expr through the forms. Inserts x as the
second item in the first form, making a list of it if it is not a
list already. If there are more forms, inserts the first form as the
second item in second form, etc.

E.g. `(-> 3 (/ 2) /)`
expands to `(/ (/ 3 2))`
=> 2/3"
  (reduce (lambda (x form)
            (if (listp form)
                `(,(first form) ,x ,@(rest form))
                (list form x)))
          forms :initial-value x))


(defmacro as-> (expr name &rest forms)
  "Binds name to expr, evaluates the first form in the lexical context
of that binding, then binds name to that result, repeating for each
successive form, returning the result of the last form.

E.g. `(as-> 3 $ (* 5 $) (/ $ 7))`
expands to 
```
(LET* (($ 3)
       ($ (* 5 $)))
  (/ $ 7))
```
=> 15/7"
  `(let* ((,name ,expr)
          ,@(mapcar (lambda (form)
                      (list name form))
                    (butlast forms)))
     ,(if (null forms)
          name
          (first (last forms)))))
