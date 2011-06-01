(progn (print "this")
       (print "is")
       (print "a")
       (print "test"))

(progn (prin1 "this")
       (prin1 "is")
       (prin1 "a")
       (prin1 "test"))

(defun say-hello ()
      (print "Please type your name:")
      (let ((name (read)))
        (print "Please to meet, ")
        (print name)))

(defun add-five ()
  (print "Please enter a number:")
  (let ((num (read)))
    (print "When I add five I get")
    (print (+ num 5))))

(defun say-hello ()
      (princ "Please type your name: ")
      (let ((name (read-line)))
        (princ "Please to meet, ")
        (princ name)))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))


(defun game-read ()
  (let ((cmd (read-from-string
               (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
                     (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))


(defparameter *allowed-commands* '(look walk pickup inventory))


(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
    (eval sexp)
    '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eq item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))


