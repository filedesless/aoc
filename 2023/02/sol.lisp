;; Common Lisp solution
;; install with `brew install sbcl` on OSX
;; run with `sbcl --script sol.lisp`

(require "asdf")

(defparameter games '())

(with-open-file (stream #p"input")
    (do ((line (read-line stream nil) (read-line stream nil)))
        ((null line))
        (push line games)
    )
)

(setf games (reverse games))

(defun valid-p (pair)
    "Tells if a pair '(n color) is valid"
    (cond 
        ((string= "red" (second pair)) (<= (parse-integer (first pair)) 12))
        ((string= "green" (second pair)) (<= (parse-integer (first pair)) 13))
        ((string= "blue" (second pair)) (<= (parse-integer (first pair)) 14))
    )
)

(defun split (delim string)
    "Split a string into a list given a delimiter"
    (uiop:split-string (string-trim " " string) :separator delim)
)

(defun game-possible-p (line)
    "Tells if a game was possible"
    (let ((game (format nil "~{~A~^ ~}" (nthcdr 2 (split " " line)))))
        (notany 'null (mapcar (lambda (pick) 
            (every 'valid-p (mapcar 
                (lambda (pairs) (split " " pairs)) (split "," pick))
            )) (split ";" game))
        )
    )
)

(defun max-color (l color)
    "finds the highest occurrence of a color in a list '(0 red 1 blue ...)"
    (if (null l)
        0
        (max 
            (if (string= (second l) color) 
                (parse-integer (first l))
                0
            ) 
            (max-color (rest (rest l)) color))
    )
)

(defun game-cover (line)
    "Tells the number of red green blue balls needed to perform the game"
    (let ((game (format nil "~{~A~^ ~}" (nthcdr 2 (split " " line)))))
        (let ((words (split " " (remove #\, (remove #\; game)))))
            (apply '* (mapcar (lambda (color) (max-color words color)) '("red" "green" "blue")))
        )
    )
)

(defun game-id (game)
    "Returns the game id"
    (parse-integer (string-trim ":" (nth 1 (split " " game))))
)

(format t "part 01: ~A~%" (apply '+
    (mapcar (lambda (game) (if (game-possible-p game) (game-id game) 0)) games)
))

(format t "part 02: ~A~%" (apply '+
    (mapcar 'game-cover games)
))