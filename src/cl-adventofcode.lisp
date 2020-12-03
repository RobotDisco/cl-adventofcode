;;;; Advent of Code 2020, Day 1
;;;; See the problem description over at https://adventofcode.com/2020/day/1

;;; I am trying to develop a sense of idiomatic Common LISP.
;;; I am looking at this repo https://github.com/christophejunke/aoc2020
;;; to get a sense of good common lisp patterns to follow.
;;;
;;; The first one was to take my boilerplate code of loading the lines of a
;;; file into a list and use a macro that allows for flexibility in what
;;; kind of data structure I return.

;; Given an exposed line entity, a file-name string, an optional result
;; and a code body...
(defmacro do-file-lines ((line file-name &optional result) &body body)
  ;;; Create a random variablename for the stream that won't collide
  ;;; with any symbol in my supplied body
  (let ((stream (gensym)))
    ;; Open a file and assign it to my generated stream variable
    `(with-open-file (,stream ,file-name)
       ;; I am guessing we supply and evaluate line because passed in the
       ;; body sexps may want to utilize it
       ;; Keep looping by calling READ-LINE to extract a new string
       (loop for ,line = (read-line ,stream nil)
	     ;; Don't terminate until LINE is null, indicating EOF
	     while ,line
	     ;; Use progn here to splice all of the body in where DO
	     ;; expects a single sexp. The @ means we're not adding a
	     ;; singly-rooted sexp tree, but that a list of flat sexps
	     ;; may be supplied, which makes sense for something that
	     ;; represents a body of code.
	     do (progn ,@body)
	     finally (return ,result)))))

(defun star1 ()
  ;; Can't assume arrays are growable in common lisp, have to
  ;; explicitly set that.
  (let ((elements (make-array 128
			      :element-type '(integer 0)
			      :adjustable t
			      :fill-pointer 0))
	(differences (make-hash-table)))
    ;; Use our macro, and pass in a body which populates our array.
    ;; We honestly did not need to return anything. All we do here is
    ;; parse the string into an integer and collect the numbers in an array.
    (do-file-lines (line "inputs/1.txt" elements)
      (vector-push-extend (parse-integer line)
			  elements
			  (array-total-size elements)))
    ;; Iterating across our list
    (loop for number across elements
	  ;; I needed to declare variables and setf them because getting
	  ;; multi-value results from something like GETHASH didn't work otherwise
	  with difference and found
	  do (setf (values difference found) (gethash number differences))
	  if found
	    ;; If our hashmap already contains this value as a key, we
	    ;; have our two numbers
	    return (* number difference)
	  else
	    ;; This algorithm banks on us knowing that, for any given
	    ;; number in this list, we know what the other number we
	    ;; need for this to be one of the qualifying pair of
	    ;; numbers. This is because we can subtract this number
	    ;; from 2020 to get that number. So we store this couplet
	    ;; in a hash so that when the second number of the pair
	    ;; shows up, it will find its pair in the hash already,
	    ;; and we can compute the answer.
	    do (setf (gethash (- 2020 number) differences) number))))

(defun star2 ()
  (let ((elements (make-array 128
			      :element-type '(integer 0)
			      :adjustable t
			      :fill-pointer 0))
	(differences (make-hash-table)))
    (with-open-file (in "./inputs/1.txt")
      (loop for line = (read-line in nil)
	    while line do
	      (vector-push-extend (parse-integer line)
				  elements
				  (array-total-size elements))))
    ;;; This is similar to the previous question except that here, we
    ;;; need to figure out a few things: compute the difference from
    ;;; 2020, but also compute new differences for any remainder that
    ;;; already exists in the hashmap since it is a sequence of
    ;;; numbers that will qualify (three numbers to be exact.) This
    ;;; will give us values that are either one integer for the second
    ;;; number to append itself to after tentatively subtracting from
    ;;; the remainder, and also the last value which will find itself
    ;;; in the remainder and associated with a value of the two
    ;;; numbers that pair with it to add up to 2020.
    (loop for number across elements
	  with partners and found
	  do (setf (values partners found) (gethash number differences))
	  if (and found (eql 2 (length partners)))
	    return (apply #'* number partners)
	  unless found do
	    (loop for k being each hash-key of differences using (hash-value v)
		  if (> k number)
		    do (let ((new-difference (- k number)))
			 (setf (gethash new-difference differences) (cons number v))))
	  do (setf (gethash (- 2020 number) differences) (list number)))))



