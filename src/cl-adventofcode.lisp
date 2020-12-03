(defmacro do-file-lines ((line file-name &optional result) &body body)
  (let ((stream (gensym)))
    `(with-open-file (,stream ,file-name)
       (loop for ,line := (read-line ,stream nil nil)
	     while ,line
	     do (progn ,@body)
	     finally (return ,result)))))

(defun star1 ()
  (let ((elements (make-array 128
			      :element-type '(integer 0)
			      :adjustable t
			      :fill-pointer 0))
	(differences (make-hash-table)))
    (do-file-lines (line "inputs/1.txt" elements)
      (vector-push-extend (parse-integer line)
			  elements
			  (array-total-size elements)))
    (loop for number across elements
	  with difference and found
	  do (setf (values difference found) (gethash number differences))
	  if found
	    return (* number difference)
	  else
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



