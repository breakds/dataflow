#! /u/b/r/breakds/usr/local/bin/sbcl --script
(require :asdf)

;; Program loader and parser
(defparameter *edges* nil)
(defparameter *code-array* nil)

(defun lstrip (str)
  ;; strip the leading spaces (space/tab)
  (if (or (eq (char str 0) #\  )
	  (eq (char str 0) #\tab))
      (lstrip (subseq str 1))
      str))

(defun rstrip (str)
  ;; strip the trailing spaces (space/tab)
  (let ((tail (char str (1- (length str)))))
    (if (or (eq tail #\  )
	    (eq tail #\tab))
	(rstrip (subseq str 0 (1- (length str))))
	str)))

(defun strip (str)
  (lstrip (rstrip str)))

(defun if-p (str)
  (if (and (>= (length str) 2) (equal (subseq str 0 2) "if"))
      t
      nil))

(defun else-p (str)
  (if (and (>= (length str) 4) 
	   (or (equal (subseq str 1 5) "else")
	       (equal (subseq str 2 6) "else")))
      t
      nil))

(defun while-p (str)
  (if (and (>= (length str) 5) (equal (subseq str 0 5) "while"))
      t
      nil))

(defun assignment-p (str)
  (if (find-if (lambda (x) (equal x #\=)) str)
      t
      nil))


(defun {-p (str)
  (if (eq (char str 0) #\{)
      t
      nil))

(defun }-p (str)
  (if (eq (char str 0) #\})
      t
      nil))


(defun load-program (stream)
  (loop for line  = (read-line stream nil 'eof)
       until (eq line 'eof)
       collect (strip line)))
    


(defun add-edges (k pre edges)
  (reduce (lambda (x y) (cons (list x k) y))
	  pre
	  :initial-value edges
	  :from-end t))

(defun parse-prog (program code edges stack pre k)
  (if (null program)
      (values (reverse code) (cons (list (1- k) k) edges))
      (let ((line (remove-if (lambda (x) (eq x #\{)) (car program))))
	(cond ((if-p line)
	       (parse-prog (rest program)
			   (cons line code)
			   (add-edges k pre edges)
			   (cons (list 'if k) stack)
			   (list k)
			   (1+ k)))
	      ((while-p line)
	       (parse-prog (rest program)
			   (cons line code)
			   (add-edges k pre edges)
			   (cons (list 'while k) stack)
			   (list k)
			   (1+ k)))
	      ((else-p line)
	       (let ((org (car stack)))
		 (parse-prog (rest program)
			     code
			     edges
			     (cons (append org (list (1- k))) (rest stack))
			     (list (cadr org))
			     k)))
	      ((}-p line)
	       (let ((org (car stack)))
		 (if (eq (car org) 'if)
		     (parse-prog (rest program)
				 code
				 edges
				 (rest stack)
				 (list (caddr org) (1- k))
				 k)
		     ;; while
		     (parse-prog (rest program)
				 code
				 (cons (list (1- k) (cadr org)) edges)
				 (rest stack)
				 (list (cadr org))
				 k))))
	      (t (parse-prog (rest program)
			     (cons line code)
			     (add-edges k pre edges)
			     stack
			     (list k)
			     (1+ k)))))))

(defun load-and-parse-program (stream)
  "Load program and return a code array and an edge list"
  (multiple-value-bind (code edges) (parse-prog (load-program stream)
						nil
						nil
						nil
						(list 0)
						1)
    (let ((code-complete (append (list "enter") code (list "exit"))))
      (setq *code-array* (make-array (length code-complete) 
				     :initial-contents code-complete))
      (setq *edges* edges))))



;; Draw Graph (CFG)
(defun edges->dot (edges)
  (loop for pair in edges do
       (fresh-line)
       (princ (car pair))
       (princ " -> ")
       (princ (cadr pair))
       (princ ";")))

(defun vertices->dot (vertices)
  (loop for i below (length vertices) do
       (fresh-line)
       (princ i)
       (princ "[label=\"")
       (princ (aref vertices i))
       (princ "\"];")))

(defun graph->dot (edges vertices)
  (fresh-line)
  (princ "digraph G {")
  (fresh-line)
  (princ "node [shape=record];")
  (edges->dot edges)
  (vertices->dot vertices)
  (fresh-line)
  (princ "}"))



(defun graph->png (edges vertices filename)
  (with-open-file (*standard-output*
		   filename
		   :direction :output
		   :if-exists :supersede)
    (graph->dot edges vertices))
  (asdf:run-shell-command (concatenate 'string "dot -Tpng -O " (namestring (truename filename)))))
       
 



(defun letter-p (x)
  "whether x is a letter, in [a-z][A-Z]"
  (let ((a (char-code x)))
    (or (and (>= a 97) (<= a 122))
	(and (>= a 65) (<= a 90)))))

(defun get-variable (str)
  "variables are supposed to be single letters in string"
  (remove-if #'null
	     (loop for i below (length str) 
		collect (let ((b (if (> i 0) (char str (1- i)) #\*))
			      (c (char str i))
			      (a (if (< i (1- (length str))) (char str (1+ i)) #\*)))
			  (if (and (not (letter-p b))
				   (not (letter-p a))
				   (letter-p c))
			      c
			      nil)))))

;; Live Variable Analysis
(defun get-generated (code-array)
  (make-array (length code-array)
	      :initial-contents
	      (loop for line in (coerce code-array 'list) collect
		   (if (assignment-p line)
		       (get-variable (subseq line (position #\= line)))
		       (get-variable line)))))

(defun get-killed (code-array)
  (make-array (length code-array)
	      :initial-contents
	      (loop for line in (coerce code-array 'list) collect
		   (if (assignment-p line)
		       (get-variable (subseq line 0 (position #\= line)))
		       nil))))

(defparameter *after* nil)
(defparameter *before* nil)
(defparameter *work-list* nil)
(defparameter *killed* nil)
(defparameter *generated* nil)

(defun init-live ()
  (setq *after* (make-array (length *code-array*)
			    :initial-contents 
			    (loop for i below (length *code-array*) collect nil)))
  (setq *before* (make-array (length *code-array*)
			     :initial-contents
			     (loop for i below (length *code-array*) collect nil)))
  (setq *work-list* (loop for i from 1 to (1- (length *code-array*))
			 collect i))
  (setq *killed* (get-killed *code-array*))
  (setq *generated* (get-generated *code-array*)))

			      

(defun state-vertices->dot (vertices)
  (loop for i below (length vertices) do
       (fresh-line)
       (princ i)
       (princ "[label=\"before: ")
       (princ (aref *after* i))
       (princ "|")
       (princ (aref vertices i))
       (princ "|after: ")
       (princ (aref *before* i))
       (princ "\"];")))

(defun work-list->dot ()
    (fresh-line)
    (princ "graph {")
    (fresh-line)
    (princ "node [shape=record];")
    (fresh-line)
    (princ "1000[label=\"{")
    (loop for i in *work-list*
       for k from 100 do
	 (princ (aref *code-array* i))
	 (princ "|"))
    (princ "}\"];")
    (princ "}"))
	 


(defun state-graph->dot (edges vertices)
  (fresh-line)
  (princ "digraph G {")
  (fresh-line)
  (princ "node [shape=record];")
  (edges->dot edges)
  (state-vertices->dot vertices)
  (fresh-line)
  (princ "}"))



(defun state-graph->png (edges vertices filename)
  (with-open-file (*standard-output*
		   filename
		   :direction :output
		   :if-exists :supersede)
    (state-graph->dot edges vertices))
  (asdf:run-shell-command (concatenate 'string "dot -Tpng -O " (namestring (truename filename)))))

(defun work-list->png (filename)
  (with-open-file (*standard-output*
		   filename
		   :direction :output
		   :if-exists :supersede)
    (work-list->dot))
  (asdf:run-shell-command (concatenate 'string "dot -Tpng -O " (namestring (truename filename)))))



(defun live-meet (x pre)
  (loop for i in pre do
       (setf (aref *before* x) (union (aref *before* x) (aref *after* i)))))
(defun live-propagate (x)
  (let ((org (aref *after* x)))
    (setf (aref *after* x) (union (set-difference (aref *before* x) (aref *killed* x))
				  (aref *generated* x)))
    (if (equal org (aref *after* x))
	nil
	t)))

(defun back-data-analysis-next ()
  (when (not (null *work-list*))
    (let* ((x (car *work-list*))
	   (pre (remove-if #'null 
			   (loop for pair in *edges* collect (when (= (car pair) x) (cadr pair))))))
      (setq *work-list* (rest *work-list*))
      (live-meet x pre)
      (when (live-propagate x)
	(let ((succ (remove-if #'zerop
			       (loop for pair in *edges* collect (if (= (cadr pair) x)
								     (car pair)
								     0)))))
	  (setq *work-list* (append *work-list* succ)))))))



;; CGI Utils

(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
	       (coerce (list c1 c2) 'string)
	       :radix 16
	       :junk-allowed t)))
    (if code
	(code-char code)
	default)))

(defun decode-param (s)
  (labels ((f (lst)
	     (when lst
	       (case (car lst)
		 (#\% (let ((ch (http-char (cadr lst) (caddr lst))))
			(if (not (= (char-code ch) 13))
			    (cons ch (f (cdddr lst)))
			    (f (cdddr lst)))))
		 (#\+ (cons #\space (f (cdr lst))))
		 (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))

(defun get-query (str)
  "store query variables in an alist"
  (if str 
      (labels ((get-query-iter (str accu)
		 (let ((i1 (position #\= str))
		       (i2 (position #\& str)))
		   (if i1 
		       (let* ((pair (list (intern (string-upcase (subseq str 0 i1)))
					  (subseq str (1+ i1) i2)))
			      (accu1 (cons pair accu)))
			 (if i2
			     (get-query-iter (subseq str (1+ i2)) accu1)
			     accu1))
		       accu))))
	(get-query-iter str nil))
      nil))
  

(defmacro with-query (vars &body body)
  (let ((query (get-query (asdf:getenv "QUERY_STRING"))))
    `(let ,(mapcan
	    (lambda (var) 
	      (cond 
		((atom var) 
		 (list (list var
			     (cadr (assoc var query)))))
		((eq (cadr var) 'int) 
		 (list (list (car var) 
			     (parse-integer (cadr (assoc (car var) query))
					    :junk-allowed t))))
		((eq (cadr var) 'string)
		 (list (list (car var)
			     (cadr (assoc (car var) query)))))
		(t nil)))
	    vars)
       ,@body)))



;; *- main -*
(format t "Content-Type: text/html~%~%")
(format t "<html>~%")
(format t "<head><title>Live Variable Analysis</title></head>~%")
(format t "<body>~%")
(with-query ((state int) program)
  (cond
    ((= state 0)
     (when program
       (load-and-parse-program (make-string-input-stream (decode-param program)))
       (when *code-array*
	 (asdf:run-shell-command "rm -f img/*")
	 (princ "<b> Analysis Processing ... <b>")
	 (init-live)
	 (loop while *work-list* 
	    for i from 1
	    do (back-data-analysis-next)
	      (work-list->png (format nil "img/w~a" i))
	      (state-graph->png *edges* *code-array* (format nil "img/~a" i)))
	 (format t "<form>~%")
	 (format t "<input type=\"hidden\" name=\"state\" value=\"1\" \>")
	 (format t "<input type=\"submit\" value=\"View\" \>")
	 (format t "</form>"))))
    ((> state 0)
     ;; Buttons
     (format t "<table>~%")
     (format t "<tr>~%")
     (format t "<td>~%")
     (format t "<form action=\"start.html\">~%")
     (format t "<input type=\"submit\" value=\"Input another program.\" \>")
     (format t "</form>")
     (format t "</td>~%")
     (format t "<tr>~%")
     (format t "<tr>~%")
     (when (> state 1)
       (format t "<td>~%")
       (format t "<form>~%")
       (format t "<input type=\"hidden\" name=\"state\" value=\"~a\" \>" (1- state))
       (format t "<input type=\"submit\" value=\"Prev\" \>")
       (format t "</form>")
       (format t "</td>~%"))
     (when (probe-file (format nil "img/~a.png" (1+ state)))
       (format t "<td>~%")
       (format t "<form>~%")
       (format t "<input type=\"hidden\" name=\"state\" value=\"~a\" \>" (1+ state))
       (format t "<input type=\"submit\" value=\"Next\" \>")
       (format t "</form>")
       (format t "</td>~%"))
     (format t "</tr>~%")
     (format t "</table>~%")
     ;; imgs
     (format t "<table>~%")
     (format t "<tr>~%")
     (format t "<td><img src=\"img/~a.png\" /></td>" state)
     (format t "<td><img src=\"img/w~a.png\" /></td>" state)
     (format t "</tr>~%")
     (format t "</table>~%"))
    (t (princ "No State Variable Passed!"))))
(format t "</body>~%")
(format t "</html>~%")
	       
		    




  

		   
				       
		   
	       
  

