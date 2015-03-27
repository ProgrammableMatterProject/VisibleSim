;;;; Defines the lexer generator.
;;;; Copyright (C) 2009 David J. Rosenbaum, email: davidjrosenbaum@comcast.net
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, under version 3 of the License.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; Copyright (C) 2009 David J. Rosenbaum
;;;; This program comes with ABSOLUTELY NO WARRANTY; for details see COPYING.
;;;; This is free software, and you are welcome to redistribute it
;;;; under certain conditions; for details see COPYING.

(in-package :cl-lex)

(defmacro with-gensyms ((&rest symbols) &body body)
  "Replaces symbols in body with new symbols created by gensym.  body is treated as an implicit progn."
  `(let (,@(loop for symbol in symbols collect `(,symbol (gensym))))
     ,@body))

(defmacro define-string-lexer (name &body patterns)
  "Defines a function that takes a string and keyword arguments for the start and end of the string and returns a closure
that takes no arguments will return the next token each time it is called.  When the input string is exhausted or no more
matches are found the closure will return nil.  Each pattern must be a regular expression or a list of one regular
expression or a list containing a regular expression as the first element and the rest of the elements of the list forming
an implicit progn.  If a pattern is a regular expression or a list of one element, then text in the string that matches the
regular expression is ignored.  If pattern is a list of two elements then when the regular expression matches text the
sub-strings that match each register in the regular expression are bound to the symbols that represent the registers.  Any
text that matches named registers is bound to a variable with the same name as the register name.  If the same name is used
more than one register then subsequent have the appropriate index appended to their names.  If a register is not named,
then text that matches it is bound to $i where i is the index of the register.  The entire matching sub-string is bound to
$@.  If no text matches a register then its variable is bound to nil.  All symbols are interned in the current package when
the macro is expanded.  Patterns are applied in the order they are provided and multiple patterns cannot be applied to the
same piece of text.  Any text that is not matched to a pattern is skipped.  The behavior of the regular expressions can be
modified by setting the appropriate variables in the cl-ppcre regex library."
  (let (regexes forms (registers (list 0)) register-names (used-register-names (make-hash-table :test #'equal)))
      (dolist (pattern patterns)
	(multiple-value-bind (regex form)
	    (if (consp pattern)
		(values (car pattern) `(progn ,@(cdr pattern)))
		(values pattern nil))
	  (push regex regexes)
	  (push form forms)
	  (push (+ (car registers)
		   (progn
		     (push nil register-names)
		     (do ((i 0 (1+ i)) (open-backslash nil (and (null open-backslash) (char= (char regex i) #\\)))
			  (open-character-set nil (case (char regex i)
						    (#\[ (or open-character-set (not open-backslash)))
						    (#\] (and open-backslash open-character-set))
						    (t open-character-set)))
			  (open-parentheses nil (and (null open-backslash)
						     (null open-character-set)
						     (char= (char regex i) #\()))
			  (registers 0 (if open-parentheses
					   (progn
					     (if (char= (char regex i) #\?)
						 (let ((register-name-end (position #\> regex :start i)))
						   (if (and register-name-end
							    (< (+ i 3) (length regex))
							    (char= (char regex (1+ i)) #\<))
						       (push (do* ((j 1 (1+ j))
								   (register-base-name
								    (string-upcase
								     (subseq regex
									     (+ i 2)
									     (1+ (position-if (lambda (char)
												(not (member char
													     '(#\0
													       #\1
													       #\2
													       #\3
													       #\4
													       #\5
													       #\6
													       #\7
													       #\8
													       #\9))))
											      regex
											      :from-end t
											      :end register-name-end)))))
								   (register-name (string-upcase
										   (subseq regex
											   (+ i 2)
											   register-name-end))
										  (format nil
											  "~a~d"
											  register-base-name
											  j)))
								  ((not (gethash register-name used-register-names))
								   (setf (gethash register-name used-register-names) t)
								   register-name))
							     register-names)
						       (error "invalid named register in ~a" regex)))
						 (let ((register-name (format nil "$~d" (1+ registers))))
						   (setf (gethash register-name used-register-names) t)
						   (push register-name register-names)))
					     (1+ registers))
					   registers)))
			 ((= i (length regex)) registers)))
		   1)
		registers)))
      (let ((total-registers (car registers)))
	(setf regexes (nreverse regexes)
	      forms (nreverse forms)
	      registers (nreverse (cdr registers))
	      register-names (nreverse register-names))
	(let ((combined-regex (apply #'concatenate (cons 'string (mapcar (lambda (regex)
									   (concatenate 'string "(" regex ")|"))
									 regexes)))))
	  (if (> (length combined-regex) 0)
	      (setf combined-regex (subseq combined-regex 0 (1- (length combined-regex)))))
	  (with-gensyms (scanner string start end match-start match-end register-starts register-ends)
	    `(let* ((cl-ppcre:*allow-named-registers* t) (,scanner (cl-ppcre:create-scanner ,combined-regex)))
	       (defun ,name (,string &key (,start 0) (,end (length ,string)))
		 (declare (ignorable ,start))
		 (if (null ,end)
		     (setf ,end (length ,string)))
		 (lambda ()
		   ,(if registers
			`(loop
			    (multiple-value-bind (,match-start ,match-end ,register-starts ,register-ends)
				(cl-ppcre:scan ,scanner ,string :start ,start :end ,end)
			      (declare (ignorable ,register-ends))
			      (if ,match-start
				  (progn
				    (if (eql ,match-start ,match-end)
					(error "matched the empty string at position ~d, this will cause an infinite loop"
					       ,match-start))
				    (setf ,start ,match-end)
				    (ecase (position-if #'identity ,register-starts)
				      ,@(mapcar (lambda (register-start register-end form)
						  (let* ((local-register-names (mapcar (lambda (register-name)
											 (intern register-name))
										       (if (< (1+ register-start)
											      total-registers)
											   (subseq register-names
												   (1+ register-start)
												   register-end))))
							 (global-registers (loop
									      for j from (1+ register-start)
									      to total-registers collect j))
							 ($@ (intern "$@")))
						    `(,register-start (let ((,$@ (subseq ,string
											 ,match-start
											 ,match-end))
									    ,@(mapcar (lambda (local-register-name i)
											`(,local-register-name
											  (if (aref ,register-starts ,i)
											      (subseq ,string
												      (aref
												       ,register-starts
												       ,i)
												      (aref
												       ,register-ends
												       ,i)))))
										      local-register-names
										      global-registers))
									(declare (ignorable ,$@ ,@local-register-names))
									,form))))
						registers
						(concatenate 'list (cdr registers) (list nil))
						forms)))
				  (return)))))))))))))

(defun stream-lexer (read-source-line string-lexer opening-delimiter-p closing-delimiter-p &key (stream *standard-input*))
  "Returns a closure that takes no arguments and will return each token from stream when called.  read-source-line is a
function that takes an input stream and returns a line of the source.  When EOF is encountered, read-source-line should
return t.  string-lexer is a function that returns a lexer that behaves as if it was returned by a function defined using
define-string-lexer. opening-delimiter-p is a function that takes a character and returns true if it is an opening
delimiter and false otherwise.  closing-delimiter-p is a function that takes a character When there no more tokens are
left, nil is returned.  nil is also returned when a newline is encountered and there are no open delimiters.  If the
closure is called again after EOF has been encountered a condition of type end-of-file is signalled."
  (let (eof line-lexer (open-delimiters 0) (update t))
    (labels ((next-token ()
	       (multiple-value-bind (token value)
		   (funcall line-lexer)
		 (cond
		   ((funcall opening-delimiter-p token) (incf open-delimiters))
		   ((funcall closing-delimiter-p token) (decf open-delimiters)))
		 (values token value)))
	     (update-line-lexer ()
	       (let ((line (funcall read-source-line stream)))
		 (if (null line)
		     (setf eof t))
		 (setf line-lexer (funcall string-lexer line)))))
      (lambda ()
	(when update
	  (update-line-lexer)
	  (setf update nil))
	(multiple-value-bind (token value)
	    (next-token)
	  (if token
	      (values token value)
	      (if eof
		  (error 'end-of-file :stream stream)
		  (if (> open-delimiters 0)
		      (progn
			(update-line-lexer)
			(next-token))
		      (progn
			(setf update t
			      open-delimiters 0)
			nil)))))))))
