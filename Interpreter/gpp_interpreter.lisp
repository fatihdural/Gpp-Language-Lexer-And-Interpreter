; Fatih DURAL
; 151044041

; HW3 Part2
; deffun keywordu yoktur.

; calistirma opsiyonlari
; terminal de clisp gpp_interpreter.lisp ile.
; terminal de clisp komutu ile clisp terminaline gectikten sonra, (load "gpp_interpreter.lisp") -> (gppinterpreter) ile terminal girdisi olarak.
; terminal de clisp komutu ile clisp terminaline gectikten sonra, (load "gpp_interpreter.lisp") -> (gppinterpreter "input.txt") ile.
; Syntax Error durumunda tamamen programdan cikar.

(defun gppinterpreter(&optional file) ; opsiyonel argumanli ana fonksiyon.
 	(if (eq file nil)  ; arguman yoksa, dosya ismi verilmemisse, surekli olarak verilen girdi okunur.
 		(progn
 			(setf line-result nil)
		 	(loop
				(setf readable (read-line))
				(setf readable (list-to-charseq readable) )
				(setf readable  (reverse (cdr (reverse (cdr readable)))) )
 				(setf parse-line (gppinterpreter-helper readable  ) )
 				(setq line-result (CFG-parsing (edit-text parse-line) ) )
 				(format t "Syntax OK.")
				(terpri)
				(format t "Result: ~a" (car (cdr line-result)) )
				(terpri)
				(format t "-----------------------------------")
				(terpri)
			)
 		)
 		(progn 		; arguman olarak dosya ismi verilirse, dosya okunur, yardimci fonksiyona icerik verilir.
			(let ((in (open file :if-does-not-exist nil))) ; dosya sonuna kadar karakter karakter okunur.
	 			(setq temp_list nil)
				(when in
				(loop for line = (read-char in nil)
			        while line do (setq temp_list (push line temp_list)))
			    (close in))) ; dosya kapanir.
				(setq temp_list (reverse temp_list))
				(setf result-line '(0))
				(setf count-op 0)
				(setf count-cp 0)
				(setf theLastResult nil)
				(loop 
					(if (equal #\; (car temp_list) ) ; commenti isleme alma
						(progn 
							(loop
								(setq temp_list (cdr temp_list))
								(when (equal #\Newline (car temp_list) )  (return 0))
							)
							(setq temp_list (cdr temp_list))
						)
					)
					(setq result-line (append result-line (list (car temp_list)) ))
					(if (equal #\(  (car temp_list)) 	; parantezler de denklik sagla
						(progn
							(setq count-op (+ 1 count-op))

						)
					)
					(if (equal #\) (car temp_list)) 
						(progn
							(setq count-cp (+ 1 count-cp))

						)						
					)
					(if  (and (not (equal count-op 0)  ) (not (equal count-cp 0)  ) (equal count-op count-cp ))      
						(progn 
							(setf parse-line (gppinterpreter-helper (cdr result-line)  ) ) ; CFG ye gonder
							(setq theLastResult (CFG-parsing (edit-text parse-line)   ) )
							(setq result-line '(0))
							(setf count-op 0)
							(setf count-cp 0)
						)
					)
					(setq temp_list (cdr temp_list))
					(if  (equal #\Newline (car temp_list) ) 
						(setq temp_list (cdr temp_list))
					)
					(when (null temp_list) (return))
				) 
				(if (and (not (equal count-op 0)  ) (not (equal count-cp 0)  ) (equal count-op count-cp ))
					(setq theLastResult (CFG-parsing (edit-text (gppinterpreter-helper (cdr result-line)  ))   ) ) 
				)
				(format t "Syntax OK.")		; print
				(terpri)
				(format t "Result: ~a" (car (cdr theLastResult)) )
				(terpri)
				(format t "-----------------------------------")
				(terpri)
				(return-from gppinterpreter "FINISH")
 		)
 	)
)

(defun edit-text (text)
	(loop
		(if (equal '(OP_OP) (list (car text)) )
			(progn
				(setq text (reverse (cdr (reverse (cdr text))) ) )
			)
			(return text)

		)
		(when (null text) (return text) )
	)
	(append (append text '(OP_CP) ) '(OP_CP) ) 
)

(defun list-to-charseq (text)	; girdi liste oldugundan gerekli donusum yapilir.
	(setf text (format nil "~s" text) ) ; list to string
  	(assert (stringp text) (text) "~s :string") ; string to char sequence
  	(setf text (coerce text 'list) )
)

(setf symbolTable '(0))
(defun CFG-parsing(text)
	(let ( (result '())  (count 0) (flag 0) )
		(setf text-token nil)
		(loop
			(if (equal '(OP_OP) (list (car text)) )	; her defasinda ic kisma ilerler.
				(progn
					(setq returned-value  (CFG-parsing (copy-list (cdr text) ) ))  ; recursive CFG algoritmasi
					(if (equal flag 0)
						(progn 
							(setq countt (car returned-value))
							(setq count  (+ countt (+ count 1) )  ) 
							(setq flag (+ flag 1))
						)
						(progn
							(setq count (car returned-value))
							(setq count (+ 1 count))
						)
					)
					(setq count_in 0)
					(loop 
						(loop
							(setq text (cdr text))
							(when (or (null text) (equal '(OP_CP) (list (car text))  ) )  (return 0))
						)
						(setq count_in (+ 1 count_in))
						(when (equal count count_in) (return 0) )
					)
					(setq result  (push  (car (cdr returned-value) )   result ) ) 
				)  
				(progn
					(if (equal '(OP_CP) (list (car text)) )			; sonuc hesaplanir.
						(progn
							(setq result (reverse result))
							(setq tempresult (list (create-result  result ))  )
							(setq tempresult (append (list count )  tempresult))
							(return tempresult)
						)  
						(progn
							(setq result (push (car text)  result ) ) 
						)
					)
				)
			)
			(setq text (cdr text))
			(when  (or (equal 0 (list-length text))  (null text) )    (return result))
		)
	)
)

(defun char-to-int(element)
	(- (char-code element) 48) 
)
(defun character-to-int(element)
	(- (char-int element) 48) 
)
(defun list-to-string (lst)
    (format nil "~{~A~}" lst)
)

(defun create-result(expression)
	(setf result 0)
	(setf flag 0)
	(if (equal '(OP_PLUS) (list (car expression)) ) 
		(progn
			(setq flag 1)
			(setq expression (cdr expression) )
			(loop
				(if (numberp (car expression) )  
					(progn
						(setq result (+ result (car expression) ))  
						(setq expression (cdr  expression)) 
					)
					(progn
						(if (eq (find (car expression) symbolTable :test #'equal) nil )
							(progn 
								(if (equal '(OP_CP) (list (car expression)))
									(progn
										(return t)
									)
									(progn 
										(format t "SYNTAX_ERROR Expression not recognized!!!!!!!" )
										(terpri)
										(format t "Error due to ~a" (car expression))
										(terpri)
										(exit)
									)
								)
							) 
							(progn
								(setf valueOfId  (nth (+ 1 (position (car expression) symbolTable :test #'equal) )  symbolTable) ) 
								(if (find  #\( (list-to-charseq  valueOfId )   :test #'equal)
									(progn 
										(format t "~a is not number" valueOfId) ; exit
										(terpri)
										(exit)
									)

								)
								(setq result (+ result valueOfId )) 
								(setq expression (cdr  expression)) 
							)
						)
					)
				)
				(when (null expression) (return result))
			)  
		)
	) 

	(if (equal '(OP_MINUS) (list (car expression)) ) 
		(progn
			(setq flag 1)
			(setq expression (cdr expression) )
			(if (numberp (car expression)) 
				(setq result (* 2 (car expression) ) )
			)
			(loop
				(if (numberp (car expression))
					(progn
						(setq result (- result (car expression) ))  
						(setq expression (cdr expression) ) 
					)
					(progn
						(if (eq (find (car expression) symbolTable :test #'equal) nil )
							(progn 
								(if (equal '(OP_CP) (list (car expression)))
									(progn
										(return t)
									)
									(progn 									
										(format t "SYNTAX_ERROR Expression not recognized!!!!!!!" )
										(terpri)
										(format t "Error due to ~a" (car expression))
										(terpri)
										(exit)									
									)
								)
							) 
							(progn
								(setf valueOfId  (nth (+ 1 (position (car expression) symbolTable :test #'equal) )  symbolTable) ) 
								(if (find  #\( (list-to-charseq  valueOfId )   :test #'equal)
									(progn 
										(format t "~a is not number" valueOfId) ; exit
										(terpri)
										(return)
									)

								)
								(setq result (- result valueOfId )) 
								(setq expression (cdr  expression)) 
							)
						
						)
					)
				)
				(when (null expression) (return result))
			)  
		)
	) 

	(if (equal '(OP_DIV) (list (car expression)) ) 
		(progn
			(setq flag 1)
			(setf result 1)
			(setq expression (cdr expression) )
			(if (numberp (car expression))
				(setq result (* (car expression) (car expression) ) )
			)
			(loop
				(if (numberp (car expression))
					(progn
						(setq result (/ result (car expression) ))  
						(setq expression (cdr expression)) 
					)
					(progn
						(if (eq (find (car expression) symbolTable :test #'equal) nil )
							(progn 
								(if (equal '(OP_CP) (list (car expression)))
									(progn
										(return t)
									)
									(progn 
										(format t "SYNTAX_ERROR Expression not recognized!!!!!!!" )
										(terpri)
										(format t "Error due to ~a" (car expression))
										(terpri)
										(exit)
									)
								)
							) 
							(progn
								(setf valueOfId  (nth (+ 1 (position (car expression) symbolTable :test #'equal) )  symbolTable) ) 
								(if (find  #\( (list-to-charseq  valueOfId )   :test #'equal)
									(progn 
										(format t "~a is not number" valueOfId) ; exit
										(terpri)
										(exit)
									)

								)
								(setq result (* result valueOfId )) 
								(setq expression (cdr  expression)) 
							)
						
						)
					)
				)
				(when (null expression) (return result))
			)  
		)
	) 

	(if (equal '(OP_MULT) (list (car expression)) ) 
		(progn
			(setq flag 1)
			(setf result 1)
			(setq expression (cdr expression) )
			(loop
				(if (numberp (car expression))
					(progn
						(setq result (* result (car expression)))  
						(setq expression (cdr expression) ) 
					)
					(progn
						(if (eq (find (car expression) symbolTable :test #'equal) nil )
							(progn 
								(if (equal '(OP_CP) (list (car expression)))
									(progn
										(return t)
									)
									(progn 
										(format t "SYNTAX_ERROR Expression not recognized!!!!!!!" )
										(terpri)
										(format t "Error due to ~a" (car expression))
										(terpri)
										(exit)
									)
								)
							) 
							(progn
								(setf valueOfId  (nth (+ 1 (position (car expression) symbolTable :test #'equal) )  symbolTable) ) 
								(if (find  #\( (list-to-charseq  valueOfId )   :test #'equal)
									(progn 
										(format t "~a is not number" valueOfId) ; exit
										(terpri)
										(exit)
									)
								)
								(setq result (/ result valueOfId )) 
								(setq expression (cdr  expression)) 
							)
						)
					)
				)
				(when (null expression) (return result))
			)  
		)
	) 

	(if (equal '(KW_LIST) (list (car expression)) )
		(progn
			(setq expression (cdr expression) )
			(setq result nil)
			(loop
				(if (numberp (car expression))
					(progn
						(setq result (append result (list (car expression) ) )) 
						(setq expression (cdr expression) ) 
					)
					(progn
						(if (equal '(IDENTIFIER) (list (car expression)) )
							(progn 
								(setq result (append result (list (car expression) ) ))
								(setq expression (cdr expression))
							)
							(progn
								(if (equal '(OP_CP) (list (car expression)))
									(progn
										(return t)
									)
									(progn 
										(if (eq nil (car expression) )  
											(progn 
												(car expression)
												(return nil)
											)
											(progn
												(setq result (append result (list (car expression) ) )) ; (list icinde parantezli varsa)
												(setq expression (cdr expression) ) 
											)
										)

									)
								)
							)
						)
					)
				)				

				(when (null expression) (return (car result) )  )
			)

		)
		(progn
			(if (eq flag 0)
				(progn
					(setq result nil)
					(loop
						(if (numberp (car expression))
							(progn
								(setq result (append result (list (car expression) ) )) 
								(setq expression (cdr expression) ) 
							)
							(progn
								(if (equal '(IDENTIFIER) (list (car expression)) )
									(setq expression (cdr expression))
									(progn
										(if (equal '(OP_CP) (list (car expression)))
											(progn
												(return result)
											)
											(progn 
												(car expression)
												(return nil)
											)
										)
									)
								)
							)
						)				
						(when (null expression) (return (car result) )  )
					)
				)
			)
		)
	)

	(if (equal '(KW_APPEND) (list (car expression)) )
		(progn
			(setq expression (cdr expression) )
			(setq result nil)
			(loop			
				(setq result (append result (list (car expression) ) )) 
				(setq expression (cdr expression) )
				(when (null expression) (return (car result) )  )
			)
		)
	)

	(if (equal '(KW_CONCAT) (list (car expression)) )
		(progn
			(setq expression (cdr expression) )
			(setq result nil)
			(loop			
				(setq result (append result (list (car expression) ) )) 
				(setq expression (cdr expression) )
				(when (null expression) (return (car result) )  )
			)
		)
	)

	(if (equal '(KW_AND) (list (car expression)) )
		(progn
			(setq expression (cdr expression) )
			(setq result t)
			(loop			
				(setq result (and result (list (car expression) ) )) 
				(setq expression (cdr expression) )
				(when (null expression) (return (setq result (car result)) )  )
			)
		)
	)

	(if (equal '(KW_OR) (list (car expression)) )
		(progn
			(setq expression (cdr expression) )
			(setq result nil)

			(loop			
				(setq result (or result (list (car expression) ) )) 
				(setq expression (cdr expression) )
				(when (null expression) (return (setq result (car result)) )  )
			)
		)
	)

	(if (equal '(KW_EQUAL) (list (car expression)) )
		(progn
			(setf id (car (cdr expression) ) ) 
			(setf valueOfId (car (cdr (cdr expression))) )
			(if (equal (list-length expression) 3 )  
				(progn
					(if (not  (eq (find id symbolTable :test #'equal) nil ) ) 
						(setq result  (equal (nth (+ 1 (position id symbolTable :test #'equal) )  symbolTable) valueOfId ) ) 
						(setq result (equal id valueOfId) )						
					)
					  
				)
				(progn 
					(format t "EQUAL ICIN ARGUMAN SAYISI 2 OLMALIDIR!" )
					(terpri)
					(exit)
				)
			)
	 	
		)
		(progn 
			(if (equal '(KW_NOT) (list (car expression)) )
				(progn 
					(if (equal (list-length expression) 2 )  
						(progn
							(if (or (equal nil  (car (cdr expression) ) ) (equal 0  (car (cdr expression) ) )) 
								(setq result t)
								(setq result nil)
							)
						)
						(progn
							(format t "NOT ICIN ARGUMAN SAYISI 1 OLMALIDIR!")
							(terpri)
							(exit)
						)
					)
				)
			)
		)
	)

	(if (equal '(KW_SET) (list (car expression)) )
		(progn
			(setq expression (cdr expression))

			(if (eq (find (car expression) symbolTable :test #'equal) nil )
				(progn 
					(setq symbolTable (append symbolTable (list (car expression) ))  ) 
					(setq expression (cdr expression))
					(setq symbolTable (append symbolTable (list (car expression) ))  ) 
					(setq result (car expression))
					(if (eq (car symbolTable) 0 )
						(setq symbolTable (cdr symbolTable))  
					)
				)
				(progn
					(setf (nth (+ 1 (position (car expression) symbolTable :test #'equal) )  symbolTable) (car (cdr expression)) ) 
					(setq expression (cdr expression))
					(setq result (car expression))
				)

			)
		)
	)

	(if (equal '(KW_IF) (list (car expression)) )
		(progn
			(setq expression (cdr expression))
			(if (or (equal (car expression) nil) (equal (car expression) 0) ) 
				(progn 
					(setq expression (cdr expression))
					(setq expression (cdr expression))
					(setq result (car expression))
				)
				(progn
					(setq expression (cdr expression))
					(setq result (car expression))
				)
			) 
		)
	)
	(car (list result)) 
)

(defun gppinterpreter-helper (temp_list)
	(setq flag 0) ; bazi durumlarda gereksiz girmemesi için bir flag yapiyoruz.
	(setq flag-t 0)
	(setq result '())
	(loop ; genel dongu.
	(if (equal #\; (car temp_list)) ; comment icin ozel durum.
		(return '(COMMENT))
	)
	(if (null temp_list)
		(return result)
	)

	(if (or (and (> (char-code (car temp_list)) 64) (< (char-code (car temp_list)) 91) )  
			(and (> (char-code (car temp_list)) 96) (< (char-code (car temp_list)) 123)))   
		(progn ;karakterse bu donguye girer.
			(setq control_ch nil)
			(loop ;karakter oldugu surece devam eder.
				(if (or (and (> (char-code (car temp_list)) 64) (< (char-code (car temp_list)) 91) )
				  		(and (> (char-code (car temp_list)) 96) (< (char-code (car temp_list)) 123)) 
						(and (> (char-code (car temp_list)) 47) (< (char-code (car temp_list)) 58) ) 
				  	)
					(setq control_ch (push (car temp_list) control_ch))
					(progn 
						(if (not  (or  (eq (car temp_list) #\Space) (eq (car temp_list) #\Newline) (eq (car temp_list) #\( ) 
							(eq (car temp_list) #\) )  ) )		
							(progn
								(princ "SYNTAX_ERROR - it cannot be tokenized")
								(terpri)
								(princ "Identifiers contain only combination of alphabetical characters and digits with no leading digit. ")
								(exit)
							)
						)
						(return 1)
					)
				)
				(setq temp_list (cdr temp_list)) 
			)
			(setq control_ch (reverse control_ch)) 
			(if (or (equal '(#\d #\e #\f #\f #\u #\n) control_ch) (equal '(#\D #\E #\F #\F #\U #\N) control_ch) )  ;eger keywordse
				(progn
					(setq result (append result '(KW_DEFFUN)))
					(setq flag 1)
				)	
			)
			(if (or (equal '(#\e #\q #\u #\a #\l) control_ch) (equal '(#\E #\Q #\U #\A #\L) control_ch) ) 
				(progn 
					(setq flag 1)
					(setq result (append result '(KW_EQUAL)))
				)	
			)	
			(if (or (equal '(#\i #\f) control_ch) (equal '(#\I #\F) control_ch) ) 
				(progn 
					(setq flag 1)
					(setq result (append result '(KW_IF)))
				)	
			)	
			(if (or (equal '(#\a #\n #\d) control_ch) (equal '(#\A #\N #\D) control_ch) ) 
				(progn
					(setq flag 1)
					(setq result (append result '(KW_AND)))
				)	
			)	
			(if (or (equal '(#\o #\r) control_ch) (equal '(#\o #\r) control_ch) ) 
				(progn 
					(setq flag 1)
					(setq result (append result '(KW_OR)))
				)	
			)
			(if (or (equal '(#\n #\o #\t) control_ch) (equal '(#\N #\O #\T) control_ch) ) 
				(progn 
					(setq flag 1)
					(setq result (append result '(KW_NOT)))
				)	
			)		
			(if (or (equal '(#\a #\p #\p #\e #\n #\d) control_ch) (equal '(#\A #\P #\P #\E #\N #\D) control_ch) ) 
				(progn 
					(setq flag 1)
					(setq result (append result '(KW_APPEND)))
				)	
			)		
			(if (or (equal '(#\c #\o #\n #\c #\a #\t) control_ch) (equal '(#\C #\O #\N #\C #\A #\T) control_ch) ) 
				(progn 
					(setq flag 1)
					(setq result (append result '(KW_CONCAT)))
				)	
			)	
			(if (or (equal '(#\s #\e #\t) control_ch) (equal '(#\S #\E #\T) control_ch) ) 
				(progn 
					(setq flag 1)
					(setq result (append result '(KW_SET)))
				)	
			)		
			(if (or (equal '(#\f #\o #\r) control_ch) (equal '(#\F #\O #\R) control_ch) ) 
				(progn 
					(setq flag 1)
					(setq result (append result '(KW_FOR)))
				)	
			)
			(if (or (equal '(#\l #\e #\s #\s) control_ch) (equal '(#\L #\E #\S #\S) control_ch) ) 
				(progn 
					(setq flag 1)
					(setq result (append result '(KW_LESS)))
				)	
			)		
			(if (or (equal '(#\n #\i #\l) control_ch) (equal '(#\N #\I #\L) control_ch) ) 
				(progn 
					(setq flag 1)
					(setq result (append result '(KW_NIL)))
				)	
			)		
			(if (or (equal '(#\e #\x #\i #\t) control_ch) (equal '(#\E #\X #\I #\T) control_ch) ) 
				(progn 
					(setq flag 1)
					(setq result (append result '(KW_EXIT)))
				)	
			)
			(if (or (equal '(#\t #\r #\u #\e) control_ch) (equal '(#\T #\R #\U #\E) control_ch) ) 
				(progn 
					(setq flag 1)
					(setq result (append result '(KW_TRUE)))
				)	
			)		
			(if (or (equal '(#\f #\a #\l #\s #\e) control_ch) (equal '(#\F #\A #\L #\S #\E) control_ch) ) 
				(progn 
					(setq flag 1)
					(setq result (append result '(KW_FALSE)))
				)	
			)		
			(if (or (equal '(#\l #\o #\a #\d) control_ch) (equal '(#\L #\O #\A #\D) control_ch) ) 
				(progn 
					(setq flag 1)
					(setq result (append result '(KW_LOAD)))
				)	
			)	
			(if (or (equal '(#\d #\i #\s #\p) control_ch) (equal '(#\D #\I #\S #\P) control_ch) ) 
				(progn 
					(setq flag 1)
					(setq result (append result '(KW_DISP)))
				)	
			)	
			(if (or (equal '(#\l #\i #\s #\t) control_ch) (equal '(#\L #\I #\S #\T) control_ch) ) 
				(progn 
					(setq flag 1)
					(setq result (append result '(KW_LIST)))
				)	
			)	
			(if (equal flag 0) ;flag 0 sa keyword degildir, identifierdir.
				(progn
					(setq result (append result (list  (read-from-string (list-to-string control_ch )) ) ))
				)
				(setq flag 0)
			)	
		)
	)
		(if (and (> (char-code (car temp_list)) 47) (< (char-code (car temp_list)) 58) ) ;integer tanimi
			(if (and (> (char-code (car (cdr temp_list))) 47) (< (char-code (car (cdr temp_list))) 58) ) 
				(progn					; tek basamak ve fazla basamakli integerler icin conditionlar.
					(setq control_num nil)
					(if (eq (car temp_list) #\0) 
						(progn
							(princ "SYNTAX_ERROR - it cannot be tokenized")
							(terpri)
							(princ "Zero(0) should not be leftmost in multiple-digit numbers.")
							(exit)
						)
					)
					(loop
						(if (or (and (> (char-code (car temp_list)) 64) (< (char-code (car temp_list)) 91) )  
								(and (> (char-code (car temp_list)) 96) (< (char-code (car temp_list)) 123)) 
							)  
							(progn
								(princ "SYNTAX_ERROR - it cannot be tokenized")
								(terpri)
								(princ "Digits should not be leftmost in variables.")
								(exit)
							)
						)

						(if (and (> (char-code (car temp_list)) 47) (< (char-code (car temp_list)) 58) ) 
							(progn
								(setq control_num (push (char-to-int (car temp_list))   control_num))
							)
							(return 1)
						)
						(setq temp_list (cdr temp_list)) 
					)
					(setq control_num (list-to-string (reverse control_num)) )
					(setq result (append result (list (parse-integer control_num) ) )) 
				)
				(progn
					(if (or (and (> (char-code (car (cdr temp_list) )) 64) (< (char-code (car (cdr temp_list) )) 91) )  
							(and (> (char-code (car (cdr temp_list) )) 96) (< (char-code (car (cdr temp_list) )) 123)) 
						)  
						(progn
							(princ "SYNTAX_ERROR - it cannot be tokenized")
							(terpri)
							(princ "Digits should not be leftmost in variables.")
							(exit)
						)
					)
					(setq result (append result (list (character-to-int (car temp_list))  )   ))
				)
			)
		) 
		(if (equal #\" (car temp_list)) ; tirnak isareti durumu
			(progn 
				(setf temp_list (cdr temp_list))
				(if (eq flag-t 0)
					(progn
						(setq result (append result '(OP_OC)))
						(setf flag-t 1)
					)
					(progn
						(setq result (append result '(OP_CC)))
						(setf flag-t 0)
					)
				)
			)
		)
		(if (and (> (char-code (car temp_list)) 39) (< (char-code (car temp_list)) 48) ) ;operator tanimi
			(if (not (eq (cdr (cdr temp_list)) nil)  )
				(if (and (equal (char-code (car (cdr temp_list))) 42) (equal (char-code (car (cdr (cdr temp_list)))) 32))  
					(progn
						(setq result (append result (append '(OP_OP) '(OP_MULT)) ))
						(setq temp_list (cdr (cdr temp_list)))  
					)
					(if (equal (char-code (car temp_list)) 45) ; - integerlar icin ozel condition.
						(if (and (> (char-code (car (cdr temp_list) )) 47) (< (char-code (car (cdr temp_list)) ) 58) )
							(progn
								(setq control_ch nil)
								(setq control_ch (push (car temp_list) control_ch))
								(setq temp_list (cdr temp_list))
								(loop
									(if (and (> (char-code (car temp_list)) 47) (< (char-code (car temp_list)) 58) ) 
										(setq control_ch (push (car temp_list) control_ch))
										(return 1)
									)
									(setq temp_list (cdr temp_list)) 
								)
								(setq control_ch (reverse control_ch))
								(setq result (append result control_num  )) 
							)
							(progn							
								(setq result (append result '(OP_MINUS))) 
							) 
						)
						(progn ; - veya ** olmayanlar icin condition.
							(if (equal #\+ (car temp_list))
								(progn 
									(setq result (append result '(OP_PLUS))) 
								)
							)
							(if (equal #\/ (car temp_list))
								(progn 
									(setq result (append result '(OP_DIV))) 
								)
							)	
							(if (equal #\* (car temp_list))
								(progn 
									(setq result (append result '(OP_MULT))) 
								)
							)	
							(if (equal #\( (car temp_list))
								(progn 
									(setq result (append result '(OP_OP))) 
								)
							)	
							(if (equal #\) (car temp_list))
								(progn 
									(setq result (append result '(OP_CP))) 								
								)
							)
							(if (equal #\, (car temp_list))
								(progn 
									(setq result (append result '(OP_COMMA))) 																	
								)
							)	
							(if (equal #\“ (car temp_list))
								(progn 
									(setq result (append result '(OP_OC))) 																	
								)
							)	
							(if (equal #\” (car temp_list))
								(progn 
									(setq result (append result '(OP_CC))) 																										
								)
							)	
						)
					)
				)
					(progn ; - veya ** olmayanlar icin condition.
						(if (equal #\+ (car temp_list))
							(progn 
								(setq result (append result '(OP_PLUS))) 
							)
						)
						(if (equal #\/ (car temp_list))
							(progn 
								(setq result (append result '(OP_DIV))) 
							)
						)	
						(if (equal #\* (car temp_list))
							(progn 
								(setq result (append result '(OP_MULT)))
							)
						)	
						(if (equal #\( (car temp_list))
							(progn  
								(setq result (append result '(OP_OP)))
							)
						)	
						(if (equal #\) (car temp_list))
							(progn 
								(setq result (append result '(OP_CP)))
							)
						)
						(if (equal #\, (car temp_list))
							(progn  
								(setq result (append result '(OP_COMMA)))
							)
						)	
						(if (equal #\“ (car temp_list))
							(progn  
								(setq result (append result '(OP_OC)))
							)
						)	
						(if (equal #\” (car temp_list))
							(progn  
								(setq result (append result '(OP_CC)))
							)
						)	
						(if (equal #\" (car temp_list))
							(progn 
								(setq result (append result '(OP_C)))
							)
						)
					)
			)
		) 
		(setq temp_list (cdr temp_list))
		(when (null temp_list)  (return result) )
	)
)

(gppinterpreter "input.txt")
(format t "\"input.txt\" dosyası otomatik olarak calistirildi.
Expressionlari terminalden girmek isterseniz, lutfen asagidaki yolu izleyin;
=> clisp => (load \"gpp_interpreter.lisp\") => (gppinterpreter)
Ayrica load edildikten sonra (gppinterpreter \"input.txt\") seklinde de kullanilabilir.")

