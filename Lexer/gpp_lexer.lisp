; Fatih DURAL
; 151044041

; (load "gpp_lexer.lisp") komutu ile dosya yuklenir.
; (gppinterpreter filename) -> Ornek: (gppinterpreter "input.txt") ile program calistirilir. filename argumani opsiyoneldir.
; gppinterpreter fonksiyonu filename alirsa, o dosyayi okur, lexical analiz yapar ve gerektigi gibi ekrana basar.
; gppinterpreter fonksiyonu argumansiz cagilirsa -> (gppinterpreter), kullanicidan girdi beklenir.
; argumansiz halde girdi yazilir ve enter'a basilir, cikti gerektigi gibi uretilir, bu halde ciktidan sonra ekrana girdi verilmeye devam edilebilir.
; Syntax Error durumunda tamamen programdan cikar.

(defun gppinterpreter(&optional file) ; opsiyonel argumanli ana fonksiyon.
 	(if (eq file nil)  ; arguman yoksa, dosya ismi verilmemisse, surekli olarak verilen girdi okunur.
 		(progn
		 	(loop
				(setf readable (read-line))
				(setf readable (list-to-charseq readable) )
				(setf readable  (reverse (cdr (reverse (cdr readable)))) )
 				(gppinterpreter-helper readable) ; icerik yardimci fonksiyona verilir.
			)
 		)
 		(progn 		; arguman olarak dosya ismi verilirse, dosya okunur, yardimci fonksiyona icerik verilir.
 			(setq temp_list nil)
			(let ((in (open file :if-does-not-exist nil))) ; dosya sonuna kadar karakter karakter okunur.
				(when in
				(loop for line = (read-char in nil)
			        while line do (setq temp_list (push line temp_list)))
			    (close in))) ; dosya kapanir.
				(setq temp_list (reverse temp_list)) 
				(gppinterpreter-helper temp_list)
				(return-from gppinterpreter "FINISH")
 		)
 	)
)

(defun list-to-charseq (text)	; girdi liste oldugundan gerekli donusum yapilir.
	(setf text (format nil "~s" text) ) ; list to string
  	(assert (stringp text) (text) "~s :string") ; string to char sequence
  	(setf text (coerce text 'list) )
)

(defun gppinterpreter-helper (temp_list)
	(setq flag 0) ; bazi durumlarda gereksiz girmemesi için bir flag yapiyoruz.
	(setq flag-t 0)
	(loop ; genel dongu.
	(if (equal #\; (car temp_list)) ; comment icin ozel durum.
		(progn 
			(loop
				(if (equal #\Newline (car temp_list))
					(progn
						(setf temp_list (cdr temp_list))
						(return 1)
					)
				)
				(setf temp_list (cdr temp_list))
				(when (null temp_list) (return 1) )
			)
			(princ "COMMMENT")
			(terpri)
		)	
	)
	(if (null temp_list)
		(return 0)
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
					(princ "KW_DEFFUN")
					(terpri)  
					(setq flag 1)
				)	
			)
			(if (or (equal '(#\e #\q #\u #\a #\l) control_ch) (equal '(#\E #\Q #\U #\A #\L) control_ch) ) 
				(progn 
					(setq flag 1)
					(princ "KW_EQUAL")
					(terpri)
				)	
			)	
			(if (or (equal '(#\i #\f) control_ch) (equal '(#\I #\F) control_ch) ) 
				(progn 
					(setq flag 1)
					(princ "KW_IF")
					(terpri)									
				)	
			)	
			(if (or (equal '(#\a #\n #\d) control_ch) (equal '(#\A #\N #\D) control_ch) ) 
				(progn 
					(setq flag 1)
					(princ "KW_AND")
					(terpri)					
				)	
			)	
			(if (or (equal '(#\o #\r) control_ch) (equal '(#\o #\r) control_ch) ) 
				(progn 
					(setq flag 1)
					(princ "KW_OR")
					(terpri)				
				)	
			)
			(if (or (equal '(#\n #\o #\t) control_ch) (equal '(#\N #\O #\T) control_ch) ) 
				(progn 
					(setq flag 1)
					(princ "KW_NOT")
					(terpri)					
				)	
			)		
			(if (or (equal '(#\a #\p #\p #\e #\n #\d) control_ch) (equal '(#\A #\P #\P #\E #\N #\D) control_ch) ) 
				(progn 
					(setq flag 1)
					(princ "KW_APPEND")
					(terpri)	
				)	
			)		
			(if (or (equal '(#\c #\o #\n #\c #\a #\t) control_ch) (equal '(#\C #\O #\N #\C #\A #\T) control_ch) ) 
				(progn 
					(setq flag 1)
					(princ "KW_CONCAT")
					(terpri)
				)	
			)	
			(if (or (equal '(#\s #\e #\t) control_ch) (equal '(#\S #\E #\T) control_ch) ) 
				(progn 
					(setq flag 1)
					(princ "KW_SET")
					(terpri)				
				)	
			)		
			(if (or (equal '(#\f #\o #\r) control_ch) (equal '(#\F #\O #\R) control_ch) ) 
				(progn 
					(setq flag 1)
					(princ "KW_FOR")
					(terpri)
				)	
			)
			(if (or (equal '(#\l #\e #\s #\s) control_ch) (equal '(#\L #\E #\S #\S) control_ch) ) 
				(progn 
					(setq flag 1)
					(princ "KW_LESS")
					(terpri)
				)	
			)		
			(if (or (equal '(#\n #\i #\l) control_ch) (equal '(#\N #\I #\L) control_ch) ) 
				(progn 
					(setq flag 1)
					(princ "KW_NIL")
					(terpri)
				)	
			)		
			(if (or (equal '(#\e #\x #\i #\t) control_ch) (equal '(#\E #\X #\I #\T) control_ch) ) 
				(progn 
					(setq flag 1)
					(princ "KW_EXIT")
					(terpri)
				)	
			)
			(if (or (equal '(#\t #\r #\u #\e) control_ch) (equal '(#\T #\R #\U #\E) control_ch) ) 
				(progn 
					(setq flag 1)
					(princ "KW_TRUE")
					(terpri)
				)	
			)		
			(if (or (equal '(#\f #\a #\l #\s #\e) control_ch) (equal '(#\F #\A #\L #\S #\E) control_ch) ) 
				(progn 
					(setq flag 1)
					(princ "KW_FALSE")
					(terpri)
				)	
			)		
			(if (or (equal '(#\l #\o #\a #\d) control_ch) (equal '(#\L #\O #\A #\D) control_ch) ) 
				(progn 
					(setq flag 1)
					(princ "KW_LOAD")
					(terpri)
				)	
			)	
			(if (or (equal '(#\d #\i #\s #\p) control_ch) (equal '(#\D #\I #\S #\P) control_ch) ) 
				(progn 
					(setq flag 1)
					(princ "KW_DISP")
					(terpri)
				)	
			)	
			(if (or (equal '(#\l #\i #\s #\t) control_ch) (equal '(#\L #\I #\S #\T) control_ch) ) 
				(progn 
					(setq flag 1)
					(princ "KW_LIST")
					(terpri)
				)	
			)	
			(if (equal flag 0) ;flag 0 sa keyword degildir, identifierdir.
				(progn
					(princ "IDENTIFIER")
					(terpri)
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
							(setq control_num (push (car temp_list) control_num))
							(return 1)
						)
						(setq temp_list (cdr temp_list)) 
					)
					(setq control_num (reverse control_num))
					(princ "VALUE")
					(terpri)
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
					(princ "VALUE")
					(terpri)
				)
			)
		) 
		(if (equal #\" (car temp_list)) ; tirnak isareti durumu
			(progn 
				(setf temp_list (cdr temp_list))
				(if (eq flag-t 0)
					(progn
						(princ "OP_OC")
						(setf flag-t 1)
					)
					(progn
						(princ "OP_CC")
						(setf flag-t 0)
					)
				)
				(terpri)
			)
		)
		(if (and (> (char-code (car temp_list)) 39) (< (char-code (car temp_list)) 48) ) ;operator tanimi
			(if (not (eq (cdr (cdr temp_list)) nil)  )
				(if (and (equal (char-code (car (cdr temp_list))) 42) (equal (char-code (car (cdr (cdr temp_list)))) 32))  
					(progn
						(princ "OP_DBLMULT")
						(terpri)
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
								(princ "VALUE")
								(terpri)
							)
							(progn							
								(princ "OP_MINUS")
								(terpri)
							) 
						)
						(progn ; - veya ** olmayanlar icin condition.
							(if (equal #\+ (car temp_list))
								(progn 
									(princ "OP_PLUS")
									(terpri)
								)
							)
							(if (equal #\/ (car temp_list))
								(progn 
									(princ "OP_DIV")
									(terpri)
								)
							)	
							(if (equal #\* (car temp_list))
								(progn 
									(princ "OP_MULT")
									(terpri)
								)
							)	
							(if (equal #\( (car temp_list))
								(progn 
									(princ "OP_OP")
									(terpri)
								)
							)	
							(if (equal #\) (car temp_list))
								(progn 
									(princ "OP_CP") 
									(terpri)
								)
							)
							(if (equal #\, (car temp_list))
								(progn 
									(princ "OP_COMMA")
									(terpri)
								)
							)	
							(if (equal #\“ (car temp_list))
								(progn 
									(princ "OP_OC")
									(terpri)
								)
							)	
							(if (equal #\” (car temp_list))
								(progn 
									(princ "OP_CC")
									(terpri)
								)
							)	
						)
					)
				)
					(progn ; - veya ** olmayanlar icin condition.
						(if (equal #\+ (car temp_list))
							(progn 
								(princ "OP_PLUS")
								(terpri)
							)
						)
						(if (equal #\/ (car temp_list))
							(progn 
								(princ "OP_DIV")
								(terpri)
							)
						)	
						(if (equal #\* (car temp_list))
							(progn 
								(princ "OP_MULT")
								(terpri)
							)
						)	
						(if (equal #\( (car temp_list))
							(progn  
								(princ "OP_OP")
								(terpri)
							)
						)	
						(if (equal #\) (car temp_list))
							(progn 
								(princ "OP_CP") 
								(terpri)
							)
						)
						(if (equal #\, (car temp_list))
							(progn  
								(princ "OP_COMMA")
								(terpri)
							)
						)	
						(if (equal #\“ (car temp_list))
							(progn  
								(princ "OP_OC")
								(terpri)
							)
						)	
						(if (equal #\” (car temp_list))
							(progn  
								(princ "OP_CC")
								(terpri)
							)
						)	
						(if (equal #\" (car temp_list))
							(progn 
								(princ "OP_C")
								(terpri)
							)
						)
					)
			)
		) 
		(setq temp_list (cdr temp_list))
		(when (null temp_list)  (return 1) )
	)
)