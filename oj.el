;;onlie judge systems
(require 'webclient)
(require 'ido)

(setq url-proxy-services '(("http" . "localhost:8888")))
(setq debug-on-error t)

(defvar oj-hook nil "hook for do something before request")

(defun file-to-string (file)
  "Read the content of FILE and return it as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun exec-action(site-symbol action)
  "dynamic request site"
  (interactive "sEnter site name: \nsEnter action ")
  (run-hook-with-args 'oj-hook site-symbol action)
  (request-site (symbol-value (intern site-symbol)) (intern action)))

;;;;;;;;;;;;poj
(defvar poj (make-wc-site :url "http://poj.org" :forms '(
						    (login .
							   (make-wc-form 
							    :action "login"
							    :fields `(
								      (user_id1 . "your-name")
								      (password1 . ,(read-passwd "input your password: "))
								      (url . "%2F")
								      (B1 . "login"))))
						    (submit .
							    (make-wc-form
							     :action "submit"
							     :fields `(
								       (problem_id ,(read-string "inpur problem id: "))
								       (language ,(read-string "select language: 0:G++,1:GCC,2:Java,3:Pascal,4:C++,5:C,6:Fortan "))
								       (source ,(file-to-string (read-file-name "select source file")))
								       (submit "Submit"))))
						    (logout .
							    (make-wc-form
							     :action "login?action=logout&url=%2F"
							     :method "GET")))))

;;;;;;;;;;;;spoj


(setq spoj-langmap '(
		       ( "ADA 95 (gnat 4.3.2)" "7" )
		       ( "Assembler (nasm 2.03.01)" "13" )
		       ( "Awk (gawk-3.1.6)" "104" )
		       ( "Bash (bash-4.0.37)" "28" )
		       ( "Brainf**k (bff 1.0.3.1)" "12" )
		       ( "C (gcc 4.3.2)" "11" )
		       ( "C# (gmcs 2.0.1)" "27" )
		       ( "C++ (g++ 4.3.2)" "41" )
		       ( "C++ (g++ 4.0.0-8)" "1" )
		       ( "C99 strict (gcc 4.3.2)" "34" )
		       ( "Clips (clips 6.24)" "14" )
		       ( "Clojure (clojure 1.1.0)" "111" )
		       ( "Common Lisp (sbcl 1.0.18)" "31" )
		       ( "Common Lisp (clisp 2.44.1)" "32" )
		       ( "D (gdc 4.1.3)" "20" )
		       ( "Erlang (erl 5.6.3)" "36" )
		       ( "F# (fsharp 2.0.0)" "124" )
		       ( "Fortran 95 (gfortran 4.3.2)" "5" )
		       ( "Go (gc 2010-07-14)" "114" )
		       ( "Haskell (ghc 6.10.4)" "21" )
		       ( "Icon (iconc 9.4.3)" "16" )
		       ( "Intercal (ick 0.28-4)" "9" )
		       ( "JAR (JavaSE 6)" "24" )
		       ( "Java (JavaSE 6)" "10" )
		       ( "JavaScript (rhino 1.7R1-2)" "35" )
		       ( "Lua (luac 5.1.3)" "26" )
		       ( "Nemerle (ncc 0.9.3)" "30" )
		       ( "Nice (nicec 0.9.6)" "25" )
		       ( "Node.js (0.8.11)" "56" )
		       ( "Ocaml (ocamlopt 3.10.2)" "8" )
		       ( "Pascal (fpc 2.2.4)" "22" )
		       ( "Pascal (gpc 20070904)" "2" )
		       ( "Perl (perl 5.12.1)" "3" )
		       ( "Perl 6 (rakudo-2010.08)" "54" )
		       ( "PHP (php 5.2.6)" "29" )
		       ( "Pike (pike 7.6.112)" "19" )
		       ( "Prolog (swipl 5.6.58)" "15" )
		       ( "Python (python 2.7)" "4" )
		       ( "Python 3 (python 3.2.3)" "116" )
		       ( "Python 3 nbc (python 3.2.3 nbc)" "126" )
		       ( "Ruby (ruby 1.9.3)" "17" )
		       ( "Scala (scala 2.8.0)" "39" )
		       ( "Scheme (guile 1.8.5)" "33" )
		       ( "Scheme (stalin 0.11)" "18" )
		       ( "Sed (sed-4.2)" "46" )
		       ( "Smalltalk (gst 3.0.3)" "23" )
		       ( "Tcl (tclsh 8.5.3)" "38" )
		       ( "Text (plain text)" "62" )
		       ( "Whitespace (wspace 0.3)" "6" )
		       ))

(setq spoj (make-wc-site :url "http://www.spoj.com" :forms '(
							    (login .
								   (make-wc-form
								    :action "http://www.spoj.com/"
								    :fields `(
									      (login_user . "your-name")
									      (password . ,(read-passwd "input yout passwd: "))
									      (autologin . "1")
									      (submit . "Log In"))))
							    (submit .
								    (make-wc-form
								     :action "submit/complete/"
								     :fields `(
									       (problemcode . ,(read-string "problem code: "))
									       (file . ,(file-to-string (read-file-name "select source file")))
									       (lang ,(nth 1 (assoc (ido-completing-read "select language " spoj-langmap) spoj-langmap)))
									       (subm_file "")
									       (submit "Send"))))
							    (logout .
								    (make-wc-form
								     :action "logout"
								     :method "GET"))
							    )))

;;;;;;;;;;;;;;;;;;;;;UVa
(defvar *oj-uva-hiddens* nil "login form hidden field")

(setq uva (make-wc-site :url "http://uva.onlinejudge.org" :forms '(
								   (prepare . ;; fetch hidden fields
									    (make-wc-form
									     :action ""
									     :method "GET"))
								   (login .
									  (make-wc-form
									   :action "index.php?option=com_comprofiler&task=login"
									   :fields `(
										     (username . "your-name")
										     (passwd . ,(read-passwd "input your passwd: "))
										     (remember . "yes")
										     ,@*oj-uva-hiddens*
										     (Submit . "Login"))))
								   (submit .
									   (make-wc-form
									    :action "index.php?option=com_onlinejudge&Itemid=25&page=save_submission"
									    :fields `(
										      (localid . ,(read-string "input problem id: "))
										      (language ,(read-string "select language:1 ANSI C 4.5.3, 2 JAVA 1.6, 3 C++ 4.5.3, 4 PASCAL 2.4 "))
										      (code ,(file-to-string (read-file-name "select source file")))
										      (submit "Submit"))))
								   (logout .
									   (make-wc-form
									    :action "index.php?option=logout"
									    :fields `(
										      (op2 . "logout")
										      (return . "http://uva.onlinejudge.org")
										      (lang "english")
										      (message "0")
										      (Submit "Logout"))))
											  
								   )))


(defun handle-hiddens(data header status)
  (if (= status 200)
      (let ((pattern-form-begin "<form.*id=\"mod_loginform\"")
	    (pattern-form-end "/form>")
	    (pattern-field "<input\s+type=\"hidden\"\s+name=\"\\(\[^\"\]+\\)\"\s+value=\"\\(\[^\"\]+\\)\"")
	    form-begin
	    form-end
	    )
	(setq *oj-uva-hiddens* nil);clear it
	(with-temp-buffer
	  (insert data)
	  (goto-char (point-min))
	  (setq form-begin (re-search-forward pattern-form-begin))
	  (setq form-end (re-search-forward pattern-form-end))
	  (goto-char form-begin)
	  (while (re-search-forward pattern-field form-end :no-error)
	    (add-to-list '*oj-uva-hiddens*
			 (list 
			  (make-symbol (match-string-no-properties 1))
			  (match-string-no-properties 2)))))
	*oj-uva-hiddens*)))


(defun uva-handler(site action)
  (message "uva handler")
  (if (string= site "uva") 
      (cond 
	((string= action "login")
	 (unless *oj-uva-hiddens*
	   (request-site uva 'prepare 'handle-hiddens)))
	((string= action "logout")
	 (setq *oj-uva-hiddens* nil)))))      

(add-hook 'oj-hook 'uva-handler)

;;;;;;;;;;;;;;;;;;;;;;;;zero judge
(setq zerojudge (make-wc-site :url "http://zerojudge.tw" :forms '(
								  (login .
								   (make-wc-form
								    :action "Login"
								    :fields `(
									      (account "ifree")
									      (passwd ,(read-passwd "input your passwd: "))
									      (returnPage "/Problems")
									      (submit "Submit"))))
								  (logout .
								   (make-wc-form
								    :action "Logout"
								    :method "GET"))
								  (submit .
								   (make-wc-form
								    :action "SubmitCode?"								    
								    :fields `(
									      (problemid ,(read-string "problem id? "))
									      (language ,(read-string "select language:C, JAVA, CPP,PASCAL"))
									      (code ,(file-to-string (read-file-name "select source file"))))
								    :multipart t
								    :files nil))								   								  
								  )))
