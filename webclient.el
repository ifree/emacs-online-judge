;;a simple emacs client
;;
;;DONE: follow redirect
;;TODO: validator
;;TODO: html parser
;;
(require-package 'http-post-simple) 
(require 'http-post-simple)

(eval-when-compile (require 'cl))

(defconst wc-max-redirect 5)

(defstruct wc-form action (method "POST") fields (multipart nil) files)

(defstruct wc-site url forms)

(defun http-get-simple(url)
  		  (let (header data status)
		    (with-current-buffer 
			(url-retrieve-synchronously url)
		      (setq status url-http-response-status)
		      (goto-char (point-min))
		      (if (search-forward-regexp "^$" nil t)
			  (setq header (buffer-substring (point-min) (point))
				data   (buffer-substring (1+ (point)) (point-max)))
			(setq data (buffer-string)))
		      (kill-buffer (current-buffer)))
		      (values data header status)))


(defun request-site(site form &optional handler max-redirect)
  "perform a request to site"
  (let* ((url (wc-site-url site))
	 (_form (cdr (assoc form (wc-site-forms site))))
	 (form-inst (eval _form))
	 (action-parser (lambda (action) 
			  (if (string-prefix-p "http" action)
			      action
			    (concat url "/" action))))
	 (result (if (string= (wc-form-method form-inst) "POST")
		     (if (wc-form-multipart form-inst)
			 (http-post-simple-multipart (funcall action-parser (wc-form-action form-inst))
					   (wc-form-fields form-inst) (wc-form-files form-inst) t)
		       (http-post-simple (funcall action-parser (wc-form-action form-inst))
					 (wc-form-fields form-inst)))
		   (http-get-simple (funcall action-parser (wc-form-action form-inst)))))
	 (_max-redirect (or max-redirect wc-max-redirect)))
    ;;follow redirect
    
    (if (and (> _max-redirect 0) (or (= (nth 2 result) 302) (= (nth 2 result) 301)));;TODO:handle html meta refresh
	(progn
	  ;Location: http://poj.org/%2F
	  (save-match-data
	  (if (string-match "Location:\s\\(.+$\\)" (nth 1 result))
	      (setq result (http-get-simple (match-string-no-properties 1 (nth 1 result))))
	      ))))

    (if handler
	(apply handler result)
      result)))


;(let*
;    ((form (cdr (assoc 'submit (site-forms poj))))     
;     (frm-inst (apply (car form) (cdr form)))
;     (fileds (nth 1(nth 4 form))))
;   form
;   (backquote ,(car fileds))
;  (eval form)
;  )



(provide 'webclient)
