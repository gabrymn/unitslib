(defparameter *units*
  '(
    (|kg|    kilogram         |kg|                       :base    1)
    (|m|     metre            |m|                        :base    2)
    (|s|     second           |s|                        :base    3)
    (|A|     Ampere           |A|                        :base    4)
    (|K|     Kelvin           |K|                        :base    5)
    (|cd|    candela          |cd|                       :base    6)
    (|mol|   mole             |mol|                      :base    7)
    (|Bq|    Becquerel        (expt |s| -1)              :derived 8)
    (|dc|    degreecelsius    |K|                        :derived 9)
    (|C|     Coulomb          (* |A| |s|)                :derived 10)
    (|F|     Farad            (/ |C| |V|)                :derived 11)  
    (|Gy|    Gray             (* |J| (expt |kg| -1))     :derived 12)
    (|Hz|    Hertz            (expt |s| -1)              :derived 13)
    (|H|     Henry            (* |V| |s| (expt |A| -1))  :derived 14)
    (|J|     Joule            (* |m| |N|)                :derived 15)
    (|kat|   Katal            (* |mol| (expt |s| -1))    :derived 16)
    (|lm|    lumen            (* |cd| |sr|)              :derived 17)
    (|lx|    lux              (* |lm| (expt |m| -2))     :derived 18)
    (|N|     Newton           (* |kg| |m| (expt |s| -2)) :derived 19)
    (|Omega| ohm              (* |V| (expt |A| -1))      :derived 20)
    (|Pa|    Pascal           (* |N| (expt |m| -2))      :derived 21)
    (|rad|   radian           1                          :derived 22)
    (|S|     Siemens          (expt |Omega| -1)          :derived 23)
    (|Sv|    Sievert          (* |J| (expt |kg| -1))     :derived 24)
    (|sr|    steradian        1                          :derived 25)
    (|T|     Tesla            (* |Wb| (expt |m| -2))     :derived 26)
    (|V|     Volt             (* |J| (expt |C| -1))      :derived 27)
    (|W|     Watt             (* |J| (expt |s| -1))      :derived 28)
    (|Wb|    Weber            (* |V| |s|)                :derived 29)
  )
)

;;; per accedere più facilmente ai singoli elementi: 
;;; per esempio (nth +name+ (third *units*)) è più leggibile
;;; rispetto a (nth 1 (third *units*))
(defconstant +symbol+ 0)
(defconstant +name+ 1)
(defconstant +expression+ 2)
(defconstant +type+ 3)
(defconstant +id+ 4)


(defparameter *prefixes*
  '((|Y| . 1000000000000000000000000) 
    (|Z| . 1000000000000000000000)    
    (|E| . 1000000000000000000)      
    (|P| . 1000000000000000)        
    (|T| . 1000000000000)            
    (|G| . 1000000000)                
    (|M| . 1000000)                   
    (|k| . 1000)
    (|h| . 100)
    (|da| . 10)
    (|d| . 1/10)
    (|c| . 1/100)
    (|m| . 1/1000)
    (|u| . 1/1000000)
    (|n| . 1/1000000000)
    (|p| . 1/1000000000000)
    (|f| . 1/1000000000000000)
    (|a| . 1/1000000000000000000)
    (|z| . 1/1000000000000000000000)
    (|y| . 1/1000000000000000000000000)))

(defun prefix-names ()
	(mapcar #'car *prefixes*))

(defun prefix-free (U)
  (cond 
    ((si-unit-name U) U)
    (t 
     (let ((prefix-value (prefix-match U)))
       (if prefix-value
           (reduce-symbol (len-symbol prefix-value) U)
           U)))))
           
(defun prefix-free-p (symbol)
	(null (prefix-match symbol)))

(defun len-symbol (symbol)
	(length (symbol-name symbol))
)

; ---- MANCA ----
(defun prefix-symbol (prefix-symbol target-symbol &key (test #'char=))
  "Se PREFIX-SYMBOL è un prefisso del nome di TARGET-SYMBOL, restituisce il simbolo del prefisso; altrimenti NIL."
  (let* ((prefix-str (symbol-name prefix-symbol))
         (target-str (symbol-name target-symbol))
         (plen (length prefix-str))
         (tlen (length target-str)))
    (if (and (<= plen tlen)
             (every test prefix-str target-str))
        (intern prefix-str) ; restituisce il prefisso come simbolo
        nil)))

; ---- MANCA ----
(defun prefix-match (symbol &optional (prefixi (prefix-names)))
  "Versione ricorsiva: restituisce il primo prefisso che è prefisso di SYMBOL, usando prefix-symbol."
  (cond
    ((null prefixi) nil) ; fine lista, nessun match
    (t
     (let ((result (prefix-symbol (car prefixi) symbol)))
       (if result
           result
           (prefix-match symbol (cdr prefixi)))))))



; ---- MANCA ----
(defun reduce-symbol (n simbolo)
  "Rimuove i primi N caratteri dal nome di SIMBOLO e restituisce un nuovo simbolo."
  (let* ((nome (symbol-name simbolo))
         (lunghezza (length nome))
         (nuovo-nome (if (<= n lunghezza)
                         (subseq nome n)
                         "")))  ; se n > lunghezza ritorna stringa vuota
    (intern nuovo-nome))
)



(defun is-quantity (Q)
	(and (listp Q)
		(eq 'q (first Q))
			(let ((n (second Q)) (d (third Q)))
					(and (numberp n)
					(is-dimension d)))))

(defun unit-symbols ()
  (mapcar (function first) *units*)
)


(defun is-si-unit (S &optional (units *units*))
  (cond
    ((null units) nil)
    ((eq S (nth +symbol+ (first units))) T)
    (t (is-si-unit S (rest units)))
  )
)

(defun is-base-si-unit (S &optional (units *units*))
	(cond
		((null units) nil)
		((and (eq S (nth +symbol+ (first units))) 
		 (eq :base (nth +type+ (first units)))) 
		 	T
		)
		(t (is-base-si-unit S (rest units)))
	)
)

; ---- MANCA ----
(defun is-dimension (d)
  (let ((symbols (unit-symbols)))
    (cond
      ;;; simbolo base riconosciuto
      ((symbolp d) (not (null (member d symbols))))
      
      ;;; prodotto: lista che inizia con '*'
      ((and (listp d)
            (eq '* (first d)))
       (every (function is-dimension) (rest d)))
      
      ;;; esponente: lista che inizia con 'expt'
      ((and (listp d)
            (eq 'expt (first d))
            (= (length d) 3))
       (and (is-dimension (second d))
            (numberp (third d))))
      
      ;;; altrimenti non è una dimensione valida
      (t nil)))
)

(defun si-unit-name (s &optional (units *units*))
	(cond
		((null units) nil) 
		((eq s (nth +symbol+ (first units))) 
			(nth +name+ (first units))
		)
		(t (si-unit-name s (rest units)))
	)
)

(defun si-unit-symbol (n &optional (units *units*))
	(cond
		((null units) nil) 
		((equal n (nth +name+ (first units))) (nth +symbol+ (first units)))
		(t (si-unit-symbol n (rest units)))
	)
)

(defun si-unit-base-expansion (s &optional (units *units*))
	(cond
		((null units) nil) 
		((eq s (nth +symbol+ (first units))) (nth +expression+ (first units)))
		(t (si-unit-base-expansion s (rest units)))
	)
)

(defun si-unit-id (s &optional (units *units*))
	(cond
		((null units) nil) 
		((eq s (nth +symbol+ (first units))) (nth +id+ (first units)))
		(t (si-unit-id s (rest units)))
	)
)

; ---- MANCA ----
(defun compare-units (u1 u2)
  (let ((id1 (si-unit-id (prefix-free u1)))
        (id2 (si-unit-id (prefix-free u2))))
    (cond
      ((or (null id1) (null id2))
       (error "Unità non trovata"))
      ((< id1 id2) '<)
      ((> id1 id2) '>)
      (t '=))))

; ---- MANCA ----
(defun flatten-dim (dim)
  "Ritorna una lista piatta di termini, espandendo prodotti."
  (cond
    ((symbolp dim) (list (list dim 1))) ; esponente implicito 1
    ((and (listp dim)
          (eq '* (first dim)))
     (mapcan #'flatten-dim (rest dim)))
    ((and (listp dim)
          (eq 'expt (first dim))
          (= (length dim) 3))
     (let ((unit (second dim))
           (exp (third dim)))
       (list (list unit exp))))
    (t (error "Dimensione non valida ~a" dim))))


; ---- MANCA ----
(defun upd-exp (unit exp acc)
  "Aggiorna la lista acc sommando exp all'esponente di unit se presente."
	(cond
		((null acc)
			(if (= exp 0) nil (list (list unit exp)))
		)
		((eq unit (caar acc))
			(let ((new-exp (+ exp (cadar acc))))
				(if (= new-exp 0)
				(cdr acc)
				(cons (list unit new-exp) (cdr acc)))
			)
		)
		(t
			(cons (car acc)
				(upd-exp unit exp (cdr acc))
			)
		)
	)
)

; ---- MANCA ----
(defun agg-exp (terms acc)
	"Aggrega ricorsivamente i termini (unit exp) in acc."
	(if (null terms)
		acc
		(agg-exp (cdr terms)
			(upd-exp (car (car terms)) (car (cdr (car terms))) acc)
		)
	)
)

; ---- MANCA ----
(defun sum-exp (terms)
  "Somma gli esponenti di unità uguali nella lista terms."
  (agg-exp terms nil))

; ---- MANCA ----
(defun aggregate-exponents (terms)
  "Data una lista di (unit exp), somma gli esponenti di unità uguali."
  (let ((table (make-hash-table :test #'eq)))
    (dolist (term terms)
      (let ((unit (first term))
            (exp (second term)))
        (setf (gethash unit table) (+ (gethash unit table 0) exp))))
    (remove-if (lambda (ue) (= (second ue) 0))
               (loop for k being the hash-keys of table
                     collect (list k (gethash k table))))))

; ---- MANCA ----

(defun norm (dim)
    
	(let* 
		((terms (flatten-dim dim))
		 (agg (aggregate-exponents terms))
		 (sorted (sort agg #'(lambda (a b)
		 	(eq (compare-units (first a) (first b)) '<))))
		 
		 (build-term 
			(lambda (ue)
				(let ((unit (first ue))
					(exp (second ue)))
					(if (= exp 1)
					unit
					(list 'expt unit exp))))))
			
			(cond
				((null sorted) 1) 
				((= (length sorted) 1) (funcall build-term (first sorted)))
				(t (cons '* (mapcar build-term sorted)))
			))
)

(defun q (N D)
	(list 
		'q 
		(* N (extract-numeric-factor (norm D))) 
		(remove-prefixes D))
)

(defun q-val (Q)
	(if (is-quantity Q) (second Q) NIL)
)

(defun q-dim (Q)
	(if (is-quantity Q) (third Q) NIL) 
)


(defun qnegate (Q)
	(unless (is-quantity Q)
		(error "ERROR: Invalid argument"))
	(q (- (q-val Q)) (q-dim Q)) 
)


(defun qinverse (Q)
	(unless (is-quantity Q)
		(error "ERROR: Invalid argument"))
	(q (/ 1 (q-val Q)) (norm (list 'expt (q-dim Q) -1))) 
)


(defun qadd (Q1 Q2)

	(unless (and (is-quantity Q1) (is-quantity Q2))
		(error "ERROR: At least one of the two arguments is not a valid quantity"))

	(if (equal (q-dim Q1) (q-dim Q2))

		(let ((res (q (+ (q-val Q1) (q-val Q2)) (q-dim Q1) )))
			(if (is-quantity res)
			res
			(error "ERROR: The result is not a valid quantity ~A" res)))
		
		(error "ERROR: Incompatible dimensions ~A and ~A" (q-dim Q1) (q-dim Q2))
	)
)

(defun qsub (Q1 Q2)
  (qadd Q1 (qnegate Q2))
)


(defun qmul (Q1 Q2)
	(unless (and (is-quantity Q1) (is-quantity Q2))
    	(error "ERROR: At least one of the two arguments is not a valid quantity"))

	(let ((res 
		(q 
			(* (q-val Q1) (q-val Q2))  
			(norm (list '* (q-dim Q1) (q-dim Q2)))
		)))
      
        (unless (is-quantity res)
			(error "ERROR: The result is not a valid quantity ~A" res))
			
		res
	)
)
        
(defun qdiv (Q1 Q2)
	(unless (is-quantity Q2)
		(error "ERROR: Divider is not a valid quantity"))
	(when (= (q-val Q2) 0)
		(error "ERROR: Division by zero"))
	(qmul Q1 (qinverse Q2))
)

(defun qexp (Q N)
	(unless (and (is-quantity Q) (numberp N))
		(error "ERROR: Invalid arguments"))
	(q 
		(expt (q-val Q) N) 
		(norm (list 'expt (q-dim Q) N)))
)	


(defun prefixed-factor (unit)
  "Returns the numerical factor associated with the unit prefix 
  or 1 if there is no prefix or it is a base/derived unit"
	(let ((prefix (prefix-match unit)))
		(if (and prefix (not (member prefix (unit-symbols))))
        	(cdr (assoc prefix *prefixes*))
        	1
    	)
	)
)

; ---- MANCA ----
(defun extract-numeric-factor (dim)
  "Estrae ricorsivamente il fattore numerico associato ai prefissi in un'espressione dimensionale."
  (cond
    ;; Caso base: simbolo singolo
    ((symbolp dim)
     (prefixed-factor dim))

    ;; Caso: (expt unit exp)
    ((and (consp dim) (eq (first dim) 'expt))
     (let ((base (extract-numeric-factor (second dim)))
           (exp (third dim)))
       (expt base exp)))

    ;; Caso: (* a b c ...)
    ((and (consp dim) (eq (first dim) '*))
     (extract-mul-factor (rest dim)))

    ;; Altro: ritorna 1 per sicurezza
    (t 1)))

; ---- MANCA ----
(defun extract-mul-factor (terms)
  "Estrae ricorsivamente il prodotto dei fattori da una lista di termini."
  (if (null terms)
      1
      (* (extract-numeric-factor (first terms))
         (extract-mul-factor (rest terms)))))

; ---- MANCA ----
(defun remove-prefixes (expr)
	(cond
		;; caso base, è una singola unità o numero
		((atom expr) 
			
			(prefix-free expr))
		
		;; caso esponente: (EXPT unit exp)
		((and (listp expr)
		 (eq (first expr) 'expt))
		
			(list 'expt (prefix-free (second expr)) (third expr)))

		;;; caso prodotto: (* elem1 elem2 ...)
		((and (listp expr)
		 (eq (first expr) '*))
		
			(cons '* (mapcar #'remove-prefixes (rest expr))))
		
		(t expr)
	)
)









