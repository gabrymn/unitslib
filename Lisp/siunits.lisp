(defparameter *units*
  '(
    ;; unità base
    (|kg|    kilogram         |kg|                       :base    1)
    (|m|     metre            |m|                        :base    2)
    (|s|     second           |s|                        :base    3)
    (|A|     Ampere           |A|                        :base    4)
    (|K|     Kelvin           |K|                        :base    5)
    (|cd|    candela          |cd|                       :base    6)
    (|mol|   mole             |mol|                      :base    7)

    ;; unità derivate
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

 
 
 
(defun is-quantity (q)
	(and (listp q)
		(eq 'q (first q))
			(let ((n (second q)) (d (third q)))
					(and (numberp n)
					(is-dimension d))
			)
	)
)

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

(defun compare-units (u1 u2)
  (let ((id1 (si-unit-id u1))
        (id2 (si-unit-id u2)))
    (cond
      ((or (null id1) (null id2))
       (error "Unità non trovata"))
      ((< id1 id2) '<)
      ((> id1 id2) '>)
      (t '=))))
      
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

(defun agg-exp (terms acc)
	"Aggrega ricorsivamente i termini (unit exp) in acc."
	(if (null terms)
		acc
		(agg-exp (cdr terms)
			(upd-exp (car (car terms)) (car (cdr (car terms))) acc)
		)
	)
)

(defun sum-exp (terms)
  "Somma gli esponenti di unità uguali nella lista terms."
  (agg-exp terms nil))

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


(defun q (n d)
  (list 'q n (norm d))
)


(defun qnegate (q)
  (unless (is-quantity q)
    (error "ERROR: Invalid argument"))
  (let ((n (second q))
        (d (third q)))
    (list 'Q (- n)  d))
)


(defun qinverse (q)
  (unless (is-quantity q)
    (error "ERROR: Invalid argument"))
  (let ((n (second q))
        (d (third q)))
    (list 'Q (/ 1 n)  (norm (list 'expt d -1)))
   )
) 

(defparameter q1 (q 42 '|m|))

(defparameter q2 (q 1/2 '(* (expt |s| 3) (expt |m| -3))))

(defun qadd (q1 q2)

  (unless (and (is-quantity q1) (is-quantity q2))
    (error "ERROR: At least one of the two arguments is not a valid quantity"))

  (let ((n1 (second q1)) (d1 (third q1)) (n2 (second q2)) (d2 (third q2)))
        
    (let ((nd1 (norm d1))
          (nd2 (norm d2)))
          
      (if (equal nd1 nd2)
          
          (let ((res (list 'Q (+ n1 n2) nd1)))
            (if (is-quantity res)
                res
                (error "ERROR: The result is not a valid quantity ~A" res)))
          
          (error "ERROR: Incompatible dimensions ~A and ~A" nd1 nd2)))
  )
)


(defun qsub (q1 q2)
  (qadd q1 (qnegate q2))
)


(defun qmul (q1 q2)
  (unless (and (is-quantity q1) (is-quantity q2))
    (error "ERROR: At least one of the two arguments is not a valid quantity"))

  (let ((n1 (second q1))
        (d1 (third q1))
        (n2 (second q2))
        (d2 (third q2)))
    (let ((new-n (* n1 n2))
          (new-d (norm (list '* d1 d2))))
      (let ((res (list 'Q new-n new-d)))
        (unless (is-quantity res)
          (error "ERROR: The result is not a valid quantity ~A" res))
        res))))
        
;(print (qmul q1 q2))    
        
(defun qdiv (q1 q2)
  (unless (is-quantity q2)
    (error "ERROR: Divider is not a valid quantity"))
  (when (= (second q2) 0)
    (error "ERROR: Division by zero"))
  (qmul q1 (qinverse q2))
)

;;(print (qadd q1 q2))        
        
(defun qexp (q n)
  (unless (and (is-quantity q) (numberp n))
    (error "ERROR: Invalid arguments"))
  (let ((val (second q)) (dim (third q)))
  	(list 'Q (expt val n) (norm (list 'expt dim n)))
  )
)




(defparameter *prefixes*
  '(("Y" . 1e24)
    ("Z" . 1e21)
    ("E" . 1e18)
    ("P" . 1e15)
    ("T" . 1e12)
    ("G" . 1e9)
    ("M" . 1e6)
    ("k" . 1e3)
    ("h" . 1e2)
    ("da" . 1e1)
    ("d" . 1e-1)
    ("c" . 1e-2)
    ("m" . 1e-3)
    ("u" . 1e-6)
    ("n" . 1e-9)
    ("p" . 1e-12)
    ("f" . 1e-15)
    ("a" . 1e-18)
    ("z" . 1e-21)
    ("y" . 1e-24)))


(defun contains (elem lst test-fn)
  (cond
    ((null lst) nil)  
    ((funcall test-fn elem (car lst)) t) 
    (t (contains test-fn elem (cdr lst))))
) 

(defun escapedp (obj)
	(if (member obj '(* / + -) :test #'eq)
      nil
  (and (symbolp obj)
       (some #'(lambda (c)
                 (or (and (char>= c #\a) (char<= c #\z))  ; minuscole
                     (not (alphanumericp c))))            ; o carattere non alfanumerico
             (coerce (symbol-name obj) 'list))))
)  

(defun prefixp (sub str)
  (and (<= (length sub) (length str))
       (equal sub (subseq str 0 (length sub)))
  )
)

(defun calc-coeff (obj)
	(if (member obj (unit-symbols) :test #'equal)
		1
		(calc-coeff-ord (symbol-name obj))
	)
)

(defun calc-coeff-ord (str)
  (let ((matches 
          (remove-if-not
            (lambda (pair)
              (prefixp (car pair) str))
            *prefixes*)))
    (if matches
        ;; prendi il prefisso più lungo
        (let* ((best (reduce (lambda (a b)
                               (if (> (length (car a)) (length (car b))) a b))
                             matches))
               (prefisso (car best))
               (valore (cdr best))
               (resto (subseq str (length prefisso))))
          valore)
        ;; se non c'è prefisso, ritorna (str . 1)
        (cons str 1))))
        
        
(defun prefix-free (obj)
	(if (member obj (unit-symbols) :test #'equal)
		obj
		(trova (symbol-name obj))
	)
)

(defun trova (str)
  (let ((matches
          (remove-if-not
           (lambda (pair)
             (prefixp (car pair) str))
           *prefixes*)))
    (if matches
        (let* ((best (reduce (lambda (a b)
                               (if (> (length (car a)) (length (car b))) a b))
                             matches))
               (prefisso (car best))
               (valore (cdr best))
               (resto (subseq str (length prefisso))))
          (let ((unit-symbol
                 (find-if (lambda (sym)
                            (string= (symbol-name sym) resto))
                          (unit-symbols))))
            (when unit-symbol
              unit-symbol)))
        ;; Se non c'è prefisso, prova a trovare direttamente il simbolo
        (find-if (lambda (sym)
                   (string= (symbol-name sym) str))
                 (unit-symbols)))))    
        
        
(defun sostituisci-units (expr)
  (cond
    ;; Caso simbolo tra pipeline: sostituisci
    ((and (symbolp expr) (escapedp expr))
     (prefix-free expr))
    
    ;; Caso lista non vuota: processa car e poi ricorsivamente cdr
    ((consp expr)
     (cons (sostituisci-units (car expr))
           (sostituisci-units (cdr expr))))
    
    ;; Altrimenti: restituisci l’elemento così com'è
    (t expr)))

(defun calc-units (expr)
  (cond
    ;; Caso simbolo tra pipeline: sostituisci
    ((and (symbolp expr) (escapedp expr))
     (calc-coeff expr))
    
    ;; Caso lista non vuota: processa car e poi ricorsivamente cdr
    ((consp expr)
     (* (calc-units (car expr))
           (calc-units (cdr expr))))
    
    ;; Altrimenti: restituisci l’elemento così com'è
    (t 1))
)

(defun uniform-quantity (q)
	(list 
		(* (calc-units (third q)) (second q))  
		(sostituisci-units (third q)) 
	)
)

;; Test 1
;(print (calc-units '|km|))
;; Output atteso: (|km|)

;; Test 2
;(print (calc-units '(* |km| |m| |s|)))
;; Output atteso: (|km| |m| |s|)

;; Test 3
;(print (sostituisci-units '(expn |km| 2)))
;; Output atteso: (|km|)

;; Test 4
;(print (sostituisci-units '(* (expn |km| 2) |s|)))
;; Output atteso: (|km| |s|)

;; Test 5
;(print (sostituisci-units '(* |kT| |km| |s|)))
;; Output atteso: (|km| |s|)

;; Test 6
;(print (sostituisci-units '(+ 3 5)))
;; Output atteso: NIL

;; Test 7
;(print (sostituisci-units '(/ (expn |m| 3) (* |s| |km|))))
;; Output atteso: (|m| |s| |km|)

;(print (uniform-quantity '(q 2 (* |s| |km| ) ) ) )




