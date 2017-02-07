(ns creditcard.fda)

(defn aUnDigito [x] (letfn [(r [n] (if (> n 9) 
                                   (if(>(+ (mod n 10) (/(- n (mod n 10))10))9)
                                     (r(+ (mod n 10) (/(- n (mod n 10))10)))
                                     (+ (mod n 10) (/(- n (mod n 10))10))
                                     ) 
                                   n))
                            ]
                      (r x)
                        
  )
  )

(defn son15numeros? [ns] (if(= (count (str ns)) 15)true false)  )

(defn digito? [s] (if (nil?(digitos s))
                    false
                    true))

(defn son-digitos? [ss] (letfn [(r [s f] (
                                           if (empty? s)
                                           f
                                           (if (digito?(first s))
                                             (r (rest s) true)
                                             false
                                             )
                                           ))] 
                          (r ss false)))


(defn uno-dos [n] (if (= n 1) 2 1))

(defn complemento [n] (- 10 (mod n 10)))

(defn sumaDigitos [ss] 
    (letfn [(r [xs a p] 
              (if (empty? xs)
                a
                (r (rest xs) (+ a (aUnDigito(* (uno-dos p) (digitos(first xs))))) (uno-dos p))))] 
      (r ss 0 1)))

(def digitos {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9})