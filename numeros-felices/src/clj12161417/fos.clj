(ns clj12161417.fos)

(defn sreduce [vi f  xs] 
               (letfn [(r [as vf g]
                         (if (empty? as)
                           vf
                           (r (rest as) 
                           (g vf (first as)) 
                           g))
                         )] (r xs vi f)))


(defn smap [f xs](letfn[(r [as bs g]
                             (if (empty? as)
                               bs
                               (r (rest as) (str bs (g (first as))) g)
                               )
                             )]
                      (r xs "" f)))


(defn sfilter [f xs] (
                          letfn [(r [as bs g] (
                                                if (empty? as)
                                                bs
                                                (r (rest as)
                                                   (if (g (first as))
                                                     (str bs (first as))
                                                     bs
                                                     )
                                                   g)
                                                ))]
                          (r xs "" f)
                          ))

(defn vfilter  [f xs] (
                          letfn [(r [as bs g] (
                                                if (empty? as)
                                                bs
                                                (r (rest as)
                                                   (if (g (first as))
                                                     (conj bs (first as))
                                                     bs
                                                     )
                                                   g)
                                                ))]
                          (r xs [] f)
                          ))


(defn vreduce [vi f xs] 
               (letfn [(r [as vf g]
                         (if (empty? as)
                           vf
                           (r (rest as) 
                           (g vf (first as)) 
                           g))
                         )] (r xs vi f)))

(defn vmap [f xs](letfn[(r [as bs g]
                             (if (empty? as)
                               bs
                               (r (rest as) (conj bs (g (first as))) g)
                               )
                             )]
                      (r xs [] f)))  


(defn recur [x] (letfn [(r [n ni i]
                                           (if (= 1 (iteracion n))
                                             true
                                             (if (or (= ni (iteracion n)) (= i 0))
                                               false
                                               (r (iteracion n) ni (dec i))
                                               )
                                             )
                                           )] 
                                  (r x x 1000)))

(defn cuadrado [x] (* x x))

(def digito {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9})
(defn digito? [s] (if (nil?(digito s))
                    false
                    true))

(defn validaCadena [s] (sreduce true (fn [x y] (and x y)) (map digito? s)))
(defn scuadrado [s] (cuadrado (digito s)))
(defn cuadrados [ss] (map scuadrado ss))
(defn sumaDigitos [ss] (sreduce 0 (fn [x y] (+ x y )) ss))
(defn iteracion [n] (sumaDigitos(cuadrados (str n))))
(defn esUno? [x] (= x 1))
(defn ciclo? [x y] (= x y))
