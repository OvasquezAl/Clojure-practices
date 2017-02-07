(ns clj12161417.fos)



(def mapa {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000})
(def pesos {\I 1 \V 3 \X 1 \L 3 \C 1 \D 3 \M 1})

(defn valor[c] (if (= nil (mapa c))
                 0
                 (mapa c)
                 ))


(defn rom-dec [cad] (letfn [(r [sumar restar s] 
                              (if (empty? s)
                                (- sumar restar)
                                (if (>= (valor (first s)) (valor (first (rest s))))
                                (r (+ sumar (valor (first s))) restar (rest s))
                                (r sumar (+ restar (valor(first s))) (rest s))
                                )
                                )
                              )
                            ] 
                      (r 0 0 cad)))

(defn vmap [f xs](letfn[(r [as bs g]
                             (if (empty? as)
                               bs
                               (r (rest as) (conj bs (g (first as))) g)
                               )
                             )]
                      (r xs [] f)))  



(defn rom? [c] (if (= (mapa c) nil)
                 false
                 true
                 ))

(defn validaCadena [s] (sreduce true (fn [x y] (and x y)) (map rom? s)))

(defn cuatro [xs] (if(empty?(filter (fn [x] (> x 4)) (cuantos-de-cada xs)
                                   ))
                    true
                    false
                    
                    )) 



(defn seguidos [xs](letfn[(r [seg cadena c]
                               (if (> seg 3)
                                 false
                                 (if (empty? cadena)
                                  true
                                  (if (= c (first cadena))
                                    (r (+ seg (pesos (first cadena))) (rest cadena) c)
                                    (r (pesos (first cadena)) (rest cadena) (first cadena))
                                    )
                                  ))
                               )]
                        (r 0 xs (first xs))))


(defn cuantosPrimero [cad] (letfn [(r [cadena c a] 
                                     (if (empty? cadena)
                                       a
                                       (if (= (first cadena) c)
                                         (r (rest cadena) c (+ a 1))
                                         (r (rest cadena) c a)
                                         )
                                       )
                                     
                                     )] 
                             (r cad (first cad) 0)
                             ))
(defn cuantos-de-cada [xs](letfn[(r [arreglo cadena]
                                      (if (empty? cadena)
                                        arreglo
                                        (r (conj arreglo (cuantosPrimero cadena)) (rest cadena) )
                                        )
                                      )]
                               (r [] xs)))


(defn mas-de-4? [n] (> n 4))


(defn suma-resta [x y] (if (< x (mapa y))
                         (- (mapa y) x)
                         (+ x (mapa y) )))

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


(defn smap [f xs](letfn[(r [as bs g]
                             (if (empty? as)
                               bs
                               (r (rest as) (str bs (g (first as))) g)
                               )
                             )]
                      (r xs "" f)))  




(defn sreduce [vi f  xs] 
               (letfn [(r [as vf g]
                         (if (empty? as)
                           vf
                           (r (rest as) 
                           (g vf (first as)) 
                           g))
                         )] (r xs vi f)))

(defn suma-resta [vi f  xs] 
               (letfn [(r [as vf g]
                         (if (empty? as)
                           vf
                           (r (rest as) 
                           (g vf (first as)) 
                           g))
                         )] (r xs vi f)))

(defn numero-valido? [n] (if (and (> n 0) (< n 4000))
                           true
                           false
                           ))
(defn dec-rom [n] ())

