(ns firstrest.core
  (:require [firstrest.fosl :as fosl] :reload)
  (:require [firstrest.fosv :as fosv] :reload)
  (:require [firstrest.fosm :as fosm] :reload)
  (:require [firstrest.fosc :as fosc] :reload)
  (:require [firstrest.foss :as foss] :reload))

//hacer funcion que valide un  numero de 10 digitos de una tarjeta de credito
//algoritmo de cifrado
//validar email
//validar rfc
//cifrado por sustitucion

(letfn[(sumaLista [listaEntrada acumulado] (if (nil? (first listaEntrada))
                                                       acumulado
                                                       (sumaLista (rest listaEntrada) (+ acumulado (first listaEntrada)))
                                                       ))
          
          ]
     (sumaLista '(1 2 3) 0))
(letfn[(sumaLista [listaEntrada acumulado] (if (nil? (first listaEntrada))
                                                       acumulado
                                                       (sumaLista (rest listaEntrada) (+ acumulado (first listaEntrada)))
                                                       ))
          
          ]
     (sumaLista [1 2 3 4 5] 0))
(letfn[(sumaLista [listaEntrada acumulado] (if (nil? (first listaEntrada))
                                                       acumulado
                                                       (sumaLista (rest listaEntrada) (+ acumulado (first listaEntrada)))
                                                       ))
          
          ]
     (sumaLista [1 2 3 4 5] 0))
(letfn[(sumaLista [listaEntrada acumulado] (if (nil? (first listaEntrada))
                                                       acumulado
                                                       (sumaLista (rest listaEntrada) (+ acumulado ((first listaEntrada)1)))
                                                       ))
          
          ]
     (sumaLista {:a 1 :b 2 :c 3} 0))

(letfn[(sumaLista [listaEntrada acumulado] (if (nil? (first listaEntrada))
                                                       acumulado
                                                       (sumaLista (rest listaEntrada) (+ acumulado (first listaEntrada)))
                                                       ))
          
          ]
     (sumaLista #{1 2 3 4 5 6} 0))

(letfn[(productoLista [listaEntrada acumulado] (if (nil? (first listaEntrada))
                                                    acumulado
                                                    (productoLista (rest listaEntrada) (* acumulado (first listaEntrada)))
                                                    )) ]
  (productoLista '() 1))



(letfn[(productoLista [listaEntrada acumulado] (if (nil? (first listaEntrada))
                                                    acumulado
                                                    (productoLista (rest listaEntrada) (* acumulado (first listaEntrada)))
                                                    )) ]
  (productoLista '[] 1))

(letfn[(productoLista [listaEntrada acumulado] (if (nil? (first listaEntrada))
                                                       acumulado
                                                       (productoLista (rest listaEntrada) (* acumulado ((first listaEntrada)1)))
                                                       ))
          
          ]
     (productoLista {:a 1 :b 2 :c 3 :d 4} 1))

(letfn[(productoLista [listaEntrada acumulado] (if (nil? (first listaEntrada))
                                                       acumulado
                                                       (productoLista (rest listaEntrada) (* acumulado (first listaEntrada))))
                                                       )
          ]
     (productoLista #{1 2 3 4} 1))

(letfn [(sumVs [xss a] (
                         if (empty? xss)
                         a
                         (if (vector? (first xss))
                           (sumVs (rest xss) (+ a (sumVs (first xss) 0)))
                           (sumVs (rest xss) (+ a (first xss)))
                         ) ))] 
  (sumVs [[10 20] [] [30] [40]] 0) )

(letfn [(multVs [xss a] (
                         if (empty? xss)
                         a
                         (if (vector? (first xss))
                           (multVs (rest xss) (+ a (multVs (first xss) 1)))
                           (multVs (rest xss) (+ a (first xss)))
                         ) ))] 
  (multVs [[10 20] [] [30] [40]] 1) )

(letfn [(dupl [li lo] (
                        if (empty? li)
                        (reverse lo)
                        (dupl (rest li) (conj lo (* 2 (first li))))
                        ))](
                        dupl '(10 20 30) '()    ))
(letfn [(dupl [li lo] (
                        if (empty? li)
                         lo
                        (dupl (rest li) (conj lo (* 2 (first li))))
                        ))](
                        dupl  [10 20 30] []    ))

(letfn [(dupl [li lo] (
                        if (empty? li)
                         lo
                        (dupl (rest li) (conj lo (* 2 (first li))))
                        ))](
                        dupl  #{10 20 30} #{}   ))
                              
                              
(letfn [(s[n p ne](
                   if(empty? n)
                   [p ne]
                   (if (pos? (first n))
                   (s (rest n) (conj p (first n)) ne)
                   (s (rest n) p (conj ne (first n)))
                   )
          ))](
               s [10 2 -3 -4 9 -1 0] [] []))

(letfn [(doble [x] (* 2 x))
        ( decimal [bins d] (
                            if(empty? bins)
                            d
                            (decimal (map doble (butlast bins)) (+ d (last bins)))
                            ))] 
  ( decimal [1 0 1 1 0 1 1] 0))

(filter (fn [x] (odd? x)) [1 2 3 4 5 6 7])


(let [filter (fn [f xs] (
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
                          ))]
  
 (filter (fn [x] (pos? x)) [-2 3 7 -5 1 -9]) )

(reduce (fn [x y] (+ x y)) [2 3 5])

(let [reduce (fn [vi f  xs] 
               (letfn [(r [as vf g]
                         (if (empty? as)
                           vf
                           (r (rest as) 
                           (g vf (first as)) 
                           g))
                         )] (r xs vi f)))] 
  (reduce 1 (fn [x y] (* x y)) [2 3 5] ))



;defn para definir map filter y reduce sobre listas, vectores, conjuntos, mapas y cadenas de caracteres.
(defn duplicar [x] (* 2 x))
(def mapa {:a -2 :b 3 :c -1 :d 0 :e 2 :f 1 :g -3})
(def coleccion #{-2 3 -1 0 2 1 -3})
(def lista '(-2 3 -1 0 2 1 -3))
(def arreglo [-2 3 -1 0 2 1 -3])




(cfilter (fn[x](pos? x)) coleccion)


;map para colecciones, mapas, vectores, listas y cadenas


;reduce para mapas, arreglos, colecciones, listas y cadenas
(vreduce 0 (fn[x y](+ x y)) [1 2 3 4 5])


