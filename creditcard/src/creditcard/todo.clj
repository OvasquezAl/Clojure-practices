(ns creditcard.todo)

(def recorrer (fn  [f xs vac] 
                (letfn [(g [ys zs f ac] 
                          (if (nil? (first ys)) 
                            ac                                
                            (g (rest ys)(conj zs ( first ys )) f (f zs (rest ys) ac))))]
                  (g xs [] f  vac )))) 
  


(def centro (fn [xs]                   
               (let [la-suma (fn [es]  (vreduce (fn [a b] (+ a b)) 0  es))  
                     busca-centro (fn [ys zs ac] (if (=(la-suma zs)(la-suma ys)) 
                                                    (conj [] (count ys)) 
                                                     ac))]
                 (recorrer busca-centro xs [])))) 

 
 (centro [1 3 4 5 -2 1]) 
 (centro [1 6 4 5 -2 1]) 
 (centro [1 2 3 4 3 2 1]) 
 (centro []) 
 (centro [5]) 
 (centro [3 3 3 3]) 
 (centro [3 3 3 3 3]) 
 (centro [-1 -2 -3 -4 -3 -2 -1]) 
 (centro [1 100 50 -51 1 1]) 


;cambiar a 1 o 2
(def vmapc (fn [xs]
            (letfn [(g [ys zs c]
                      (if (nil? (first ys))
                        zs
                        (g (rest ys) (conj zs (if (even? c)
                                                2
                                                1)) (inc c))))]
              (g xs [] 1))))
(vmapc [1 2 3 4 9 10 11])
;multiplicar
(def multiplica (fn [xs]
                  (letfn [(g [ys ts h zs]
                            (if (nil? (first ys))
                              zs
                              (g (rest ys) 
                                 (rest ts) 
                                 h 
                                 (conj zs (h (first ys) (first ts))) 
                                 )))]
                    (g xs (vmapc xs) (fn [x y] (* x y)) []))))
(multiplica [3 0 9 0 6 8 2 4 9 3])

;sumar
(def smap (fn [f xs]
            (letfn [(g [ys zs h]
                      (if (nil? (first ys))
                        zs
                        (g (rest ys) (str zs (h (first ys))) h)))]
              (g xs "" f))))

(smap (fn [x]x) [3 0 9 0 6 8 2 4 9 3])

(def astring (fn [xs]
               (vmap (fn [x] x)
                 (smap (fn [x]x) (multiplica xs)
                       ))))
(def avector (fn [xs]
               (vmap (fn [x] (- (int x) 48)) (astring xs))))

(def valor (fn [xs]
             (vreduce (fn [x y] (+ x y)) 0 (avector xs))))
(valor )
(valor )

(def ultima (fn [xs]
              (if (= (mod (valor xs) 10) 0)
                0
                (* (- (mod (valor xs) 10) 10)-1))))




(def cuenta (fn [xs]
              (letfn [(g [ys]
                        (- (int (first((sfilter (fn [y] (= (first ys) y)) xs)0))) 48))
                      (t [ys]
                        (count ((sfilter (fn [y] (= (first ys) y)) ys)0))
                        )]
                {(g xs) (t xs)})))

(def recorre (fn [xs]
               (letfn [(g [ys zs]
                         (if (nil? (first ys))
                           zs
                           (g ((sfilter  (fn [x] (if (= x (first ys))
                                                  true
                                                  false)) ys)1)
                              (conj zs (cuenta ys))
                              )))]
                 (g xs {}))))

(def mmap (fn [f xs]
            (letfn [(g [ys zs h]
                      (if (nil? (first ys))
                        zs
                        (g (rest ys) (conj zs (h (first ys))) h)))]
              (g xs {} f))))

(def final (fn [xs]
             (mmap (fn [x]x) (recorre xs))))

(final "12345.23456543")


;partir el arreglo
(def parte2 (fn [xs x]
              (letfn [(g [ys zs a ts]
                        (if (= a 0)
                          [zs ts]
                          (g (rest ys) (str zs (first ys)) (dec a) (vmap (fn [x]x) (rest ys)))))]
                (g xs "" x ""))))
;invertir el str y pasarlo a vector
(def invertir2 (fn [xs]
                 (letfn [(g [ys zs]
                          (if (nil? (first ys))
                            zs
                            (g (rest ys) (cons (first ys) zs))) )]
                   (g xs []))))
;convertir a str
(def conver (fn [xs]
              (letfn [(g [ys zs]
                        (if (nil? (first ys))
                          zs
                          (g (rest ys) (str zs (first ys)))))]
                (g xs ""))))
(conver (invertir2 (parte "abcdefg" 2)))
;juntar los arreglos y mostrarlos
(def por-grupos-inversos (fn [xs x]
                           (letfn [(g [ys y zs]
                                     (if (nil? (first ys))
                                       zs
                                       (g ((parte2 ys y)1) y (str zs (conver(invertir2 ((parte2 ys y)0)))))))]
                             (g xs x ""))))


(por-grupos-inversos "Programacion" 3)
(por-grupos-inversos "Logica" 4)
(por-grupos-inversos "Funcional" 2)
(por-grupos-inversos "Programacion#logica#y#funcional" 5)
(por-grupos-inversos "mundo" 1)
(por-grupos-inversos "hola" 0)
(por-grupos-inversos "hola" 5)



(defn cfilter [f xs] (
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
                          (r xs #{} f)
                          ))


(defn cmap [f xs](letfn[(r [as bs g]
                             (if (empty? as)
                               bs
                               (r (rest as) (conj bs (g (first as))) g)
                               )
                             )]
                      (r xs #{} f)) )

(defn creduce [vi f  xs] 
               (letfn [(r [as vf g]
                         (if (empty? as)
                           vf
                           (r (rest as) 
                           (g vf (first as)) 
                           g))
                         )] (r xs vi f)))


(defn lmap  [f xs](letfn[(r [as bs g]
                             (if (empty? as)
                               bs
                               (r (butlast as) (cons (g (last as)) bs) g)
                               )
                             )]
                      (r xs '() f))  )

(defn lreduce [vi f  xs] 
               (letfn [(r [as vf g]
                         (if (empty? as)
                           vf
                           (r (rest as) 
                           (g vf (first as)) 
                           g))
                         )] (r xs vi f)))


(defn lfilter [f xs] (
                          letfn [(r [as bs g] (
                                                if (empty? as)
                                                bs
                                                (r (butlast as)
                                                   (if (g (last as))
                                                     (cons (last as) bs)
                                                     bs
                                                     )
                                                   g)
                                                ))]
                          (r xs '() f)
                          ))
(defn mfilter [f xs] (
                          letfn [(r [as bs g] (
                                                if (empty? as)
                                                bs
                                                (r (rest as)
                                                   (if (g ((first as)1))
                                                     (conj bs (first as))
                                                     bs
                                                     )
                                                   g)
                                                ))]
                          (r xs {} f)
                          ))


(defn mmap [f xs](letfn[(r [as bs g]
                             (if (empty? as)
                               bs
                               (r (rest as) (conj bs{((first as)0)(g((first as)1))}) g)
                               )
                             )]
                      (r xs {} f)))  
(defn mreduce [vi f  xs] 
               (letfn [(r [as vf g]
                         (if (empty? as)
                           vf
                           (r (rest as) 
                           (g vf ((first as)1)) 
                           g))
                         )] (r xs vi f)))

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


(defn vmap [f xs](letfn[(r [as bs g]
                             (if (empty? as)
                               bs
                               (r (rest as) (conj bs (g (first as))) g)
                               )
                             )]
                      (r xs [] f)))  

(defn vreduce [vi f xs] 
               (letfn [(r [as vf g]
                         (if (empty? as)
                           vf
                           (r (rest as) 
                           (g vf (first as)) 
                           g))
                         )] (r xs vi f)))


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

(sfilter (fn[x](clojure.string/ends-with? x "a")) "hola cara de bola")


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
