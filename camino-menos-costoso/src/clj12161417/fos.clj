(ns clj12161417.fos)

(defn vreduce [vi f xs] 
               (letfn [(r [as vf g]
                         (if (empty? as)
                           vf
                           (r (rest as) 
                           (g vf (first as)) 
                           g))
                         )] (r xs vi f)))



(defn adyacente-mayor [i arre] (if (> (count arre) 1)
                     (if(> (arre i) (arre (+ i 1))) 
                      (conj (conj [] (arre i)) i)
                      (conj (conj [] (arre (+ i 1))) (+ i 1)))
                     [(arre i) 0]
                                                      ) )

(defn adyacente-menor [i arre] (if (> (count arre) 1)
                     (if(< (arre i) (arre (+ i 1))) 
                      (conj (conj [] (arre i)) i)
                      (conj (conj [] (arre (+ i 1))) (+ i 1)))
                     [(arre i) 0]
                                                      ) )


(defn camino [f xs](letfn[(r [as bs i g]
                             (if (empty? as)
                               bs
                               (r (rest as) (conj bs ((g  i (first as))0)) ((g  i (first as))1) g)
                               )
                             )]
                      (r xs [] 0 f)))  