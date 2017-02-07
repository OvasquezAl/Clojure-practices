(ns firstrest.fosc)


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

