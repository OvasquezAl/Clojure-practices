(ns firstrest.fosm)


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
