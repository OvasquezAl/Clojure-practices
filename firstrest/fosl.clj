(ns firstrest.fosl)

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
