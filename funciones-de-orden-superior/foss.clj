(ns firstrest.foss)


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

