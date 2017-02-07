(ns apuntes.numero
  (:require [apuntes.fda :as fda ] :reload)
  )





(def de-digitos  (fn [numero ]
                   (let [xs (str numero )
                        
                         gxs (fda/sfilter (fn [e]  (if (= e \.) false true) )  xs)
                         fxs (fda/sfilter (fn [e]  (if (= e \- ) false true) )  gxs)
                         zs (fda/stoc fxs )
                         vxs (fda/stov fxs)
                         r (fn [y]  (let [ g (fn [x] (= x y) ) 
                                           
                                          h ( fda/vfilter g vxs)
                                          ]
                                      [ y (count h)  ]
                                      )  )
                         ]
                     
                     (fda/vmap r zs )
                     )
                   
                   
                   
                   ))

  


(de-digitos -1234.456)