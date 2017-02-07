(ns final.numero
  (:require [final.fde :as fde ] :reload)
  )



(def de-digitos  (fn [numero ]
                   (let [xs (str numero )
                        
                         gxs (fde/sfilter (fn [e]  (if (= e \.) false true) )  xs)
                         fxs (fde/sfilter (fn [e]  (if (= e \- ) false true) )  gxs)
                         zs (fde/stoc fxs )
                         vxs (fde/stov fxs)
                         r (fn [y]  (let [ g (fn [x] (= x y) ) 
                                           
                                          h ( fde/vfilter g vxs)
                                          ]
                                      [ y (count h)  ]
                                      )  )
                         ]
                     
                     (fde/vmap r zs )
                     )
                   
                   
                   
                   ))

  


(de-digitos -1234.456)