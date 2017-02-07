(ns final.fde)


(def vfilter (fn [f xs ]   
               (letfn [(g [h ac ys]
                         (if (= (first ys) nil) 
                           ac
                           
                           (g h 
                              (if (h (first ys)) (conj ac (first ys) )  ac   )   
                              (rest ys) )
                           )
                      )]
                 (g f [] xs)
                 
                 )
               ))
(def sfilter (fn [f xs ]   
               (letfn [(g [h ac ys]
                         (if (= (first ys) nil) 
                           ac
                           
                           (g h 
                              (if (h (first ys)) (str ac (first ys) )  ac   )   
                              (rest ys) )
                           )
                      )]
                 (g f "" xs)
                 
                 )
               ))

(def vmap (fn [f xs]
            (letfn [(g [ys ac h]
                      (if (= (first ys) nil)   
                          ac
                          
                          (g (rest ys) 
                             (conj ac (h (first ys)))
                              h 
                           )
                        )
                                           
                      )
                    
                    ]
              (g xs [] f)
              )
           )
  )



(def stov (fn [ xs]
            (letfn [(g [ys ac ]
                      (if (= (first ys) nil)   
                          ac
                          
                          (g (rest ys) 
                             (conj ac (first ys))
                              
                           )
                        )
                                           
                      )
                    
                    ]
              (g xs [] )
              )
           )
  )
(def stoc (fn [ xs]
            (letfn [(g [ys ac ]
                      (if (= (first ys) nil)   
                          ac
                          
                          (g (rest ys) 
                             (conj ac (first ys))
                              
                           )
                        )
                                           
                      )
                    
                    ]
              (g xs #{} )
              )
           )
  )