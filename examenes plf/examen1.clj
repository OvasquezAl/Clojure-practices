(ns apunte.examen1)

(def vreduce  (fn [f vi xs] 
          (letfn [(g [ys vf h]  
                (if (= (first ys) nil) 
                  vf
                  (g (rest ys) (h vf(first ys)) h)))] 
        (g xs vi f)))) 

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