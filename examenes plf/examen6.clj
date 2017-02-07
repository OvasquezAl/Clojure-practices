(ns apunte.examen6)

(def vreduce (fn [f vi xs]
               (letfn [(g [ys vf h]
                         (if (nil? (first ys))
                           vf
                           (g (rest ys) (h vf (first ys)) h)))]
                 (g xs vi f))))


(vreduce (fn [x y] (+ x y)) 0 [1 2 3 4])



(def vmap (fn [f xs]
            (letfn [(g [ys zs h]
                      (if (nil? (first ys))
                        zs
                        (g (rest ys) (conj zs (h (first ys))) h)))]
              (g xs [] f))))


(vmap (fn [x] (* 2 x)) [1 2 3 4])

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




