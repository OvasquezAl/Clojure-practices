(ns ayuda.puertas)

(def vmap (fn [f xs mul]
            (letfn [(g [ys zs h b c]
                      (if (nil? (first ys))
                        zs
                        (g (rest ys) (if (= 0 (rem c b))
                                                (conj zs (h (first ys)))
                                                (conj zs (first ys))) h b (+ c 1))))]
              (g xs [] f mul 1))))

(def llenar (fn [x]
              (letfn [(g [y zs]
                        (if (= 0 y)
                          zs
                          (g (- y 1) (conj zs true))))]
                (g x [] ))))

(def cambia (fn [x] (if (= x false)
								          true
								          false)))


(def final (fn [x]
             (letfn [(g [xs y a]
                       (if (> a y)
                         xs
                       (g (vmap cambia xs a) y (+ a 1))))]
               (g (llenar x) x 2))))
(final 7)
