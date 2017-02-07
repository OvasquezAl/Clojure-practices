(ns apunte.examen4)
(def vmap (fn [f xs]
            (letfn [(g [ys zs h]
                      (if (nil? (first ys))
                        zs
                        (g (rest ys) (conj zs (h (first ys))) h)))]
              (g xs [] f))))
;partir el arreglo
(def parte2 (fn [xs x]
              (letfn [(g [ys zs a ts]
                        (if (= a 0)
                          [zs ts]
                          (g (rest ys) (str zs (first ys)) (dec a) (vmap (fn [x]x) (rest ys)))))]
                (g xs "" x ""))))
;invertir el str y pasarlo a vector
(def invertir2 (fn [xs]
                 (letfn [(g [ys zs]
                          (if (nil? (first ys))
                            zs
                            (g (rest ys) (cons (first ys) zs))) )]
                   (g xs []))))
;convertir a str
(def conver (fn [xs]
              (letfn [(g [ys zs]
                        (if (nil? (first ys))
                          zs
                          (g (rest ys) (str zs (first ys)))))]
                (g xs ""))))
(conver (invertir2 (parte "abcdefg" 2)))
;juntar los arreglos y mostrarlos
(def por-grupos-inversos (fn [xs x]
                           (letfn [(g [ys y zs]
                                     (if (nil? (first ys))
                                       zs
                                       (g ((parte2 ys y)1) y (str zs (conver(invertir2 ((parte2 ys y)0)))))))]
                             (g xs x ""))))


(por-grupos-inversos "Programacion" 3)
(por-grupos-inversos "Logica" 4)
(por-grupos-inversos "Funcional" 2)
(por-grupos-inversos "Programacion#logica#y#funcional" 5)
(por-grupos-inversos "mundo" 1)
(por-grupos-inversos "hola" 0)
(por-grupos-inversos "hola" 5)


