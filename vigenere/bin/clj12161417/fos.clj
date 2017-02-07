(ns clj12161417.fos)

(def mapa {\a 0 \á 1 \b 2 \c 3 \d 4 \e 5 \é 6 \f 7 \g 8 \h 9 \i 10 \í 11 \j 12 \k 13 \l 14 \m 15 \n 16 \ñ 17 \o 18 \ó 19 \p 20 \q 21 \r 22 \s 23 \t 24 \u 25 \ú 26 \ü 27 \v 28 \w 29 \x 30 \y 31 \z 32})
(def mapa2 {0 \a 1 \á 2 \b 3 \c 4 \d 5 \e 6 \é 7 \f 8 \g 9 \h 10 \i 11 \í 12 \j 13 \k 14 \l 15 \m 16 \n 17 \ñ 18 \o 19 \ó 20 \p 21 \q 22 \r 23 \s 24 \t 25 \u 26 \ú 27 \ü 28 \v 29 \w 30 \x 31 \y 32 \z})

(defn ida[c] (if (nil? (mapa c))
               -1
               (mapa c)))

(defn vuelta[n] (mapa2 n))

(defn mezclar [c1 c2] (if (= -1 (ida c2))
                        \space
                        (vuelta(mod (+ (ida c1) (ida c2)) 33)))
  )



(defn cifrado [c msj](letfn[(r [clave cve m  cif g]
                             (if (empty? m)
                               cif
                               (if (empty? cve)
                       (r clave clave m cif g)
                       (r clave (rest cve) (rest m) (str cif (g (first cve) (first m))) g)
                       ) 
                               )
                             )]
                      (r c c msj "" mezclar))) 

