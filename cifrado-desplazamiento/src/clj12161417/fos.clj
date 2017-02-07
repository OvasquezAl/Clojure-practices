(ns clj12161417.fos
  (:require [clojure.string :as str]:reload))

(defn validarEntrada[n s] (if (and (>= n 0) (< n 33) (not (empty? s)))
                            true
                            false
                            ))
(def indice {\a 0 \á 1 \b 2 \c 3 \d 4 \e 5 \é 6 \f 7 \g 8 \h 9 \i 10 \í 11 \j 12 \k 13 \l 14 \m 15 \n 16 \ñ 17 \o 18 \ó 19 \p 20 \q 21 \r 22 \s 23 \t 24 \u 25 \ú 26 \ü 27 \v 28 \w 29 \x 30 \y 31 \z 32})
(def alfabeto ["a" "á" "b" "c" "d" "e" "é" "f" "g" "h" "i" "í" "j" "k" "l" "m" "n" "ñ" "o" "ó" "p" "q" "r" "s" "t" "u" "ú" "ü" "v" "w" "x" "y" "z"])

(defn smapx [f xs n](letfn[(r [as bs g num]
                                (if (empty? as)
                                  bs
                                  (r (rest as) (str bs (g num (first as))) g num)
                                  )
                                )]
                         (r xs "" f n)))  

(defn cif [n c] (if (nil? (indice c))
                  c
                  (alfabeto
                   (if (<= (+(indice c) n) 32)
                     (+(indice c) n)
                     (- (+(indice c) n) 33)
                     )))  )

(defn descif [n c] (if (nil? (indice c))
                     c
                     (alfabeto
                      (if (>= (-(indice c) n) 0)
                        (-(indice c) n)
                        (+ 33 (-(indice c) n))
                        )))  )

(defn cifrar[n c] (if (nil?(indice c))
                    (if (nil? (indice (str/lower-case c)))
                      c
                      (str/upper-case (cif n c))
                      )
                    (cif n c)
                    ))


(defn descifrar[n c] (if (nil?(indice c))
                       (if (nil? (indice (str/lower-case c)))
                         c
                         (str/upper-case (descif n c))
                         )
                       (descif n c)
                       ))
