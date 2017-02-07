(ns clj12161417.atbash
  (:require [clj12161417.fos :as fos] :reload)
  (:require [clojure.string :as str] :reload)
  )



(def atbash {\a \z \á \y \b \x \c \w \d \v \e \ü \é \ú \f \u \g \t \h \s \i \r \í \q \j \p \k \ó \l \o \m \ñ \n \n \ñ \m \o \l \ó \k \p \j \q \í \r \i \s \h \t \g \u \f \ú \é \ü \e \v \d \w \c \x \b \y \á \z \a})



(defn enc[x](
              if (nil? (atbash x))
              (if (nil? (atbash
                           (first(char-array(str/lower-case x)))
                           )
                        )
                x
                (first(char-array(str/upper-case(atbash
                                                        (first(char-array(str/lower-case x)))
                                                        ))))
                )
                
              (atbash x)
              )
  )
               

(defn latbash [xs] (fos/lmap enc xs))
(defn vatbash [xs] (fos/vmap enc xs))
(defn satbash [xs] (fos/smap enc xs))