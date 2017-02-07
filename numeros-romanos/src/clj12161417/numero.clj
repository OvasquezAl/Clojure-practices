(ns clj12161417.numero
  (:require [clj12161417.fos :as fos ] :reload))



(defn romano? [s] (and (fos/validaCadena s) (fos/cuatro s) (fos/seguidos s)))

(defn romano-a-decimal [s] (if (romano? s)
                             (fos/rom-dec s)
                             0
                             ))

(defn romanos-a-decimal [arr] (fos/vmap romano-a-decimal arr))

(defn decimal-a-romano [n] (if (fos/numero-valido? n)
                             "Sin implementar"
                             ""
                             ))
(defn decimales-a-romano [arr] (fos/vmap decimal-a-romano arr))
