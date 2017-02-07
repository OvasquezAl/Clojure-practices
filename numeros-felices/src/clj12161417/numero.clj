(ns clj12161417.numero
  (:require [clj12161417.fos :as fos] :reload))




(defn feliz? [x] (if (fos/validaCadena (str x))
                             (fos/recur x)
                             false        
                   ))

(defn infeliz? [x] (not (feliz? x)))

(defn felices? [xs] (fos/vreduce true (fn [x y] (and x y)) (fos/vmap feliz? xs)))

(defn infelices? [xs] (not (fos/vreduce true (fn [x y] (and x y)) (fos/vmap feliz? xs))))


