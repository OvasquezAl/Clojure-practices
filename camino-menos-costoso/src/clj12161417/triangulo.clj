(ns clj12161417.triangulo
  (:require [clj12161417.fos :as fos] :reload))


 (defn camino-menos-costoso [lista] 
   (fos/camino fos/adyacente-menor lista))
 
 (defn camino-mas-costoso [lista] 
   (fos/camino fos/adyacente-mayor lista))
 
 (defn total-del-camino-menos-costoso [lista] (fos/vreduce 0 (fn [x y] (+ x y)) (camino-menos-costoso lista)))
 
 (defn total-del-camino-mas-costoso [lista] (fos/vreduce 0 (fn [x y] (+ x y)) (camino-mas-costoso lista)))
 