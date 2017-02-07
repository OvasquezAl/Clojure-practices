(ns clj12161417.desplazamiento
  (:require [clj12161417.fos :as fos] :reload))


(defn archivo-cifrado [numero archivo] 
  (if (fos/validarEntrada numero archivo)
    (fos/smapx fos/cifrar (slurp archivo) numero)
    ""
    ))

(defn archivo-descifrado [numero archivo] 
  (if (fos/validarEntrada numero archivo)
    (fos/smapx fos/descifrar (slurp archivo) numero)
    ""
    ))