(ns creditcard.tarjeta
  (:require [creditcard.fda :as fda] :reload))

(defn digito-de-comprobacion [ns] (if (and (fda/son15numeros? ns) (fda/son-digitos? (str ns)) )
                                    (fda/complemento (fda/sumaDigitos (str ns)))
                                    -1
                                    ))


(defn valida? [ns] (if (= (fda/digitos(last (str ns))) (digito-de-comprobacion (/(- ns (mod ns 10))10)))
                     true
                     false
                     ))

