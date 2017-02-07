(ns apunte.examen5)

(def sfilter (fn [f xs]
               (letfn [(g [ys zs ts h]
                         (if (nil? (first ys))
                           [zs ts]
                           (g 
                             
                             (rest ys)
                              
                              (if (h (first ys))
                                (str zs (first ys))
                                zs)
                              
                              (if (h (first ys))
                                ts
                                (str ts (first ys)))
                              
                              h)))]
                 (g xs "" "" f))))

(sfilter (fn [x] (if (= x (first "123321"))
                   true
                   false)) "123321")

(def cuenta (fn [xs]
              (letfn [(g [ys]
                        (- (int (first((sfilter (fn [y] (= (first ys) y)) xs)0))) 48))
                      (t [ys]
                        (count ((sfilter (fn [y] (= (first ys) y)) ys)0))
                        )]
                {(g xs) (t xs)})))

(def recorre (fn [xs]
               (letfn [(g [ys zs]
                         (if (nil? (first ys))
                           zs
                           (g ((sfilter  (fn [x] (if (= x (first ys))
                                                  true
                                                  false)) ys)1)
                              (conj zs (cuenta ys))
                              )))]
                 (g xs {}))))

(def mmap (fn [f xs]
            (letfn [(g [ys zs h]
                      (if (nil? (first ys))
                        zs
                        (g (rest ys) (conj zs (h (first ys))) h)))]
              (g xs {} f))))

(def final (fn [xs]
             (mmap (fn [x]x) (recorre xs))))

(final "12345.23456543")



























