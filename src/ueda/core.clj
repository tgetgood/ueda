(ns ueda.core
  (:require [oz.core :as oz]))

(defn linear-regression [xs ys]
  (let [sx  (reduce + xs)
        sy  (reduce + ys)
        sx2 (transduce (map #(* % %)) + xs)
        sxy (reduce + (map * xs ys))
        n   (count xs)
        det (- (* n sx2) (* sx sx))]
    {:a (/ (- (* n sxy) (* sy sx)) det)
     :b (/ (- (* sx2 sy) (* sx sxy)) det)}))

(def data
  {:sweden       [7.87 4.22 2.49 0.94 0.89 0.87 0.81 0.78 0.71 0.69]
   :netherlands  [8.68 7.31 6.02 2.64 1.75 1.72 1.51 1.42 1.31 1.29]
   :canada       [11.9 6.72 3.84 2.81 2.73 2.68 2.65 2.49 1.71 1.69]
   :france       [28.11 9.83 5.35 3.3 2.94 2.54 2.46 2.33 2.03 1.99]
   :mexico       [31.18 10.12 2.06 3.79 3.46 2.91 2.71 2.17 2.06 1.86]
   :argentina    [29.66 7.61 6.35 4.1 3.8 2.75 2.7 2.69 2.51 2.44]
   :spain        [25.99 16.96 5.01 4.74 3.57 3.34 3.12 2.64 2.14 1.69]
   :england      [79.86 11.02 7.22 6.38 5.09 4.88 4.3 3.3 3.1 2.99]
   :italy        [23.59 15.8 11.82 11.14 7.84 5.9 4.54 4.44 3.61 3.36]
   :west-germany [21.92 18.56 11.42 8.27 7.28 7.04 6.94 6.53 5.84 5.66]
   :brazil       [49.81 38.57 9.68 9.52 8.08 8.03 6.99 5.02 4.95 2.78]
   :ussr         [63.34 36.36 13.32 11.37 10.9 10.84 10.7 10.27 9.5 9.17]
   :japan        [110.21 32.14 18.88 16.34 13.37 11.95 10.7 7.89 7.71 7.04]
   :us           [77.81 35.5 24.79 20.02 16.7 9.39 9.38 8.76 7.63 7.5]
   :india        [45.97 30.03 22.98 20.62 17.25 16.11 11.49 9.47 9.07 7.21]
   :china        [69 40.1 36.92 32.2 24.11 21.46 21.21 16.5 15 11.13]})


(defn pull-or-average [xs i]
  (if (zero? (mod i 1))
    (nth xs (dec i))
    (let [i (Math/floor i)]
      (/ (+ (nth xs (dec i)) (nth xs i)) 2))))

(defn median [xs]
  (let [i (/ (inc (count xs)) 2)]
    (pull-or-average xs i)))

(defn lower-fourth [xs]
  (let [m (Math/floor (/ (inc (count xs)) 2))
        i (/ (inc m) 2)]
    (pull-or-average xs i)))

(defn summarise [xs]
  (let [xs (sort xs)
        lf (lower-fourth xs)
        uf (lower-fourth (reverse xs))
        df (- uf lf)
        lc (- lf (* 1.5 df))
        uc (+ uf (* 1.5 df))
        upper-outliers (filter #(> % uc) xs)
        lower-outliers (filter #(< % lc) xs)]
    {:median (median xs)
     :lower-fourth lf
     :upper-fourth uf
     :spread df
     :lower-cutoff lc
     :upper-cuttoff uc
     :lower-outliers lower-outliers
     :upper-outliers upper-outliers
     :upper-whisker (->> xs
                        (remove #(contains? (set upper-outliers) %))
                        last
                        (min uc))
     :lower-whisker (->> xs
                         (remove #(contains? (set lower-outliers) %))
                         first
                         (max lc))
     :min (first xs)
     :max (last xs)}))

(def jukedata
  (->> data
       (map (fn [[k v]] [k (median v) v]))
       (sort-by second >)
       (map-indexed (fn [i [k _ v]]
                      (for [x v]
                        {:country k :pop x})))
       flatten))

(def display-order
  (->> data
       (map (fn [[k v]] [k (median v)]))
       (sort-by second >)
       (map first)))

(defn denormalise [m]
  (conj
   (map (fn [x]
          (assoc m :upper-outliers x))
        (:upper-outliers m))
   (dissoc m :upper-outliers)))

(def box-plot
  {:data {:values jukedata}
   :width 500
   :mark {:type :boxplot
          :extent 1.5}
   :encoding {:x {:field :pop :type :quantitative}
              :y {:field :country
                  :type :ordinal
                  :sort (mapv name display-order)}}})

(def y-axis
  {:y {:field :country
       :type  :nominal
       :sort  {:field :median :order :descending}}})

(defn clean-data [data]
  (->> data
       (map (fn [[k v]]
              (assoc (summarise v) :country k)))
       (mapcat denormalise)))

(defn spread-level [[k v]]
  (let [{:keys [median spread]} (summarise v)]
    {:log-median (/ (Math/log median) (Math/log 10))
     :log-spread (/ (Math/log spread) (Math/log 10))
     :country k}))

(defn slope [data]
  (let [points (map spread-level data)]
    (linear-regression (map :log-median points) (map :log-spread points))))

(defn xform [f data]
  (into {} (map (fn [[k v]] [k (mapv f v)])) data))

(defn nrsum [xs]
  (let [m (/ (reduce + xs) (count xs))
        var (/ (reduce + (map (fn [x] (Math/pow (- m x) 2)) xs)) (dec (count xs)))
        sd (Math/pow var 0.5)]
    {:log-median (Math/log m)
     :log-spread (Math/log (* sd 5.396))}))

(defn mb [data]
  {:data   {:values (clean-data data)}
   :width  600
   :height 700
   :scale {:domain [1 3.5]}
   :layer  [{:mark     {:style :boxplot-outliers
                        :type  :point}
             :encoding (merge y-axis
                              {:x {:field :upper-outliers
                                   :type  :quantitative}})}
            {:mark     {:type  :bar
                        :size  14
                        :style :boxplot-box}
             :encoding (merge y-axis
                              {:x  {:field :lower-fourth
                                    :type  :quantitative}
                               :x2 {:field :upper-fourth
                                    :type  :quantitative}})}
            {:mark     {:type   :tick
                        :color  :white
                        :orient :vertical
                        :style  :boxplot-median}
             :encoding (merge y-axis
                              {:x {:field :median
                                   :type  :quantitative}})}
            {:mark     {:type :rule :style :boxplot-rule}
             :encoding (merge y-axis
                              {:x  {:field :lower-whisker
                                    :type  :quantitative}
                               :x2 {:field :lower-fourth}})}
            {:mark     {:type :rule :style :boxplot-rule}
             :encoding (merge y-axis
                              {:x  {:field :upper-fourth
                                    :type  :quantitative}
                               :x2 {:field :upper-whisker}})}]})

(defn slp [data]
  {:data   {:values (map nrsum (vals data))}
   :width  500
   :height 500
   :layer  [{:mark     {:type :point}
             :encoding {:x {:field :log-median
                            :type  :quantitative}
                        :y {:field :log-spread
                            :type  :quantitative}}}]})

(def p (- 1 0.91414) #_(- 1 (:a (slope data))))

(oz/view! (mb (xform #(Math/pow % p) data)))
