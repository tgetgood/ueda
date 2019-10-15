(ns ueda.five
  (:require [oz.core :as oz]
            [ueda.core :as core]))

(def raw
  {:defense        [1486 24508000]
   :hew            [388 21000000]
   :agriculture    [650 11467300]
   :treasury       [202 5798235]
   :congress       [446 5663174]
   :comerce        [164 5683609]
   :erda           [128 5236800]
   :nasa           [208 4500000]
   :transportation [117 2913509]
   :hud            [69 2455000]
   :white-house    [85 2300000]
   :vetadmin       [47 1313300]})


(def data
  (map (fn [[k [e c]]] {:name (name k) :employees e :cost c}) raw))


(def ls-reg
  (core/linear-regression (map :employees data) (map :cost data)))

(defn fit [line x] (double (+ (* (:a line) x) (:b line))))

(defn split-points [key ps]
  (let [s (sort-by key ps)
        c (count ps)
        n (quot c 3)]
    (case (mod c 3)
      0 [(take n s) (take n (drop n s)) (drop (* 2 n) s)]
      1 [(take n s) (take (inc n) (drop n s)) (drop (inc (* 2 n)) s)]
      2 [(take (inc n) s) (take n (drop (inc n) s)) (drop (inc (* 2 n)) s)])))

(defn median [xs]
  (let [n (count xs)]
    (if (= 0 (mod n 2))
      (/ (reduce + (take 2 (drop (dec (quot n 2)) xs))) 2)
      (first (drop (quot n 2) xs)))))

(defn three-group* [ps]
  (let [groups (split-points first ps)
        meds   (map (fn [ps]
                      [(median (map first ps)) (median (map second ps))])
                    groups)
        slope (/ (- (second (last meds)) (second (first meds)))
                 (- (first (last meds)) (first (first meds))))]
    {:a slope
     :b (/ (reduce + (map (fn [[x y]] (- y (* slope x))) meds)))}))

(defn residual [line [x y]]
  (let [y' (fit line x)]
    [x (- y y')]))

(defn loop-until-fit [epsilon line ps it]
  (when (< 100 it)
    (throw (Exception. "Failed to converge")))
  (let [residuals (map (partial residual line) ps)
        delta (three-group* residuals)]
    (if (or (< (* epsilon (:a delta)) (:a line)) (< (* epsilon (:b delta)) (:b line)))
      line
      (recur epsilon delta residuals (inc it)))))

(defn three-group [ps x-key y-key]
  (let [ps (map (fn [p] [(x-key p) (y-key p)]) ps)
        line (three-group* ps)
        residuals (map (partial residual line) ps)]
    (loop-until-fit 0.01 line residuals 0)))

(def scatter
  (let [i1 (three-group data :employees :cost)]
    {:data   {:values (concat data
                              (map (fn [{:keys [employees]}]
                                     {:lsx employees :lsy (fit ls-reg employees)})
                                   data)
                              (map (fn [{:keys [employees]}]
                                     {:tgx employees :tgy (fit i1 employees)})
                                   data)
                              #_(map (fn [{:keys [employees cost]}]
                                     {:trx employees :try (- cost (fit i1 employees))})
                                   data))}
     :height 700
     :width  700
     :layer  [
              {:mark {:type    :point
                      :tooltip {:content :data}}
               :encoding
               {:x {:field :employees :type :quantitative}
                :y {:field :cost :type :quantitative}}}
              {:mark {:type :line :color :firebrick :tooltip {:content :data}}
               :encoding {:x {:field :lsx :type :quantitative}
                          :y {:field :lsy :type :quantitative}}}

              {:mark {:type :line :color :purple}
               :encoding {:x {:field :tgx :type :quantitative}
                          :y {:field :tgy :type :quantitative}}}

              {:mark {:type :point :color :black}
               :encoding {:x {:field :trx}
                          :y {:field :try}}}]}))

(oz/view! scatter)
