(ns faconne.core
  (:require [faconne.compile :refer [gen-iterator gen-transformer]]
            [faconne.util :refer [error]]
            [clojure.pprint :refer [pprint]]))

(defn- options-map
  [options]
  (let [m (apply hash-map options)
        unsupported (-> m (dissoc :where))]
    (if-not (empty? unsupported)
      (error (str "Unsupported options to transform(er): "
                  unsupported "; only `:where` is supported."))
      m)))

(defmacro transformer
  [domain range & options]
  (let [where (:where (options-map options))]
    (gen-transformer domain range where)))

(defmacro iterator
  [domain action & options]
  (let [where (:where (options-map options))]
    (gen-iterator domain action where)))

(defmacro transform
  [x domain range & options]
  `(let [f# (transformer ~domain ~range ~@options)]
     (f# ~x)))

(defmacro for-each
  [x domain action & options]
  `(let [f# (iterator ~domain ~action ~@options)]
     (f# ~x)))

(defmacro print-generated-transformer
  [domain range & options]
  (let [where (:where (options-map options))]
    (pprint (gen-transformer domain range where))))

(defmacro print-generated-iterator
  [domain action & options]
  (let [where (:where (options-map options))]
    (pprint (gen-iterator domain action where))))


;; (gen-transformer '{:k [v] :as o} '{:o o} nil)

;; ((fn
;;    [structure78262]
;;    (let
;;        [result78261 (volatile! {})]
;;      (let
;;          [literal-parent78263 (get structure78262 :k) ;; changing this (big hack, but if tests work then whatever)
;;           o structure78262]
;;        (doseq [vec-parent78264 (or literal-parent78263 [nil])]
;;          (let [v vec-parent78264]
;;            ;; this line never gets executed because our pattern ignores v
;;            ;; this can be solved if result78261 starts off as {:o o} instead of {}
;;            ;; we may also want to check that we have a non-empty vector and do this parse-range
;;            (vswap! result78261 faconne.parse-range/deep-merge {:o o}))))
;;      (deref result78261)))
;;  {:a :b})

;; (gen-transformer '{:k [v] :as o} '{:o o} '[v])

;; (fn [structure90029]
;;   (let [result90028 (volatile! {})]
;;     (let [literal-parent90030 (get structure90029 :k)
;;           o structure90029]
;;       (doseq [vec-parent90031 (or (seq literal-parent90030) [nil])]
;;         (let [v vec-parent90031]
;;           (when v (vswap! result90028 faconne.parse-range/deep-merge {:o o})))))
;;     (deref result90028)))

;; (gen-transformer '{:k v :as o} '{:o o} nil)
;; '(fn
;;    [structure78401]
;;    (let
;;        [result78400 (volatile! {})]
;;      (let
;;          [literal-parent78402 (get structure78401 :k)
;;           o structure78401]
;;        (let [v literal-parent78402]
;;          (vswap! result78400 faconne.parse-range/deep-merge {:o o})))
;;      (deref result78400)))

;; (gen-transformer
;;   '{:k [{:vk vk}] :as o}
;;   '[vk]
;;   nil)

;; ((fn [structure89581]
;;    (let
;;        [result89580 (volatile! (transient []))]
;;      (let [literal-parent89582 (get structure89581 :k)
;;            o structure89581]
;;        (doseq [vec-parent89583 (or (seq literal-parent89582) [nil])]
;;          (let [literal-parent89584 (get vec-parent89583 :vk)]
;;            (let [vk literal-parent89584]
;;              (when vk (vswap! result89580 conj! vk))))))
;;      (vswap! result89580 persistent!)))
;;  {:a :b})

;; (transform
;;   {:a :b :k [1 1]}
;;   {:k [vk] :as o}
;;   {vk o})

;; (gen-transformer
;;   ;;{:a :b :k [1 2]}
;;   '{:k [{:a vk} {:b vk2}] :as o}
;;   '{
;;     vk o
;;     vk2 o
;;     }
;;   nil)


;; (transform
;;   {:a :b :k [{:a 1} 2]}
;;   {:k [{:a vk} {:b vk2}] :as o}
;;   {
;;    vk o
;;    }
;;   :where [vk]
;;   )

;; (transform
;;   {:a :b}
;;   {:k [vk vk2] :as o}
;;   {vk o})

;; (transform
;;   {:a :b}
;;   {:k [v] :as o}
;;   {:o o})

;; (transform
;;   {:k [] :a :b}
;;   {:k [v] :as o}
;;   {:o o})

;; (transform
;;   {:a :b}
;;   {:k {a b} :as o}
;;   {:o o})

;; from "super-contrived" in faconne.test.core-test
;; wtf is this?
#_(transform
    [{1 [2 3]} {}]
    [{k [v]} _]
    #{(+ k v)})
