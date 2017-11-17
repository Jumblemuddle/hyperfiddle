(ns hypercrud.client.origin
  (:require [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [hypercrud.client.transit :as transit]
            [hypercrud.types.Err :refer [Err]]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.core :as util]
            [kvlt.core :as kvlt]
            [kvlt.middleware.params]
            [promesa.core :as p]
            [clojure.string :as string]
            [cuerdas.core :as str]
            [cats.monad.either :as either]))

(defmethod kvlt.middleware.params/coerce-form-params
  (keyword "application/transit+json")
  [{:keys [form-params]}]
  (transit/encode form-params))

(defmethod kvlt.middleware/from-content-type
  (keyword "application/transit+json; charset=utf-8")
  [resp]
  (update resp :body transit/decode))

(defmethod kvlt.middleware/from-content-type
  (keyword "application/transit+json")
  [resp]
  (update resp :body transit/decode))

(defmethod kvlt.middleware.params/coerce-form-params :application/edn [{:keys [form-params]}]
  (binding [pprint/*print-miser-width* nil
            pprint/*print-right-margin* 200]
    (with-out-str (pprint/pprint form-params))))

(defmethod kvlt.middleware/from-content-type :application/edn [resp]
  (let [decoded-val (reader/read-string (:body resp))]
    (assoc resp :body decoded-val)))

(defn v-not-nil? [stmt]
  (or (map? stmt)
      (let [[op e a v] stmt]
        (not (and (or (= :db/add op) (= :db/retract op))
                  (nil? v))))))

(defn hydrate-route! [service-uri route stage-val]
  (let [req (merge {:url (str (.-uri-str service-uri) "hydrate-route" route)
                    :accept :application/transit+json :as :auto}
                   (if (empty? stage-val)
                     {:method :get}                         ; Try to hit CDN
                     {:method :post
                      :form stage-val                       ; UI-facing interface is stage-val
                      :content-type :application/transit+json}))]
    (-> (kvlt/request! req) (p/then :body))))

(defn transact! [service-uri htx-groups]
  (let [htx-groups (->> (get htx-groups nil)
                        (util/map-values (partial filter v-not-nil?)))]
    (-> (kvlt/request!
          {:url (str (.-uri-str service-uri) "transact")
           :accept :application/transit+json :as :auto
           :method :post :form htx-groups
           :content-type :application/transit+json})
        (p/then (fn [resp]
                  (if (= 200 (:status resp))
                    ; clear master stage
                    ; but that has to be transactional with a redirect???
                    (p/resolved (:body resp))
                    (p/rejected resp)))))))

(defn human-error [e req]
  (let [unfilled-holes (->> (filter (comp nil? val) (.-params req)) (map key))]
    (if-not (empty? unfilled-holes)
      {:message "Invalid query" :data {:datomic-error (.-msg e) :query (.-query req) :missing unfilled-holes}}
      {:message "Datomic error" :data {:datomic-error (.-msg e)}})))

(defn process-result [resultset-or-error request]
  (if (instance? Err resultset-or-error)
    (either/left (human-error resultset-or-error request))
    (either/right resultset-or-error)))

(defn hydrate-one! [service-uri request stage-val]
  (mlet [{:keys [pulled-trees id->tempid]} (    hydrate-unprocessed! service-uri [request] stage-val)]
        (-> (process-result (nth pulled-trees 0) request)
            (either/branch p/rejected p/resolved))))

(defn hydrate! [service-uri requests stage-val]
  (mlet [{:keys [pulled-trees id->tempid]} (    hydrate-unprocessed! service-uri requests stage-val)]
        (let [result (->> pulled-trees (map #(process-result % requests)) sequence)]
          (either/branch result p/rejected p/resolved))))