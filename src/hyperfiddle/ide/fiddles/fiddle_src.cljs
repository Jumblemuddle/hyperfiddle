(ns hyperfiddle.ide.fiddles.fiddle-src
  (:require
    [cats.core :as cats]
    [cats.monad.either :as either]
    [clojure.pprint]
    [contrib.css :refer [css]]
    [contrib.pprint]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.ui]
    [contrib.uri :refer [is-uri?]]
    [cuerdas.core :as str]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hypercrud.browser.system-fiddle :as system-fiddle]
    [hypercrud.types.Entity :refer [->Entity shadow-entity]]
    [hypercrud.ui.control.link-controls :refer [anchors]]
    [hypercrud.ui.error :as error]
    [hyperfiddle.ide.fiddles.topnav :refer [shadow-fiddle]]
    [hyperfiddle.ide.fiddles.fiddle-links.renderer :as links-fiddle]
    #_[hyperfiddle.ide.hf-live :as hf-live]                 ;cycle
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui :refer [browse field hyper-control link markdown]]))


(defn process-links [uri links]
  (->> links
       ;(sort-by (juxt :link/disabled :link/rel)
       ;         (fn [[a b] [a' b']] (if (= a a') (< b b') (< a a'))))
       (mapv (fn [link]
               (->> (if (system-fiddle/system-fiddle? (get-in link [:link/fiddle :db/ident]))
                      (dissoc link :link/fiddle)
                      link)
                    (->Entity uri))))))

(defn inject-links [fiddle-ref links-ref]
  (let [fiddle @fiddle-ref]
    (shadow-entity fiddle #(assoc % :fiddle/links (process-links (some-> fiddle .-uri) @links-ref)))))

; todo does this merge with shadow-fiddle?
; not sure if other downstream consumers of shadow-fiddle can run data-from-route
(defn shadow-links [ctx]
  (->> (base/data-from-route (:target-route ctx)
                             (assoc ctx
                               :hypercrud.browser/domain @(runtime/state (:peer ctx) [::runtime/domain])
                               :keep-disabled-anchors? true))
       (cats/fmap (fn [{:keys [:hypercrud.browser/links]}]
                    (-> ctx
                        (dissoc :hypercrud.browser/data :hypercrud.browser/data-cardinality :hypercrud.browser/path)
                        (update :hypercrud.browser/result #(r/track inject-links % links))
                        (context/focus [:body]))))))

(defn schema-links [ctx]
  (->> @(runtime/state (:peer ctx) [::runtime/domain :domain/environment])
       (filter (fn [[k v]] (and (str/starts-with? k "$") (is-uri? v))))
       sort
       (map (fn [[$db _]]
              (let [props {:route [(keyword "hyperfiddle.schema" $db)]
                           :target "_blank"}]
                ^{:key $db}
                [(:navigate-cmp ctx) props $db])))
       (doall)))

(def underdocs
  {:fiddle/pull "See [:fiddle/pull examples](http://www.hyperfiddle.net/:docs/:fiddle-pull/) and the
  [Datomic pull docs](https://docs.datomic.com/on-prem/pull.html)."
   :fiddle/query "See [:fiddle/query examples](http://www.hyperfiddle.net/:docs/:fiddle-query/) and the
   [Datomic query docs](https://docs.datomic.com/on-prem/query.html)."
   :fiddle/markdown "See [:fiddle/markdown examples](http://www.hyperfiddle.net/:docs/:fiddle-markdown/)."
   :fiddle/css "See [:fiddle/css examples](http://www.hyperfiddle.net/:docs/:fiddle-css/)."
   :fiddle/renderer "See [:fiddle/renderer examples](http://www.hyperfiddle.net/:docs/:fiddle-renderer/). `ctx` and
   `class` are in lexical scope. No `(ns (:require ...))` yet so vars must be fully qualified."
   :fiddle/links "See [:fiddle/links examples](http://www.hyperfiddle.net/:docs/:fiddle-links/)."})

(def controls
  {:fiddle/pull (fn [ref props ctx]
                  [:div
                   [hyper-control (dissoc props :embed-mode) ctx]
                   [:span.schema "schema: " (schema-links ctx)]
                   (when-not (:embed-mode props)
                     [:div.hf-underdoc [markdown (:fiddle/pull underdocs)]])])
   :fiddle/query (fn [ref props ctx]
                   [:div
                    [hyper-control (dissoc props :embed-mode) ctx]
                    [:span.schema "schema: " (schema-links ctx)]
                    (when-not (:embed-mode props)
                      [:div.hf-underdoc [markdown (:fiddle/query underdocs)]])])
   :fiddle/markdown (fn [ref props ctx]
                      [:div
                       [hyper-control (dissoc props :embed-mode) ctx]
                       (when-not (:embed-mode props)
                         [:div.hf-underdoc [markdown (:fiddle/markdown underdocs)]])])
   :fiddle/css (fn [ref props ctx]
                 [:div
                  [hyper-control (dissoc props :embed-mode) ctx]
                  (when-not (:embed-mode props)
                    [:div.hf-underdoc [markdown (:fiddle/css underdocs)]])])
   :fiddle/renderer (fn [ref props ctx]
                      [:div
                       [hyper-control (dissoc props :embed-mode) ctx]
                       (when-not (:embed-mode props)
                         [:div.hf-underdoc [markdown (:fiddle/renderer underdocs)]])])
   :fiddle/links (fn [ref props ctx]
                   [:div
                    [:div [links-fiddle/renderer ctx (:embed-mode props)]]
                    [anchors (:hypercrud.browser/path ctx) (dissoc props :embed-mode) ctx]
                    (when-not (:embed-mode props)
                      [:div.hf-underdoc [markdown (:fiddle/links underdocs)]])])
   })

(defn fiddle-src-renderer [ctx class & {:keys [embed-mode]}]
  (let [ctx (-> ctx
                (dissoc :user-renderer)                     ; this needs to not escape this level; inline links can't ever get it
                ; these two shadow calls are inefficient, throwing away work
                (shadow-fiddle))
        ectx (shadow-links ctx)
        ctx (either/branch ectx (constantly ctx) identity)]
    [:div {:class class}
     [:h3 (str @(r/cursor (:hypercrud.browser/result ctx) [:fiddle/ident])) " source"]
     (field [0 :fiddle/ident] ctx nil)
     (field [0 :fiddle/type] ctx nil)
     (case @(r/cursor (:hypercrud.browser/result ctx) [:fiddle/type])
       :entity (fragment (field [0 :fiddle/pull-database] ctx nil)
                         (field [0 :fiddle/pull] ctx (controls :fiddle/pull)))
       :query (field [0 :fiddle/query] ctx (controls :fiddle/query))
       :blank nil
       nil nil)
     (field [0 :fiddle/markdown] ctx (controls :fiddle/markdown))
     (field [0 :fiddle/css] ctx (controls :fiddle/css))
     (field [0 :fiddle/renderer] ctx (controls :fiddle/renderer))
     (field [0 :fiddle/hydrate-result-as-fiddle] ctx nil)
     (when (either/left? ectx)
       [:div
        [:h5 "Unable to shadow links:"]
        [error/error-block @ectx]])
     (field [0 :fiddle/links] ctx (controls :fiddle/links))
     [:div.p "Additional attributes"]
     (->> @(:hypercrud.browser/fields ctx)
          first
          ::field/children
          (map ::field/path-segment)
          (remove #(= (namespace %) "fiddle"))
          (map #(field [0 %] ctx nil))
          (doall))
     (when-not embed-mode (link :hyperfiddle/remove [:body 0] ctx "Remove fiddle" {:class "btn-outline-danger"}))]))
