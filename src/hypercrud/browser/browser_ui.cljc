(ns hypercrud.browser.browser-ui
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either :refer-macros [try-either]]
            [hypercrud.browser.anchor :as anchor]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.routing :as routing]
            [hypercrud.state.actions.core :as actions]
            [hypercrud.state.actions.util :as actions-util]
            [hypercrud.ui.css :as css]
            [hypercrud.ui.native-event-listener :refer [native-listener]]
            [hypercrud.ui.safe-render :refer [safe-user-renderer]]
            [hypercrud.ui.stale :as stale]
            [hypercrud.util.core :as util]
            [hypercrud.util.reactive :as reactive]
            [taoensso.timbre :as timbre]))


(declare ui-from-anchor)

; defn because hypercrud.ui.result/view cannot be required from this ns
(defn f-mode-config []
  {:from-ctx :user-renderer
   :from-link :fiddle/renderer
   :with-user-fn (fn [user-fn]
                   (fn [result ordered-fes anchors ctx]
                     [safe-user-renderer user-fn result ordered-fes anchors ctx]))
   ; todo ui binding should be provided by a RT
   :default hypercrud.ui.result/view})

(letfn [(browse [anchor-index ident ctx & args]
          (let [kwargs (util/kwargs args)
                [user-renderer & args] (get kwargs nil)
                ctx (if user-renderer
                      (assoc ctx :user-renderer user-renderer #_(if f #(apply f %1 %2 %3 %4 args)))
                      ctx)]
            [ui-from-anchor (get anchor-index ident) ctx (:class kwargs)]))
        (anchor [anchor-index ident ctx label]
          (let [props (-> (anchor/build-link-props (get anchor-index ident) ctx)
                          #_(dissoc :style) #_"custom renderers don't want colored links")]
            [(:navigate-cmp ctx) props label]))
        (browse' [anchor-index ident ctx]
          (->> (base/data-from-anchor (get anchor-index ident) ctx)
               (cats/fmap :result)))
        (anchor* [anchor-index ident ctx]
          (anchor/build-link-props (get anchor-index ident) ctx))
        (link-fn [anchor-index ident label ctx]
          (timbre/error "Warning: :link-fn is deprecated, and will be removed in a future release. Use :anchor instead")
          (anchor anchor-index ident ctx label))]
  ; process-data returns an Either[Error, DOM]
  (defn process-data [{:keys [result ordered-fes anchors ctx]}]
    (mlet [ui-fn (base/fn-from-mode (f-mode-config) (:fiddle ctx) ctx)
           :let [anchor-index (->> anchors
                                   (filter :link/rel)       ; cannot lookup nil idents
                                   (mapv (juxt #(-> % :link/rel) identity)) ; [ repeating entity attr ident ]
                                   (into {}))
                 ctx (assoc ctx
                       :anchor (reactive/partial anchor anchor-index)
                       :browse (reactive/partial browse anchor-index)
                       :anchor* (reactive/partial anchor* anchor-index)
                       :browse' (reactive/partial browse' anchor-index)
                       :link-fn (reactive/partial link-fn anchor-index))]]
      (cats/return (ui-fn result ordered-fes anchors ctx)))))

(defn e->map [e]
  (if (map? e)
    e
    {:message (ex-message e)
     :data (ex-data e)
     :cause (ex-cause e)}))

(defn ui-error-inline [e ctx]
  (let [dev-open? true
        {:keys [cause data message]} (e->map e)
        detail (if dev-open? (str " -- " (pr-str data)))]
    [:code message " " detail]))

(defn ui-error-block [e ctx]
  (let [dev-open? true
        {:keys [cause data message]} (e->map e)
        detail (if dev-open? (util/pprint-str data))]
    ; todo we don't always return an error with a message
    [:pre message "\n" detail]))

(defn ui-error [e ctx]
  ; :find-element :attribute :value
  (let [C (cond
            (:ui-error ctx) (:ui-error ctx)                 ; botnav
            (:attribute ctx) ui-error-inline                ; table: header or cell, form: header or cell
            (:find-element ctx) ui-error-inline             ;
            :else ui-error-block)]                          ; browser including inline true links
    [C e ctx]))

(defn page-on-click [ctx route event]
  (when (and route (.-altKey event))
    ((:dispatch! ctx) (fn [dispatch! get-state]
                        (let [encoded-route (routing/encode route)]
                          (when (actions-util/navigable? encoded-route (get-state))
                            (actions/set-route (:peer ctx) encoded-route dispatch! get-state)))))
    (.stopPropagation event)))

(defn wrap-ui [v' route ctx & [class]]
  (let [on-click (reactive/partial (or (:page-on-click ctx)
                                       (reactive/partial page-on-click ctx))
                                   route)]
    ^{:key route}
    [native-listener {:on-click on-click}
     [stale/loading v'
      (fn [e] [:div {:class (css/classes "ui" class "hyperfiddle-error")} (ui-error e ctx)])
      (fn [v] [:div {:class (css/classes "ui" class)} v])
      (fn [v] [:div {:class (css/classes "ui" class "hyperfiddle-loading")} v])]]))

(defn ui-from-route [route ctx & [class]]
  [wrap-ui (cats/bind (base/data-from-route route ctx) process-data) route ctx class])

(defn ui-from-anchor [anchor ctx & [class]]
  (let [anchor-props' (try-either (anchor/build-link-props anchor ctx)) ; LOOOOOLLLLLL we are dumb
        v' (mlet [anchor-props anchor-props']
             ; todo should filter hidden anchors out before recursing (in widget/render-inline-anchors)
             (if (:hidden anchor-props)
               (either/right [:noscript])
               (mlet [route (routing/build-route' anchor ctx)
                      ; entire context must be encoded in the route
                      data (base/data-from-route route (context/clean ctx))]
                 (process-data data))))
        route (-> (cats/fmap :route anchor-props')
                  (cats/mplus (either/right nil))
                  (cats/extract))]
    [wrap-ui v' route ctx class]))