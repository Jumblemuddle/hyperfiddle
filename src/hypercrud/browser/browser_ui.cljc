(ns hypercrud.browser.browser-ui
  (:require [cats.core :as cats :refer [mlet >>=]]
            [cats.monad.either :as either]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.link :as link]
            [hypercrud.browser.routing :as routing]
            [hypercrud.ui.css :refer [css-slugify classes]]
            [hypercrud.ui.native-event-listener :refer [native-listener]]
            [hypercrud.ui.safe-render :refer [safe-user-renderer]]
            [hypercrud.ui.stale :as stale]
            [hypercrud.ui.tooltip :as tooltip]
            [hypercrud.util.core :as util :refer [unwrap]]
            [hypercrud.util.non-fatal :refer [try-either]]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.foundation.actions :as foundation-actions]))


(declare ui-from-link)

; defn because hypercrud.ui.result/view cannot be required from this ns
(defn f-mode-config []
  {:from-ctx :user-renderer
   :from-link :fiddle/renderer
   :with-user-fn (fn [user-fn]
                   (fn [result ordered-fes links ctx]
                     [safe-user-renderer user-fn result ordered-fes links ctx]))
   ; todo ui binding should be provided by a RT
   :default #?(:clj  (assert false "todo")
               :cljs hypercrud.ui.result/view)})

(letfn [(browse [link-index ident ctx & args]
          (let [kwargs (util/kwargs args)
                [user-renderer & args] (get kwargs nil)
                ctx (if user-renderer
                      (assoc ctx :user-renderer user-renderer #_(if f #(apply f %1 %2 %3 %4 args)))
                      ctx)]
            [ui-from-link (get link-index ident) ctx (:class kwargs)]))
        (anchor [link-index ident ctx label & args]
          (let [kwargs (util/kwargs args)
                props (-> (link/build-link-props (get link-index ident) ctx)
                          #_(dissoc :style) #_"custom renderers don't want colored links")]
            [(:navigate-cmp ctx) props label (:class kwargs)]))
        (browse' [link-index ident ctx]
          (->> (base/data-from-link (get link-index ident) ctx)
               (cats/fmap :result)
               (cats/fmap deref)))
        (anchor* [link-index ident ctx]
          (link/build-link-props (get link-index ident) ctx))]
  ; process-data returns an Either[Error, DOM]
  (defn process-data [{:keys [result ordered-fes links ctx]}]
    (mlet [ui-fn (base/fn-from-mode (f-mode-config) (:fiddle ctx) ctx)
           :let [link-index (->> links
                                 (filter :link/rel)         ; cannot lookup nil idents
                                 (mapv (juxt #(-> % :link/rel) identity)) ; [ repeating entity attr ident ]
                                 (into {}))
                 ctx (assoc ctx
                       :anchor (reactive/partial anchor link-index)
                       :browse (reactive/partial browse link-index)
                       :anchor* (reactive/partial anchor* link-index)
                       :browse' (reactive/partial browse' link-index))]]
      (cats/return (ui-fn @result ordered-fes links ctx)))))

(defn e->map [e]
  (if (map? e)
    e
    {:message #?(:clj  (.getMessage e)
                 :cljs (ex-message e))
     :data (ex-data e)
     :cause #?(:clj  (.getCause e)
               :cljs (ex-cause e))}))

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
                          (when (foundation/navigable? encoded-route (get-state))
                            (foundation-actions/set-route (:peer ctx) encoded-route dispatch! get-state)))))
    (.stopPropagation event)))

(defn wrap-ui [ui' route ctx & [class]]
  (let [on-click (reactive/partial (or (:page-on-click ctx)
                                       (reactive/partial page-on-click ctx))
                                   route)]
    ^{:key route}                                           ; clear memory when route changes
    [native-listener {:on-click on-click}
     [stale/loading (stale/can-be-loading? ctx) ui'
      (fn [e] [:div {:class (classes "ui" class "hyperfiddle-error")} (ui-error e ctx)])
      (fn [ui] [:div {:class (classes "ui" class)} ui])
      (fn [ui] [:div {:class (classes "ui" class "hyperfiddle-loading")} ui])]]))

(defn ui-from-route [route ctx & [class]]
  [wrap-ui (>>= (base/data-from-route route ctx) process-data) route ctx class])

(defn ui-from-link [link ctx & [class2]]
  (let [link-props' (try-either (link/build-link-props link ctx)) ; Why can this throw? This shouldn't throw.
        {:keys [route class hidden tooltip]} (unwrap link-props') #_"What does this error even mean"]
    ; todo should filter hidden links out before recursing (in render-inline-links)
    ;;;; Dustin disagrees with above, because the hidden-eval could break and report that error here
    (if-not hidden                                          ; undocumented semantic user-prop
      [tooltip/tooltip
       (let [[status label] (if (string? tooltip) [:info tooltip] [(first tooltip) (second tooltip)])]
         {:status status :label label})
       (let [ui' (if route (>>= (base/data-from-route route ctx) process-data) link-props')]
         [wrap-ui ui' route ctx (classes class class2 (css-slugify (:link/rel link)))])])))
