(ns hyperfiddle.ide.fiddles.user-dashboard
  (:require
    [contrib.reactive :as r]
    [hyperfiddle.ui :refer [markdown]]
    [contrib.reagent-native-events :refer [native-click-listener]]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.runtime :as runtime]))


(defn logout! [rt e]
  (runtime/dispatch! rt (actions/set-user-id rt nil))
  (.preventDefault e)
  (.stopPropagation e))

(defn renderer [ctx]
  [:div.hyperfiddle-user-dashboard
   (when @(runtime/state (:peer ctx) [::runtime/user-id])
     [native-click-listener
      {:key :logout :on-click (r/partial logout! (:peer ctx))}
      ; todo https://auth0.com/docs/logout
      [:span.nav-link.auth {:key :logout}
       [:a {:href "/logout"} "logout"]]])
   [hyperfiddle.ui/markdown (some-> ctx :hypercrud.browser/fiddle deref :fiddle/markdown) ctx]
   [:ul.link-list
    (->> @(:hypercrud.browser/result ctx)
         (sort-by :domain/ident)
         (map (fn [domain]
                [:li {:key (hash (:db/id domain))}
                 [:a {:href (str "http://" (:domain/ident domain) "." (get-in ctx [:host-env :ide/root]) "/")} (:domain/ident domain)]]))
         (doall))]])
