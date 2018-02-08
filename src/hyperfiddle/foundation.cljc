(ns hyperfiddle.foundation
  (:require [clojure.string :as string]
            [cuerdas.core :as cuerdas]
            [hypercrud.client.core :as hc]
            [hypercrud.compile.reader :as reader]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
    #?(:cljs [hypercrud.ui.control.code :refer [code*]])
    #?(:cljs [hypercrud.ui.css :refer [classes]])
    #?(:cljs [hypercrud.ui.stale :as stale])
            [hypercrud.util.core :as util]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.foundation.actions :as foundation-actions]
            [hyperfiddle.runtime :as runtime]
            [promesa.core :as p]))


(def domain-uri #uri "datomic:free://datomic:4334/domains")
(def auth0-redirect-path "/auth0")                          ; ide

(defn hostname->hf-domain-name
  ([ctx]
   (hostname->hf-domain-name (:hostname ctx) (:hyperfiddle-hostname ctx)))
  ([hostname hyperfiddle-hostname]
    ; buggy
   (string/replace hostname (str "." hyperfiddle-hostname) "")))

(defn alias? [hf-domain-name]
  (string/includes? hf-domain-name "."))

(defn domain-request [hf-domain-name peer]
  (let [e (if (alias? hf-domain-name)
            [:domain/aliases hf-domain-name]
            [:domain/ident hf-domain-name])]
    (->EntityRequest e nil
                     (hc/db peer domain-uri nil)
                     [:db/id :domain/ident :domain/home-route :domain/aliases
                      {:domain/code-databases [:db/id :dbhole/name :dbhole/uri :repository/environment]}])))

(defn user-profile->ident [user-profile]
  (-> user-profile :email (cuerdas/replace #"\@.+$" "") (cuerdas/slug)))

(defn error-cmp [e]
  [:div
   [:h1 "Fatal error"]
   [:fieldset [:legend "(pr-str e)"]
    [:pre (pr-str e)]]
   [:fieldset [:legend "(ex-data e)"]                       ; network error
    [:pre (pr-str (ex-data e))]]                            ; includes :body key
   [:fieldset [:legend "(.-stack e)"]                       ; network error
    [:pre (.-stack e)]]])

; Can be removed once domain/databases are flattened up.
(defn process-domain-legacy [domain]
  (-> (into {} domain)
      (update :domain/code-databases
              (fn [repos]
                (->> repos
                     (map (fn [repo]
                            (-> (into {} repo)
                                ; todo this can throw
                                (update :repository/environment reader/read-string)))))))))

(defn context [ctx domain]
  (assoc ctx :hypercrud.browser/domain (process-domain-legacy domain)))

(defn local-basis [foo global-basis route domain #_"hack" ctx f]
  (concat
    (:domain global-basis)
    (f global-basis route ctx)))

(defn api [foo route ctx f]
  (let [domain-q (domain-request (hostname->hf-domain-name ctx) (:peer ctx))
        user-qs (when-let [domain (hc/hydrate-api (:peer ctx) domain-q)]
                  (f route (context ctx domain)))]
    (case foo
      "page" (concat [domain-q] user-qs)
      (concat [domain-q] user-qs))))

#?(:cljs
   (defn staging [peer dispatch!]
     (let [stage-val @(reactive/cursor (.-state-atom peer) [:stage])
           edn (util/pprint-str stage-val 70)]
       ; todo this can throw
       [code* edn #(dispatch! (foundation-actions/reset-stage peer (reader/read-edn-string %)))])))

#?(:cljs
   (defn leaf-view [route ctx f]
     (when-let [domain (let [user-domain (hostname->hf-domain-name ctx)]
                         (hc/hydrate-api (:peer ctx) (domain-request user-domain (:peer ctx))))]
       ; A malformed stage can break bootstrap hydrates, but the root-page is bust, so ignore here
       ; Fix this by branching userland so bootstrap is sheltered from staging area? (There are chickens and eggs)
       (f route (context ctx domain)))))

#?(:cljs
   (defn page-view [route ctx f]
     (let [domain' @(hc/hydrate (:peer ctx) (domain-request (hostname->hf-domain-name ctx) (:peer ctx)))]
       [stale/loading (stale/can-be-loading? ctx) domain'
        (fn [e]
          [:div.hyperfiddle-foundation
           [error-cmp e]
           [staging (:peer ctx) (:dispatch! ctx)]])
        (fn [domain]
          (let [ctx (context ctx domain)]
            [:div {:class (apply classes "hyperfiddle-foundation" @(reactive/cursor (.-state-atom (:peer ctx)) [:pressed-keys]))}
             (f route ctx)                                  ; nil, seq or reagent component
             (if @(reactive/cursor (.-state-atom (:peer ctx)) [:staging-open])
               [staging (:peer ctx) (:dispatch! ctx)])]))])))

#?(:cljs
   (defn view [foo route ctx f]
     (case foo
       ; The foundation comes with special root markup which means the foundation/view knows about page/user (not ide)
       ; Can't ide/user (not page) be part of the userland route?
       "page" (page-view route ctx f)
       (leaf-view route ctx f))))

(defn confirm [message]
  #?(:clj  (throw (ex-info "confirm unsupported by platform" nil))
     :cljs (js/confirm message)))

(defn navigable? [route {:keys [encoded-route branches] :as state}]
  (and (not= route encoded-route)
       (or (empty? branches)
           (confirm "Unstaged work will be lost on navigate, are you sure?"))))

(def LEVEL-NONE 0)
(def LEVEL-GLOBAL-BASIS 1)
(def LEVEL-SET-ROUTE 2)
(def LEVEL-LOCAL-BASIS 3)
(def LEVEL-HYDRATE-PAGE 4)

; careful setting load-level to LOCAL-BASIS; it is not supported as an init-level in the browser yet
; how can the browser know if hydrate page has or has not happened yet?
(defn bootstrap-data [rt dispatch! get-state init-level load-level encoded-route]
  (if (>= init-level load-level)
    (p/resolved nil)
    (-> (condp = (inc init-level)
          LEVEL-GLOBAL-BASIS (foundation-actions/refresh-global-basis rt dispatch! get-state)
          LEVEL-SET-ROUTE (foundation-actions/set-route rt (runtime/decode-route rt encoded-route) dispatch! get-state)
          LEVEL-LOCAL-BASIS (foundation-actions/refresh-page-local-basis rt dispatch! get-state)
          LEVEL-HYDRATE-PAGE (foundation-actions/hydrate-page rt nil dispatch! get-state))
        (p/then #(bootstrap-data rt dispatch! get-state (inc init-level) load-level encoded-route)))))
