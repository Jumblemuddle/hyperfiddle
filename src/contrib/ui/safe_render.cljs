(ns contrib.ui.safe-render
  (:require [contrib.cljs-platform :refer [code-for-browser code-for-nodejs]]
            [contrib.reagent :refer [fragment]]
            [reagent.core :as reagent]))


(code-for-nodejs
  (require '[reagent.dom.server :as reagent-server]))

(declare user-portal)

(code-for-nodejs
  (defn user-portal [with-error & children]
    ; No portal in SSR, so errors will crash the whole page.
    ; IDE doesn't SSR so use the IDE to fix it.
    (apply fragment children)))

(code-for-browser
  (defn user-portal [with-error & children]
    (let [show-error (atom false)
          e-state (reagent/atom nil)]
      (reagent/create-class
        {:reagent-render (fn [with-error & children]
                           (apply fragment
                                  (let [e @e-state]
                                    (if (and @show-error e)
                                      (do
                                        (reset! show-error false) ; only show the error once, retry after that
                                        [[with-error e]])
                                      children))))

         :component-did-catch (fn [#_this e info]           ; args will need updating in reagent0.8.x
                                (reset! show-error true)
                                (reset! e-state e))}))))
