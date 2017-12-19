(ns hypercrud.ui.control.code
  (:require [cats.monad.either :as either]
            [cuerdas.core :as str]
            [hypercrud.browser.anchor :as link]
            [hypercrud.client.tx :as tx]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.util.string :refer [safe-read-edn-string]]
            [re-com.core :as re-com]
            [reagent.core :as reagent]))


(defn sync-changed-props! [ref props]
  (doseq [[prop val] props]
    (if-not (= val (.getOption ref (name prop)))
      (.setOption ref (name prop) val))))

(def -codemirror

  ;; all usages of value (from react lifecycle) need to be (str value), because
  ;; codemirror throws NPE if value is nil

  (reagent/create-class
    {:reagent-render
     (fn [value change! props]
       [:textarea {:default-value (str value) :auto-complete "off" :class "text"}])

     :component-did-mount
     (fn [this]
       (let [[_ value change! props] (reagent/argv this)    ;[value change! props] (reagent/props this)
             ref (.fromTextArea js/CodeMirror (reagent/dom-node this) (clj->js props))]
         (aset this "codeMirrorRef" ref)
         (.on ref "blur" (fn [_ e]
                           (let [[_ value change! props] (reagent/argv this)
                                 value' (.getValue ref)]
                             (if-not (= value value')
                               (if change! (change! value'))))
                           nil))))

     :component-will-unmount
     (fn [this] (.toTextArea (aget this "codeMirrorRef")))

     :component-did-update
     (fn [this]
       (let [[_ value change! props] (reagent/argv this)
             ref (aget this "codeMirrorRef")]
         (sync-changed-props! ref (assoc props :value (str value)))))}))

(def validators {"clojure" #(-> (safe-read-edn-string %) (either/right?))})

(defn code* [value change! props]
  (let [defaults {:lineNumbers true
                  :matchBrackets true
                  :autoCloseBrackets true
                  :viewportMargin js/Infinity}
        props (merge defaults props)
        valid? ((get validators (:mode props) (constantly true)))
        class (str/join " " (list (if (:readOnly props) "read-only")
                                  (if (not valid?) "invalid")))]
    [:div.code-editor-wrapper {:class class}
     [-codemirror value change! props]]))

; useless layer, merge code-block with code
(defn code-block [props value change!]
  (let [props (if-not (nil? (:read-only props))
                (-> props
                    (dissoc :read-only)
                    (assoc :readOnly (:read-only props)))
                props)]
    [code* value change! props]))

(defn code-inline-block [& args]
  (let [showing? (reactive/atom false)]
    (fn [props value change!]
      [:div
       [re-com/popover-anchor-wrapper
        :showing? showing?
        :position :below-center
        :anchor [:a {:href "javascript:void 0;" :on-click #(swap! showing? not)} "edit"]
        :popover [re-com/popover-content-wrapper
                  :close-button? true
                  :on-cancel #(reset! showing? false)
                  :no-clip? true
                  :width "600px"
                  :body (code-block props value change!)]]
       " " value])))