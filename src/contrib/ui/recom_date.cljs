(ns contrib.ui.recom-date
  (:require
    [clojure.set :refer [rename-keys]]
    [goog.date.UtcDateTime]
    [contrib.ui.input :as input]
    [re-com.core :as re-com]))



(defn valid-date-str? [s]
  (or (empty? s)
      (let [ms (.parse js/Date s)]                          ; NaN if not valid string
        (integer? ms))))

(defn parse-iso8601-string [s]
  (if (empty? s)
    nil
    (let [ms (.parse js/Date s)]
      (js/Date. ms))))

(defn iso8601-string [value change! props]
  (let [to-string #(some-> % .toISOString)]
    [input/validated-input value change! parse-iso8601-string to-string valid-date-str? props]))


(defn recom-date [value change! props]
  ; (new goog.date.UtcDateTime(new Date())).toIsoString()
  (let [props (rename-keys props {:disabled :disabled?})
        props (select-keys props [:class :disabled? :id])]
    (into
      [re-com/datepicker-dropdown
       :model (if value (goog.date.UtcDateTime. value))     ; not reactive
       :on-change #(change! (.-date %))]
      (flatten (seq props)))))
