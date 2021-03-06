(ns hyperfiddle.ui.docstring
  (:require
    [contrib.string :refer [blank->nil]]
    [contrib.reactive :as r]
    [cuerdas.core :as str]))


(defn fqn->name [s]
  (-> s
      (str/split "\n" 2)
      (first)

      ; both cljs and js work with cljs eval
      (as-> s (if (str/includes? s "/")
                (str/split s "/")
                (str/split s ".")))
      last))

(defn attribute-schema-human [attr]
  ((juxt :db/ident
         #(some-> % :attribute/renderer fqn->name)
         #(some-> % :db/valueType :db/ident name)
         #(some-> % :db/cardinality :db/ident name)
         #(some-> % :db/isComponent (if :component) name)
         #(some-> % :db/unique :db/ident name))
    attr))

(defn semantic-docstring [ctx]
  (let [dbdoc (some-> ctx :hypercrud.browser/fat-attribute (r/cursor [:db/doc]) deref blank->nil)
        typedoc (some->> ctx :hypercrud.browser/fat-attribute
                         (r/fmap attribute-schema-human)
                         deref (interpose " ") (apply str))
        help-md (blank->nil
                  (str (if dbdoc (str dbdoc "\n\n"))        ; markdown needs double line-break
                       (if typedoc (str "`" typedoc "`"))))]
    help-md))
