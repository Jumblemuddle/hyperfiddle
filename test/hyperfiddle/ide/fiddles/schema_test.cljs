(ns hyperfiddle.ide.fiddles.schema_test
  (:require [clojure.test :refer [deftest is]]
            [contrib.reactive :as r]
            [contrib.pprint :refer [slow-pprint-str]]
            [hypercrud.browser.browser-ui-test :refer [test-renderer-str]]
            [hyperfiddle.ide.fiddles.schema :as schema]
            [hyperfiddle.ide.fiddles.schema-attribute :refer [read-only?]]
            ))


(deftest schema-renderer []
  (let [ctx {:cell (constantly [:pre])
             :hypercrud.browser/attribute (r/atom nil)
             :hypercrud.browser/fiddle (r/atom nil)
             :hypercrud.browser/fields (r/atom [])
             :hypercrud.browser/links (r/atom nil)
             :hypercrud.browser/request (r/atom nil)
             :hypercrud.browser/result (r/atom [])
             :hypercrud.browser/schema (r/atom nil)
             :hypercrud.browser/schemas (r/atom nil)}]
    ; just test it renderers something
    #_(is (not (nil? (test-renderer-str (:fiddle/renderer (schema/schema "$")) ctx))))))

(deftest db-attribute-renderer []
  (let [ctx {:cell (constantly [:pre])
             :hypercrud.browser/attribute (r/atom nil)
             :hypercrud.browser/fiddle (r/atom nil)
             :hypercrud.browser/fields (r/atom [])
             :hypercrud.browser/links (r/atom nil)
             :hypercrud.browser/request (r/atom nil)
             :hypercrud.browser/result (r/atom nil)
             :hypercrud.browser/schema (r/atom nil)
             :hypercrud.browser/schemas (r/atom nil)}]
    ; just test it renderers something
    ; Working, but missing a find-element etc mock. We need actual context mocks.
    #_(is (not (nil? (test-renderer-str (:fiddle/renderer (schema/db-attribute-edit "$")) ctx))))))

(deftest readonly-test
  []

  (let [record {}]
    (assert (not (read-only? :db/ident record)))
    (assert (not (read-only? :db/cardinality record)))
    (assert (not (read-only? :db/valueType record)))
    (assert (read-only? :db/unique record))
    (assert (read-only? :db/isComponent record)))

  (let [record {:db/ident :hello}]
    (assert (not (read-only? :db/ident record)))
    (assert (not (read-only? :db/cardinality record)))
    (assert (not (read-only? :db/valueType record)))
    (assert (read-only? :db/unique record))
    (assert (read-only? :db/isComponent record)))

  (let [record {:db/ident :hello :db/cardinality :one :db/valueType :string}]
    (assert (not (read-only? :db/ident record)))
    (assert (not (read-only? :db/cardinality record)))
    (assert (not (read-only? :db/valueType record)))
    (assert (not (read-only? :db/unique record)))
    (assert (not (read-only? :db/isComponent record))))

  )