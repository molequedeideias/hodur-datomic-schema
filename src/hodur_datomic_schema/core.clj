(ns hodur-datomic-schema.core
  (:require [camel-snake-kebab.core :refer [->kebab-case-string]]
            [datascript.core :as d]
            [datascript.query-v3 :as q]
            [com.rpl.specter :refer :all]
            #_[sc.api :as sc]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti ^:private primitive-or-ref-type
          (fn [field]
            (let [ref-type (-> field :field/type :type/name)
                  dat-type (-> field :datomic/type)]
              (or dat-type ref-type))))

(defmethod primitive-or-ref-type "String" [_] :db.type/string)

(defmethod primitive-or-ref-type "Float" [_] :db.type/float)

(defmethod primitive-or-ref-type "Integer" [_] :db.type/long)

(defmethod primitive-or-ref-type "Boolean" [_] :db.type/boolean)

(defmethod primitive-or-ref-type "DateTime" [_] :db.type/instant)

(defmethod primitive-or-ref-type "ID" [_] :db.type/uuid)

(defmethod primitive-or-ref-type :default [_] :db.type/ref)

(defn ^:private get-value-type
  [field]
  (if-let [dat-type (-> field :datomic/type)]
    dat-type
    (primitive-or-ref-type field)))

(defn ^:private get-cardinality
  [{:keys [field/cardinality]}]
  (if cardinality
    (if (and (= (first cardinality) 1)
             (= (second cardinality) 1))
      :db.cardinality/one
      :db.cardinality/many)
    :db.cardinality/one))

(defn ^:private assoc-documentation
  [m {:keys [field/doc field/deprecation]}]
  (if (or doc deprecation)
    (assoc m :db/doc
             (cond-> ""
                       doc (str doc)
                       (and doc deprecation) (str "\n\n")
                       deprecation (str "DEPRECATION NOTE: " deprecation)))
    m))

(def ^:private defaults-customized-datomic-db-atributes
  {:model.attr/calculado? false
   :model.attr/apenas-runtime? false})



(def ^:private assoc-customized-datomic-db-atributes
  {:model.attr/calculado? :formiguinhas/calculado?
   :model.attr.enum/dominio :formiguinhas/dominio
   :model.attr.enum/descontinuado :formiguinhas/descontinuado
   :model.attr/apenas-runtime? :formiguinhas/apenas-runtime?
   :model.attr/estrategia-calculo :formiguinhas/estrategia-calculo
   :model.attr/metodo-de-calculo :formiguinhas/metodo-de-calculo
   :model.attr/dominios-que-pode-referenciar :formiguinhas/dominios-que-pode-referenciar
   :model.attr/atributo-metadados-evento-externo? :formiguinhas/atributo-metadados-evento-externo?
   :model.attr/atributo-que-persiste-estado-workflow? :formiguinhas/atributo-que-persiste-estado-workflow?})

(def ^:private assoc-extra-datomic-atributes
  {:datomic/tupleType               :db/tupleType
   :datomic/tupleTypes              :db/tupleTypes
   :datomic/tupleAttrs              :db/tupleAttrs
   :datomic.attr/preds              :db.attr/preds})


(defn ^:private assoc-attributes
  [m field]
  (let [table (merge {:datomic/isComponent :db/isComponent
                      :datomic/fulltext    :db/fulltext
                      :datomic/index       :db/index
                      :datomic/unique      :db/unique
                      :datomic/noHistory   :db/noHistory}
                     assoc-extra-datomic-atributes
                     assoc-customized-datomic-db-atributes)]



    (reduce-kv (fn [a k v]
                 (if-let [entry (get table k)]
                   (assoc a entry v)
                   a))
               m field)))

(defn ^:private process-field
  [entity-id is-enum? {:keys [field/name] :as field}]
  (cond-> {:db/ident (keyword entity-id
                              (->kebab-case-string name))}
          (not is-enum?) (assoc :db/valueType (get-value-type field)
                                :db/cardinality (get-cardinality field))

          (= (get-value-type field) :db.type/string) (assoc :db/fulltext true)

          ;;defaults
          (not is-enum?) (merge (transform [MAP-KEYS]
                                           #(% assoc-customized-datomic-db-atributes)
                                           defaults-customized-datomic-db-atributes))

          (not is-enum?) (assoc-attributes field)

          :always (assoc-documentation field)))


(defn ^:private get-type
  [{:keys [type/name type/enum field/_parent]}]
  (let [entity-id (->kebab-case-string name)]
    (->> _parent
         (sort-by :field/name)
         (reduce (fn [c {:keys [datomic/tag] :as field}]
                   (if tag
                     (conj c (process-field entity-id enum field))
                     c))
                 []))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn schema
  [conn]
  (let [selector '[* {:field/_parent
                      [* {:field/type [*]}]}]
        eids (-> (q/q '[:find ?e
                        :where
                        [?e :datomic/tag true]
                        [?e :type/nature :user]
                        (not [?e :type/interface true])
                        (not [?e :type/union true])]
                      @conn)
                 vec flatten)
        types (->> eids
                   (d/pull-many @conn selector)
                   (sort-by :type/name))]
    (->> types
         (reduce (fn [c t]
                   (concat c (get-type t)))
                 [])
         vec)))


(comment
  (do
    (require '[hodur-engine.core :as engine])

    (def conn (engine/init-schema
                '[^{:datomic/tag true}
                  default

                  ^:interface
                  Person
                  [^String name]

                  Employee
                  [^String name
                   ^{:datomic/type :db.type/tuple
                     :datomic/tupleType :db/long
                     :model.attr/persisted? true}  tupla
                   ^{:type String
                     :doc "The very employee number of this employee"
                     :datomic/unique :db.unique/identity
                     :datomic/fulltext false}
                   number
                   ^Float salary
                   ^Integer age
                   ^DateTime start-date
                   ^Employee supervisor
                   ^{:type Employee
                     :cardinality [0 n]
                     :doc "Has documentation"
                     :deprecation "But also deprecation"}
                   co-workers
                   ^{:datomic/type :db.type/keyword}
                   keyword-type
                   ^{:datomic/type :db.type/uri}
                   uri-type
                   ^{:datomic/type :db.type/double}
                   double-type
                   ^{:datomic/type :db.type/bigdec
                     :deprecation "This is deprecated"}
                   bigdec-type
                   ^EmploymentType employment-type
                   ^SearchResult last-search-results]

                  ^{:union true}
                  SearchResult
                  [Employee Person EmploymentType]

                  ^{:enum true}
                  EmploymentType
                  [FULL_TIME
                   ^{:doc "Documented enum"}
                   PART_TIME]]))

    (clojure.pprint/pprint
      (schema conn))))


