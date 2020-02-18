(ns hodur-datomic-schema.octopus
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

(defn ^:private keys-tupla-composite
  [field]
  (-> field :datomic/tupleAttrs))

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
  {:model.attr/calculado?      false
   :model.attr/apenas-runtime? false})



(def ^:private table-customized-datomic-db-atributes-for-fields
  {:model.attr/calculado?                :formiguinhas/calculado?
   :model.attr/apenas-runtime?           :formiguinhas/apenas-runtime?
   :model.attr/estrategia-calculo        :formiguinhas/estrategia-calculo
   :model.attr/metodo-de-calculo         :formiguinhas/metodo-de-calculo
   :model.attr/dominio                   :formiguinhas/dominios-que-pode-referenciar
   :model.attr/metadados-evento-externo? :formiguinhas/atributo-metadados-evento-externo?
   :model.attr/persiste-estado-workflow? :formiguinhas/atributo-que-persiste-estado-workflow?})

(def ^:private table-customized-datomic-db-atributes-for-enums
  {:model.enum/dominio :formiguinhas/dominio})

(def ^:private table-extra-datomic-atributes
  {:datomic/tupleType  :db/tupleType
   :datomic/tupleTypes :db/tupleTypes
   :datomic/tupleAttrs :db/tupleAttrs
   :datomic.attr/preds :db.attr/preds})


(defn ^:private assoc-attributes
  [m field]
  (let [table (merge {:datomic/isComponent :db/isComponent
                      :datomic/fulltext    :db/fulltext
                      :datomic/index       :db/index
                      :datomic/unique      :db/unique
                      :datomic/noHistory   :db/noHistory}
                     table-extra-datomic-atributes
                     table-customized-datomic-db-atributes-for-fields)]



    (reduce-kv (fn [a k v]
                 (if-let [entry (get table k)]
                   (assoc a entry v)
                   a))
               m field)))

(defn ^:private assoc-attributes-for-enums
  [m field]
  (let [table table-customized-datomic-db-atributes-for-enums]
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

          (= (get-value-type field) :db.type/uuid) (assoc :db/unique :db.unique/identity)

          (keys-tupla-composite field) (assoc :db/unique :db.unique/identity)

          ;;defaults
          #_(not is-enum?) #_(merge (transform [MAP-KEYS]
                                               #(% table-customized-datomic-db-atributes-for-fields)
                                               defaults-customized-datomic-db-atributes))

          (not is-enum?) (assoc-attributes field)

          is-enum? (assoc-attributes-for-enums field)

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

(defn schema-atributos-e-enums
  "Diferença da implementação core é que ele gera atributos de interfaces"
  [conn]
  (let [selector '[* {:field/_parent
                      [* {:field/type [*]}]}]
        eids (-> (q/q '[:find ?e
                        :where
                        [?e :datomic/tag true]
                        [?e :type/nature :user]
                        #_(not [?e :type/interface true])
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

(defn schema-dominios-enums
  [conn]
  (let [dominios (d/q '[:find [?dominio ...]
                        :where [_ :model.attr/dominio ?dominio]]
                      @conn)]

    (mapv (fn [enum] {:db/doc   (str "Enums do domínio " enum)
                      :db/ident enum})
          dominios)))

;;TODO REESCREVER COM MEANDER, VAI FICAR MUITO MAIS LIMPO...
(defn ^:private separar-tuplas-no-schema
  [schema-octopus]
  (let [tuplas (select [(nthpath 1) ALL #(:db/tupleAttrs %)] schema-octopus)]
    [(first schema-octopus)
     (vec (remove (set tuplas) (second schema-octopus)))
     tuplas]))

(defn schema
  [conn]
  (->> [(schema-dominios-enums conn) (schema-atributos-e-enums conn)]
       separar-tuplas-no-schema
       (remove empty?)))

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
                   ^{:datomic/type          :db.type/tuple
                     :datomic/tupleType     :db/long
                     :model.attr/persisted? true} tupla
                   ^{:type             String
                     :doc              "The very employee number of this employee"
                     :datomic/unique   :db.unique/identity
                     :datomic/fulltext false}
                   number
                   ^Float salary
                   ^Integer age
                   ^DateTime start-date
                   ^Employee supervisor
                   ^{:type        Employee
                     :cardinality [0 n]
                     :doc         "Has documentation"
                     :deprecation "But also deprecation"}
                   co-workers
                   ^{:datomic/type :db.type/keyword}
                   keyword-type
                   ^{:datomic/type :db.type/uri}
                   uri-type
                   ^{:datomic/type :db.type/double}
                   double-type
                   ^{:datomic/type :db.type/bigdec
                     :deprecation  "This is deprecated"}
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

(comment "
No meta-db: 1- pegar todos os tipos Enum.

Para cada tipo Enum, achar todos os campos que usam este tipo
Para cada um destes campos, adicionar o atriuto dominios-que-pode-referenciar. com os valores do dominio

2= No plugin datomic, para cada enum, marcar incluir o dominio ")





