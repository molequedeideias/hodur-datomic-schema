(ns octopus-test
  (:require [clojure.test :refer :all]
            [hodur-engine.octopus :as engine]
            [datascript.core :as d]
            [hodur-datomic-schema.octopus :as datomic]
            [com.rpl.specter :refer :all]
            [midje.sweet :refer :all]
            [matcher-combinators.midje :refer [match throws-match]]
            [matcher-combinators.matchers :as mt]
            #_[sc.api :as sc]))


(defn ^:private schema->datomic [s]
  (-> s
      engine/init-schema
      datomic/schema))

(deftest test-expansion
  (let [s (schema->datomic '[^{:datomic/tag                true
                               :model.attr/apenas-runtime? false}

                             default

                             ^{:interface true}

                             Person
                             [^{:type                       String
                                :model.attr/apenas-runtime? false}
                              name]

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
                              ^{:datomic/type               :db.type/keyword
                                :model.attr/apenas-runtime? true}
                              keyword-type
                              ^{:datomic/type :db.type/uri}
                              uri-type
                              ^{:datomic/type :db.type/double}
                              double-type
                              ^{:datomic/type :db.type/bigdec
                                :deprecation  "This is deprecated"}
                              bigdec-type
                              ^{:datomic/type       :db.type/tuple
                                :datomic/tupleAttrs [:employee/age :employee/co-workers]
                                :datomic/unique     :db.unique/identity
                                :cardinality        1
                                :doc                "Identificador entidade composta"
                                :spec/tag           false}
                              composite-key
                              ^EmploymentType employment-type
                              ^SearchResult last-search-results]

                             ^{:union true}
                             SearchResult
                             [Employee Person EmploymentType]

                             ^{:enum               true
                               :model.attr/dominio :enum/teste}
                             EmploymentType
                             [FULL_TIME
                              ^{:doc "Documented enum"}
                              PART_TIME]])]

    (facts
      (fact "Todos String tem full-text, default true"
            (select [ALL ALL (collect-one :db/ident) (must :db/fulltext)] s)
            => [[:employee/name true] [:employee/number false] [:person/name true]])

      (facts "Só os atributos marcados explicitamente como :model.attr/apenas-runtime? true não tem default false"
             (fact
               (set (select [ALL FIRST]
                            (select [ALL ALL (collect-one :db/ident) (pred #(= (:formiguinhas/apenas-runtime? %) false))] s)))
               => #{:person/name
                    :employee/age
                    :employee/bigdec-type
                    :employee/co-workers
                    :employee/double-type
                    :employee/employment-type
                    :employee/last-search-results
                    :employee/name
                    :employee/number
                    :employee/salary
                    :employee/start-date
                    :employee/supervisor
                    :employee/tupla
                    :employee/uri-type
                    :employee/composite-key})


             (fact (select [LAST ALL :db/ident] s)
                   => [:employee/composite-key])

             (fact
               (select [ALL FIRST]
                       (select [ALL ALL (collect-one :db/ident) (pred #(= (:formiguinhas/apenas-runtime? %) true))] s))
               => [:employee/keyword-type])

             (fact
               (select [ALL FIRST]
                       (select [ALL ALL (collect-one :db/ident) (pred #(= (:formiguinhas/dominio %) :enum/teste))] s))
               => [:employment-type/full-time :employment-type/part-time])

             (fact
               (select [ALL FIRST]
                       (select [ALL ALL (collect-one :db/ident) (pred #(= (:formiguinhas/dominios-que-pode-referenciar %) :enum/teste))] s))
               => [:employee/employment-type])))))



