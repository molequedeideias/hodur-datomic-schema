(ns octopus-test
  (:require [clojure.test :refer :all]
            [hodur-engine.octopus :as engine]
            [datascript.core :as d]
            [datascript.query-v3 :as q]
            [hodur-datomic-schema.octopus :as datomic]
            [hodur-datomic-schema.core :as datomic-core]
            [com.rpl.specter :refer :all]
            [midje.sweet :refer :all]
            [matcher-combinators.midje :refer [match throws-match]]
            [matcher-combinators.matchers :as mt]
            [clojure.set :as set]
    #_[sc.api :as sc]))


(defn ^:private schema->datomic [s]
  (-> s
      engine/init-schema
      datomic/schema))

(defn ^:private schema->datomic-core [s]
  (-> s
      engine/init-schema
      datomic-core/schema))

(deftest test-expansion-comportamento-core
  "Alguns comportamento padrão para entendimento. Lembrar que o plugfin datomic normal gera um unico vetr de transacoes"
  (facts "sobre o comportamento dos tags-recursives"
         (let [s (schema->datomic-core '[^{:datomic/index true}
                                         default
                                         ^{:datomic/tag-recursive  true
                                           :spec/tag-recursive     true
                                           :graphviz/tag-recursive true}

                                         Workflow
                                         [^{:type                                 Estados-workflow, :cardinality 1, :optional false
                                            :model.attr/persiste-estado-workflow? true
                                            :doc                                  "Estado da máquina de estados associada à entidade"} status
                                          ^{:type String, :cardinality 1, :optional false,
                                            :doc  "Identificador do workflow - namespace completo"} ident
                                          ^EmploymentType employment-type]


                                         ^{:enum               true
                                           :model.attr/dominio :enum/teste}

                                         EmploymentType
                                         [FULL_TIME
                                          ^{:doc "Documented enum"}
                                          PART_TIME]

                                         ^{:enum               true
                                           :model.attr/dominio :enum/estado-do-workflow}
                                         Estados-workflow
                                         [ACEITO]])]
           (fact "o datomic-recursive não é aplicado nos enums...."
                 (set/difference #{:employment-type/full-time :employment-type/part-time :estados-workflow/aceito}
                                 (set (select [ALL :db/ident] s)))
                 => #{:employment-type/full-time :employment-type/part-time :estados-workflow/aceito})))

  (facts "sobre a gerencia dos enums prorpiamente ditos. Noteque neste exemplo já coloquei o :datomic/tag como default"
         (let [s (schema->datomic-core '[^{:datomic/index true
                                           :datomic/tag   true}
                                         default
                                         ^{:datomic/tag-recursive  true
                                           :spec/tag-recursive     true
                                           :graphviz/tag-recursive true}


                                         Workflow
                                         [^{:type                                 Estados-workflow, :cardinality 1, :optional false
                                            :model.attr/persiste-estado-workflow? true
                                            :doc                                  "Estado da máquina de estados associada à entidade"} status
                                          ^{:type String, :cardinality 1, :optional false,
                                            :doc  "Identificador do workflow - namespace completo"} ident
                                          ^EmploymentType employment-type]

                                         ^{:enum               true
                                           :model.attr/dominio :enum/teste}

                                         EmploymentType
                                         [FULL_TIME
                                          ^{:doc "Documented enum"}
                                          PART_TIME]

                                         ^{:enum               true
                                           :model.attr/dominio :enum/estado-do-workflow}
                                         Estados-workflow
                                         [ACEITO]])]
           (fact "o datomic/tag default é aplicado.."
                 (set/difference #{:employment-type/full-time :employment-type/part-time :estados-workflow/aceito}
                                 (set (select [ALL :db/ident] s)))
                 => #{})))

  (facts "... as os enums são gerados apenas se algum campo os referencia.
  Neste exemplo retires o campo status e que usava o enum Estadosworkflow"
         (let [s (schema->datomic-core '[^{:datomic/index true
                                           :datomic/tag   true}
                                         default
                                         ^{:datomic/tag-recursive  true
                                           :spec/tag-recursive     true
                                           :graphviz/tag-recursive true}

                                         Workflow
                                         [^{:type                                 String, :cardinality 1, :optional false
                                            :model.attr/persiste-estado-workflow? true
                                            :doc                                  "Estado da máquina de estados associada à entidade"} status
                                          ^{:type String, :cardinality 1, :optional false,
                                            :doc  "Identificador do workflow - namespace completo"} ident
                                          ^EmploymentType employment-type]


                                         ^{:enum               true
                                           :model.attr/dominio :enum/teste}

                                         EmploymentType
                                         [FULL_TIME
                                          ^{:doc "Documented enum"}
                                          PART_TIME]

                                         ^{:enum               true
                                           :model.attr/dominio :enum/estado-do-workflow}
                                         Estadosworkflow
                                         [ACEITO]])]
           (fact "Não é gerado o enum do atributo "
                 (set/difference #{:employment-type/full-time :employment-type/part-time :estados-workflow/aceito}
                                 (set (select [ALL :db/ident] s)))
                 => #{:estados-workflow/aceito})))

  (facts "Não são gerados campos para interfaces mas o enum referenciado pelo campo da interface é gerado..."
         (let [s (schema->datomic-core '[^{:datomic/index true
                                           :datomic/tag   true}
                                         default
                                         ^{:datomic/tag-recursive  true
                                           :spec/tag-recursive     true
                                           :graphviz/tag-recursive true}

                                         ^{:interface true}
                                         Workflow
                                         [^{:type                                 Estados-workflow, :cardinality 1, :optional false
                                            :model.attr/persiste-estado-workflow? true
                                            :doc                                  "Estado da máquina de estados associada à entidade"} status
                                          ^{:type String, :cardinality 1, :optional false,
                                            :doc  "Identificador do workflow - namespace completo"} ident
                                          ^EmploymentType employment-type]

                                         ^{:enum               true
                                           :model.attr/dominio :enum/teste}

                                         EmploymentType
                                         [FULL_TIME
                                          ^{:doc "Documented enum"}
                                          PART_TIME]

                                         ^{:enum               true
                                           :model.attr/dominio :enum/estado-do-workflow}
                                         Estados-workflow
                                         [ACEITO]])]
           (fact "o datomic/tag default é aplicado.."
                 (set (select [ALL :db/ident] s))
                 => #{:employment-type/full-time
                      :employment-type/part-time
                      :estados-workflow/aceito}))))

(deftest test-expansion-ocotopus

  (facts "Diferencas entre ocotopus e core"
         (let [s (schema->datomic '[^{:datomic/tag                true
                                      :model.attr/apenas-runtime? false}

                                    default

                                    ^{:interface true}

                                    Person
                                    [^{:type                       String
                                       :model.attr/apenas-runtime? false}
                                     name
                                     ^{:type                       ID
                                       :datomic/unique :db.unique/value}
                                     id]

                                    Employee
                                    [^ID id
                                     ^String name
                                     ^{:datomic/type          :db.type/tuple
                                       :datomic/tupleType     :db/long
                                       :model.attr/persisted? true
                                       :model.attr/restricoes-persistencia "[[?eid :model/attr]]"} tupla
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
                                       :cardinality        1
                                       :doc                "Identificador entidade composta"
                                       :spec/tag           false}
                                     composite-key
                                     ^EmploymentType employment-type
                                     ^Estadoworkflow status
                                     ^SearchResult last-search-results]

                                    ^{:union true}
                                    SearchResult
                                    [Employee Person EmploymentType]

                                    ^{:enum               true
                                      :model.attr/dominio :enum/teste}
                                    EmploymentType
                                    [FULL_TIME
                                     ^{:doc "Documented enum"}
                                     PART_TIME]
                                    ^{:enum               true
                                      :model.attr/dominio :enum/estado-do-workflow}
                                    Estadoworkflow
                                    [ACEITO]])]

           (fact "Todos ID ou Tuplas- composite tem :db/unique, default :db.unique/entity, sem precisar declarar"
                 (select [ALL ALL (collect-one :db/ident) (must :db/unique)] s)
                 =>  (match (mt/in-any-order [[:employee/id :db.unique/identity] [:employee/number :db.unique/identity]
                                              [:person/id :db.unique/value][:employee/composite-key :db.unique/identity]])))

           (fact "Todos String tem full-text, default true, sem precisar declarar"
                 (select [ALL ALL (collect-one :db/ident) (must :db/fulltext)] s)
                 => (match (mt/in-any-order [[:employee/name true] [:employee/number false] [:person/name true]])))

           (facts "Só os atributos marcados explicitamente como :model.attr/apenas-runtime? true não tem default false.
        Gerado tb os campos de Interface, (Person) e composite-keys."
                  (fact
                    (set (select [ALL FIRST]
                                 (select [ALL ALL (collect-one :db/ident) (pred #(= (:formiguinhas/apenas-runtime? %) false))] s)))
                    => #{:person/name
                         :person/id
                         :employee/age
                         :employee/bigdec-type
                         :employee/co-workers
                         :employee/double-type
                         :employee/employment-type
                         :employee/id
                         :employee/status
                         :employee/last-search-results
                         :employee/name
                         :employee/number
                         :employee/salary
                         :employee/start-date
                         :employee/supervisor
                         :employee/tupla
                         :employee/uri-type
                         :employee/composite-key})


                  (fact "os campos composite-keys são gerados na ultima posicao do vetor de transacoes"
                        (select [LAST ALL :db/ident] s)
                        => [:employee/composite-key])

                  (fact "consegue gerar corretamenet quando é sobreescreito a informacao de apenas-runtime?"
                        (select [ALL FIRST]
                                (select [ALL ALL (collect-one :db/ident) (pred #(= (:formiguinhas/apenas-runtime? %) true))] s))
                        => [:employee/keyword-type])

                  (fact "1. gera corretamente os enums, na posicao correta, juntos com os atributos comuns, e com as referencias do dominio "
                        (select [ALL FIRST]
                                (select [(nthpath 1) ALL (collect-one :db/ident) (must :formiguinhas/dominio)] s))
                        => [:employment-type/full-time :employment-type/part-time :estadoworkflow/aceito])

                  (fact "2. gera corretamente os enums, na posicao correta, juntos com os atributos comuns, e com as referencias do dominio "
                        (select [ALL FIRST]
                                (select [(nthpath 1) ALL (collect-one :db/ident) (pred #(= (:formiguinhas/dominio %) [ :enum/teste]))] s))
                        => [:employment-type/full-time :employment-type/part-time])

                  (fact "1. gera corretamente os metadados que podem referenciar enums"
                        (select [ALL FIRST]
                                (select [ALL ALL (collect-one :db/ident) (pred #(= (:formiguinhas/dominios-que-pode-referenciar %):enum/teste))] s))
                        => [:employee/employment-type])

                  (fact "2. gera corretamente os metadados que podem referenciar enums"
                        (select [ALL FIRST]
                                (select [ALL ALL (collect-one :db/ident) (must :formiguinhas/dominios-que-pode-referenciar)] s))
                        => [:employee/employment-type :employee/status])

                  (fact "Gera corretamente ps dominios de enums na primeira posicao do vetor de transacoes"
                        (set (select [FIRST ALL :db/ident] s))
                        => #{:enum/teste :enum/estado-do-workflow})

                  (fact "1.Gera corretamente atributos com propreidade de restricao de persistencia "
                        (select-one [(nthpath 1) ALL (collect-one :db/ident) (must :formiguinhas/restricoes-persistencia)] s)
                        => [:employee/tupla "[[?eid :model/attr]]"])))))
(deftest test-expansion-ocotopus-2

  (facts "Diferencas entre ocotopus e core"
         (let [s (schema->datomic '[^{:datomic/index true
                                      :datomic/tag   true}
                                    default
                                    ^{:datomic/tag-recursive  true
                                      :spec/tag-recursive     true
                                      :graphviz/tag-recursive true}

                                    ^{:interface true}
                                    Workflow
                                    [^{:type Estados-Workflow, :cardinality 1, :optional false
                                       :model.attr/persiste-estado-workflow? true
                                       :doc  "Estado da máquina de estados associada à entidade"} status
                                     ^{:type String, :cardinality 1, :optional false,
                                       :doc  "Identificador do workflow - namespace completo"} ident]

                                    Person
                                    [^{:type                       String
                                       :model.attr/apenas-runtime? false}
                                     name
                                     ^Estado-Workflow-Person status]



                                    Employee
                                    [
                                     ^String name
                                     ^{:datomic/type          :db.type/tuple
                                       :datomic/tupleType     :db/long
                                       :model.attr/persisted? true} tupla
                                     ^{:type             String
                                       :doc              "The very employee number of this employee"
                                       :datomic/unique   :db.unique/identity
                                       :datomic/fulltext false}
                                     number
                                     ^EmploymentType employment-type
                                     ^Estado-Workflow-Employee status
                                     ^SearchResult last-search-results]

                                    ^{:union true}
                                    SearchResult
                                    [Employee Person EmploymentType]

                                    ^{:enum               true
                                      :model.attr/dominio :enum/employment-type}
                                    EmploymentType
                                    [FULL_TIME
                                     ^{:doc "Documented enum"}
                                     PART_TIME]

                                    ^{:enum               true
                                      :model.attr/dominio :enum/estado-workflow.person}
                                    Estado-Workflow-Person
                                    [ACEITO]

                                    ^{:enum               true
                                      :model.attr/dominio :enum/estado-workflow.employee}
                                    Estado-Workflow-Employee
                                    [ACEITO]

                                    ^{:enum               true
                                      :union              true
                                      :model.attr/dominio :enum/estados-workflow}
                                    Estados-Workflow
                                    [Estado-Workflow.Employee Estado-Workflow.Person]])]



           (facts "Geração de enums hierarquicos."


                  (fact "0. gera corretamente os enums, na posicao correta, juntos com os atributos comuns, e com as referencias do dominio "
                        (set (select [ALL FIRST]
                                     (select [(nthpath 1) ALL (collect-one :db/ident) (must :formiguinhas/dominio)] s)))
                        => #{:employment-type/full-time
                             :employment-type/part-time
                             :estado-workflow-employee/aceito
                             :estado-workflow-person/aceito})

                  (fact "1. gera corretamente os enums, na posicao correta, juntos com os atributos comuns, e com as referencias do dominio "
                        (set (select [ALL FIRST]
                                     (select [(nthpath 1) ALL (collect-one :db/ident) (pred #(= (:formiguinhas/dominio %) :enum/estados-workflow))] s)))
                        => #{} #_#{:estado-workflow-employee/aceito
                                   :estado-workflow-person/aceito})

                  (fact "2. gera corretamente os enums, na posicao correta, juntos com os atributos comuns, e com as referencias do dominio "
                        (select [ALL FIRST]
                                (select [(nthpath 1) ALL (collect-one :db/ident) (pred #(= (:formiguinhas/dominio %) [ :enum/employment-type]))] s))
                        => [:employment-type/full-time :employment-type/part-time])

                  (fact "3. gera corretamente os enums, na posicao correta, juntos com os atributos comuns, e com as referencias do dominio "
                        (select [ALL FIRST]
                                (select [(nthpath 1) ALL (collect-one :db/ident) (pred #(= (:formiguinhas/dominio %) [ :enum/estado-workflow.employee]))] s))
                        => [:estado-workflow-employee/aceito])

                  (fact "4. gera corretamente os enums, na posicao correta, juntos com os atributos comuns, e com as referencias do dominio "
                        (select [ALL FIRST]
                                (select [(nthpath 1) ALL (collect-one :db/ident) (pred #(= (:formiguinhas/dominio %) [ :enum/estado-workflow.person]))] s))
                        => [:estado-workflow-person/aceito])


                  (fact "2. gera corretamente os metadados que podem referenciar enums"
                        (set (select [ALL FIRST]
                                     (select [ALL ALL (collect-one :db/ident) (must :formiguinhas/dominios-que-pode-referenciar)] s)))
                        => #{:employee/employment-type :employee/status :person/status :workflow/status})

                  (fact "1. gera corretamente os metadados que podem referenciar enums"
                        (select [ALL FIRST]
                                (select [ALL ALL (collect-one :db/ident) (pred #(= (:formiguinhas/dominios-que-pode-referenciar %) :enum/estados-workflow))] s))
                        => [:workflow/status])

                  (fact "Gera corretamente ps dominios de enums na primeira posicao do vetor de transacoes"
                        (set (select [FIRST ALL :db/ident] s))
                        => #{:enum/employment-type :enum/estado-workflow.employee :enum/estado-workflow.person :enum/estados-workflow})))))





