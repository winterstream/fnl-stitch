;;; Stitch behavior tests, adapted from Integrant's core suite.

(local t (require :faith))
(local st (require :stitch))
(local original-package-path package.path)
(local workflow st.reloaded-workflow)
(var workflow-callback nil)
(var deep-path-module nil)

(fn expect-error [reason func]
  (let [[ok? err] [(pcall func)]]
    (t.is (not ok?) "expected an error")
    (t.is (and (= :table (type err)) err.exception?)
              "expected a structured Stitch error")
    (t.= reason err.data.reason)
    err))

(fn expect-error-text [pattern func]
  (let [[ok? err] [(pcall func)]]
    (t.is (not ok?) "expected an error")
    (t.match pattern (tostring err))
    err))

(fn teardown []
  (st.log.set-writer nil)
  (set package.path original-package-path)
  (each [_ name (ipairs [:test-fixture
                         :test-fixture.component
                         :test-stitch-test
                         :test-test-modules
                         :test-test-modules.parent])]
    (tset package.loaded name nil))
  (when deep-path-module
    (tset package.loaded deep-path-module nil)
    (set deep-path-module nil))
  (when workflow-callback
    (workflow.off :after-system-change workflow-callback)
    (set workflow-callback nil))
  (when workflow.system
    (workflow.halt))
  (workflow.set-config nil))

(fn test-preconditions-keys []
  (expect-error-text "Precondition failed:" (fn [] (st.init false)))
  (expect-error-text "%(type %?keys%)" (fn [] (st.init {} false)))
  (t.is (st.valid-config-key? :test.component))
  (t.is (st.valid-config-key? [:test.group :test.component]))
  (t.is (not (st.valid-config-key? [:test|invalid])))
  (t.is (not (st.valid-config-key? 42)))
  (t.= :test.group|test.component
           (st.normalize-key [:test.group :test.component]))
  (expect-error :invalid-composite-key
                (fn [] (st.normalize-key [:test|invalid])))
  (values))

(fn test-annotations-refs []
  (local annotation-key :test.test.annotation)
  (local metadata {:doc "An annotation" :version 1})
  (st.annotate annotation-key metadata)
  (t.= metadata (st.describe annotation-key))
  (t.= nil (st.describe :test.test.unannotated))
  (local reference (st.ref :test.test.target))
  (t.is (st.ref? reference))
  (t.is (st.reflike? reference))
  (t.is (not (st.refset? reference)))
  (t.= :test.test.target reference.key)
  (local reference-set (st.refset [:test.test.group :test.test.target]))
  (t.is (st.refset? reference-set))
  (t.is (st.reflike? reference-set))
  (t.is (not (st.ref? reference-set)))
  (t.= :test.test.group|test.test.target reference-set.key)
  (values))

(fn test-hierarchy-lookups []
  (local parent :test.derived.parent)
  (local child :test.derived.child)
  (local root :test.derived.root)
  (st.derive child parent)
  (st.derive parent root)
  (t.is (st.isa? child root))
  (t.is (not (st.isa? root child)))
  ;; Composite keys inherit from each component.
  (st.derive :test.kind.child :test.kind.parent)
  (t.is (st.isa? :test.scope|test.kind.child
                     :test.scope|test.kind.parent))
  ;; Derived lookup returns every matching key, while the singular form rejects
  ;; ambiguity and returns nil for a missing key.
  (local left :test.lookup.left)
  (local right :test.lookup.right)
  (local lookup-parent :test.lookup.parent)
  (st.derive left lookup-parent)
  (st.derive right lookup-parent)
  (local config {})
  (tset config left 1)
  (tset config right 2)
  (local found {})
  (each [key value (st.find-derived config lookup-parent)]
    (tset found key value))
  (local expected {})
  (tset expected left 1)
  (tset expected right 2)
  (t.= expected found)
  (t.= nil (st.find-derived-1 {} lookup-parent))
  (expect-error :ambiguous-key (fn [] (st.find-derived-1 config lookup-parent)))
  (local unique-key :test.lookup.unique)
  (st.derive unique-key lookup-parent)
  (local unique-config {})
  (tset unique-config unique-key 3)
  (local [matching-key value] [(st.find-derived-1 unique-config lookup-parent)])
  (t.= unique-key matching-key)
  (t.= 3 value)
  ;; Composite references match a key derived from every component.
  (local composite :test.lookup.scope|test.lookup.service)
  (local composite-config {})
  (tset composite-config composite :found)
  (t.= :found
           (select 2
                   (st.find-derived-1 composite-config
                                      :test.lookup.scope|test.lookup.service)))
  (values))

(fn test-dependency-order []
  (local consumer :test.graph.consumer)
  (local hard :test.graph.hard)
  (local passive :test.graph.passive)
  (local config {})
  (tset config consumer {:hard (st.ref hard) :passive (st.refset passive)})
  (tset config hard 1)
  (tset config passive 2)
  (local active (st.dependency-graph config true))
  (local inactive (st.dependency-graph config false))
  (local roots {})
  (tset roots consumer true)
  (local active-dependencies (active:transitive-dependencies-set roots))
  (local inactive-dependencies (inactive:transitive-dependencies-set roots))
  (t.is (. active-dependencies hard))
  (t.is (. active-dependencies passive))
  (t.is (. inactive-dependencies hard))
  (t.is (not (. inactive-dependencies passive)))
  (t.is (. (active:immediate-dependents hard) consumer))
  (expect-error :invalid-node
                (fn [] (active:immediate-dependents :test.graph.missing)))
  (local order-config {})
  (tset order-config consumer (st.ref hard))
  (tset order-config hard 1)
  (tset order-config :test.graph.other 2)
  (local graph (st.dependency-graph order-config nil))
  (local keys [consumer hard :test.graph.other])
  (table.sort keys (st.key-comparator graph keys))
  (local positions (collect [index key (ipairs keys)] key index))
  (t.is (< (. positions hard) (. positions consumer)))
  (t.= "Graph[\n]" (tostring (st.dependency-graph {} nil)))
  (values))

(fn test-cycle-detection []
  (local depth 256)
  (local root :test.depth.root)
  (local leaf (faccumulate [parent root index 1 depth]
                (let [key (string.format "test.depth.%03d" index)]
                  (st.derive key parent)
                  key)))
  (t.is (st.isa? leaf root))
  (local config {})
  (local last-key (faccumulate [previous nil index 1 depth]
                    (let [key (string.format "test.graph.%03d" index)]
                      (tset config key
                            (if previous {:dependency (st.ref previous)} {}))
                      key)))
  (local order (st.init-ordered-dependency-keys config [last-key]))
  (t.= depth (length order))
  (t.= :test.graph.001 (. order 1))
  (t.= last-key (. order depth))
  ;; The graph reports the shortest cycle, not the first one encountered.
  (local cycles {})
  (tset cycles :test.cycle.long.a (st.ref :test.cycle.long.b))
  (tset cycles :test.cycle.long.b (st.ref :test.cycle.long.c))
  (tset cycles :test.cycle.long.c (st.ref :test.cycle.long.a))
  (tset cycles :test.cycle.short.a (st.ref :test.cycle.short.b))
  (tset cycles :test.cycle.short.b (st.ref :test.cycle.short.a))
  (local err
         (expect-error :circular-dependency
                       (fn [] (st.dependency-graph cycles nil))))
  (t.= [:test.cycle.short.a :test.cycle.short.b :test.cycle.short.a]
           err.data.cycle)
  (values))

(fn test-converge-and-expand []
  (t.= {:x 1 :y 2} (st.converge {:first {:x 1} :second {:y 2}}))
  (t.= {:server {:port 8080 :host :example.test :enabled true}}
           (st.converge {:first {:server {:port 8080 :host :localhost}}
                         :second {:server {:enabled true :host :internal}}}
                        {:server {:host :example.test}}))
  (t.= {:x 2} (st.converge {:first {} :second {:x 1}} {:x 2}))
  (t.= {:shared 3} (st.converge {:first {:shared 1} :second {:shared 2}}
                                    {:shared 3}))
  (t.= {:server {:host :localhost}}
           (st.converge {:first {:server {:host :localhost}}} {:server {}}))
  (local replacement [:chosen])
  (t.identical replacement (. (st.converge {:first {:shared 1}
                                                :second {:shared 2}}
                                               {:shared replacement})
                                  :shared))
  ;; Separate expansion sources conflict even when their leaves are equal.
  (expect-error :conflicting-expands
                (fn []
                  (st.converge {:first {:shared 1} :second {:shared 1}})))
  (expect-error :conflicting-expands
                (fn []
                  (st.converge {:first {:server {:port 8080}}
                                :second {:server {:port 8080}}})))
  (local nested-conflict
         (expect-error :conflicting-expands
                       (fn []
                         (st.converge {:first {:server {:port 80}}
                                       :second {:server {:port 443}}}))))
  (t.= [:server :port] (. nested-conflict.data :conflicting-index))
  (expect-error :conflicting-expands
                (fn []
                  (st.converge {:first {:x 1} :second {:x 2}})))
  (expect-error :conflicting-expands
                (fn []
                  (st.converge {:first {:server {:port 80}}
                                :second {:server {:port 443}}})))
  (expect-error :conflicting-expands
                (fn []
                  (st.converge {:first {:server {:host :localhost}}
                                :second {:server 3}})))
  (expect-error :conflicting-expands
                (fn []
                  (st.converge {:first {:server 3}
                                :second {:server {:host :localhost}}})))
  (t.= {:terminal 1} (st.expand {:terminal 1}))
  (st.expand-key:add-method :test.expand.single
                            (fn [_ value] {:component {:generated value}}))
  (local expanded (st.expand {:test.expand.single 7
                              :component {:override true}}))
  (t.= 7 (. (. expanded :component) :generated))
  (t.is (. (. expanded :component) :override))
  (st.expand-key:add-method :test.expand.wrapped
                            (fn [_ value] {:amount value}))
  (local wrapped
         (st.expand {:test.expand.wrapped 4}
                    (fn [result] {:amount (+ result.amount 1)})))
  (t.= 5 (. wrapped :amount))
  (st.expand-key:add-method :test.expand.empty (fn [_ _] {}))
  (local empty-expanded (st.expand {:test.expand.empty true :terminal 1}))
  (t.= 1 (. empty-expanded :terminal))
  ;; References survive expansion and profile selection can run inside it.
  (local dependency (st.ref :test.expand.dependency))
  (st.expand-key:add-method :test.expand.reference (fn [_ _] {: dependency}))
  (local reference-expansion (st.expand {:test.expand.reference true}))
  (t.is (st.ref? (. reference-expansion :dependency)))
  (st.expand-key:add-method :test.expand.profile
                            (fn [_ value]
                              {:config (st.profile {:dev value :prod 0})}))
  (local profiled-expansion
         (st.expand {:test.expand.profile 8} (st.deprofile [:dev])))
  (local {:config profile-config} profiled-expansion)
  (t.= 8 profile-config)
  ;; Conflicting methods are errors until a preference resolves the dispatch.
  (local method (st.method :test.expand.method (fn [_] nil)))
  (method:add-method :test.expand.left (fn [_] :left))
  (method:add-method :test.expand.right (fn [_] :right))
  (st.derive :test.expand.both :test.expand.left)
  (st.derive :test.expand.both :test.expand.right)
  (expect-error :ambiguous-match (fn [] (method:dispatch :test.expand.both)))
  (method:prefer :test.expand.left :test.expand.right)
  (t.= :left (method:dispatch :test.expand.both))
  (values))

(fn test-init-references []
  (local dependency :test.init.dependency)
  (local consumer :test.init.consumer)
  (local unrelated :test.init.unrelated)
  (st.register dependency {:init (fn [_ value] (* value 2))})
  (st.register consumer {:init (fn [_ value] value.dependency)})
  (st.register unrelated {:init (fn [_ value] value)})
  (local config {})
  (tset config dependency 4)
  (tset config consumer {:dependency (st.ref dependency)})
  (tset config unrelated 9)
  (local system (st.init config))
  (t.= 8 (. system dependency))
  (t.= 8 (. system consumer))
  ;; Selecting a component initializes its hard dependencies, not unrelated keys.
  (local selected (st.init config [consumer]))
  (t.= 8 (. selected dependency))
  (t.= 8 (. selected consumer))
  (t.= nil (. selected unrelated))
  (local dependent-selection
         (st.init config [dependency] {:include-transitive-dependents true}))
  (t.= 8 (. dependent-selection consumer))
  (t.= nil (. dependent-selection unrelated))
  ;; Selecting a derived key initializes the concrete matching key.
  (local concrete :test.init.concrete)
  (local abstract :test.init.abstract)
  (st.derive concrete abstract)
  (st.register concrete {:init (fn [_ value] value)})
  (local derived-config {})
  (tset derived-config concrete 12)
  (local derived-system (st.init derived-config [abstract]))
  (t.= 12 (. derived-system concrete))
  ;; Invalid references outside the selected subgraph do not block its build.
  (local broken :test.init.broken)
  (local partial-config {})
  (tset partial-config dependency 5)
  (tset partial-config broken {:missing (st.ref :test.init.absent)})
  (t.= 10 (. (st.init partial-config [dependency]) dependency))
  (expect-error :missing-refs (fn [] (st.init partial-config)))
  (local ambiguous {})
  (local first :test.init.service.first)
  (local second :test.init.service.second)
  (local service :test.init.service)
  (st.derive first service)
  (st.derive second service)
  (tset ambiguous :test.init.ambiguous-user {:service (st.ref service)})
  (tset ambiguous first 1)
  (tset ambiguous second 2)
  (expect-error :ambiguous-key (fn [] (st.init ambiguous)))
  ;; A custom resolve method hides the implementation wrapper from references.
  (local resource :test.init.resource)
  (local resource-user :test.init.resource-user)
  (st.register resource
               {:init (fn [_ value] {:public value :private true})
                :resolve (fn [_ instance] instance.public)})
  (st.register resource-user {:init (fn [_ value] value.resource)})
  (local resolved-config {})
  (tset resolved-config resource 21)
  (tset resolved-config resource-user {:resource (st.ref resource)})
  (local resolved (st.init resolved-config))
  (t.= 21 (. resolved resource-user))
  ;; Composite component keys and references resolve through every component.
  (local group :test.composite.group)
  (local kind :test.composite.kind)
  (local implementation :test.composite.implementation)
  (local composite-provider [group kind implementation])
  (local composite-reference [group kind])
  (local composite-user :test.composite.user)
  (st.register composite-provider {:init (fn [_ value] value)})
  (st.register composite-user {:init (fn [_ value] value.provider)})
  (local composite-config {})
  (tset composite-config composite-provider 31)
  (tset composite-config composite-user
        {:provider (st.ref composite-reference)})
  (local composite-system (st.init composite-config))
  (t.= 31 (. composite-system composite-user))
  (expect-error :invalid-composite-key (fn [] (st.ref [:test.bad|part])))
  ;; A passing assertion hook does not alter the built value.
  (local valid :test.assert.valid)
  (st.register valid {:init (fn [_ value] value)})
  (st.assert-key:add-method valid
                            (fn [_ value]
                              (when (< value 0) (error "negative value"))))
  (local valid-config {})
  (tset valid-config valid 4)
  (t.= 4 (. (st.init valid-config) valid))
  ;; Missing initializer functions are structured errors.
  (local missing-init-error
         (expect-error :build-threw-exception
                       (fn [] (st.init {:test.no.such.initializer 1}))))
  (t.= :missing-init-key missing-init-error.cause.data.reason)
  ;; References used as table keys are resolved along with values.
  (local key-dependency :test.init.key-dependency)
  (local key-consumer :test.init.key-consumer)
  (st.register key-dependency {:init (fn [_ value] value)})
  (st.register key-consumer {:init (fn [_ value] value)})
  (local key-value {})
  (tset key-value (st.ref key-dependency) :mapped)
  (local key-config {})
  (tset key-config key-dependency :resolved)
  (tset key-config key-consumer key-value)
  (local key-system (st.init key-config))
  (t.= :mapped (. (. key-system key-consumer) :resolved))
  (values))

(fn test-refset-selection []
  (local base :test.refset.base)
  (local first :test.refset.first)
  (local second :test.refset.second)
  (local consumer :test.refset.consumer)
  (st.derive first base)
  (st.derive second base)
  (st.register first {:init (fn [_ value] value)})
  (st.register second {:init (fn [_ value] value)})
  (st.register consumer {:init (fn [_ value] value.values)})
  (local config {})
  (tset config consumer {:values (st.refset base)})
  (tset config first 1)
  (tset config second 2)
  ;; Refsets do not pull every match into an explicitly selected subgraph.
  (local empty-set-system (st.init config [consumer]))
  (t.= [] (. empty-set-system consumer))
  ;; Only initialized matches are included in a selected refset.
  (local one-system (st.init config [consumer first]))
  (t.= [1] (. one-system consumer))
  (local full-system (st.init config))
  (local resolved-values (. full-system consumer))
  (t.= 2 (length resolved-values))
  (local value-set (collect [_ value (ipairs resolved-values)] value true))
  (t.is (. value-set 1))
  (t.is (. value-set 2))
  (values))

(fn test-build-run-each-and-fold []
  (local dependency :test.build.dependency)
  (local consumer :test.build.consumer)
  (local config {})
  (tset config dependency 3)
  (tset config consumer {:dependency (st.ref dependency)})
  (local built (st.build config [consumer] (fn [_ value] value)
                         (fn [_ _ _] nil) st.resolve-key))
  (t.= 3 (. built dependency))
  (t.= 3 (. (. built consumer) :dependency))
  (local a :test.run.a)
  (local b :test.run.b)
  (st.register a {:init (fn [_ value] value)})
  (st.register b {:init (fn [_ value] value)})
  (local run-config {})
  (tset run-config a 1)
  (tset run-config b (st.ref a))
  (local system (st.init run-config))
  (var visited [])
  (st.run system nil (fn [key _] (table.insert visited key)))
  (t.= [a b] visited)
  (set visited [])
  (st.reverse-run system nil (fn [key _] (table.insert visited key)))
  (t.= [b a] visited)
  ;; Selected keys are visited as given, ordered by system initialization.
  (set visited [])
  (st.run system [b] (fn [key _] (table.insert visited key)))
  (t.= [b] visited)
  (set visited [])
  (st.reverse-run system [a] (fn [key _] (table.insert visited key)))
  (t.= [a] visited)
  (local iterator (st.each system))
  (local [first-key first-value] [(iterator)])
  (local [second-key second-value] [(iterator)])
  (t.= a first-key)
  (t.= 1 first-value)
  (t.= b second-key)
  (t.= 1 second-value)
  (t.= nil (iterator))
  (t.= [[a 1] [b 1]] (st.fold system
                                  (fn [result key value]
                                    (doto result (table.insert [key value])))
                                  []))
  (values))

(fn test-halt-suspend-and-resume []
  (var events [])
  (local base :test.lifecycle.base)
  (local user :test.lifecycle.user)
  (st.register base
               {:init (fn [_ value] {: value})
                :halt (fn [key _] (table.insert events [:halt key]))
                :suspend (fn [key _] (table.insert events [:suspend key]))
                :resume (fn [key value old-config instance]
                          (table.insert events [:resume key value old-config])
                          (if (= value old-config) instance {: value}))})
  (st.register user
               {:init (fn [_ value] {:dependency value.dependency})
                :halt (fn [key _] (table.insert events [:halt key]))
                :suspend (fn [key _] (table.insert events [:suspend key]))
                :resume (fn [key value old-config instance]
                          (table.insert events [:resume key value old-config])
                          (if (rawequal value.dependency old-config.dependency)
                              instance
                              {:dependency value.dependency}))})
  (local config {})
  (tset config base 1)
  (tset config user {:dependency (st.ref base)})
  (local system (st.init config))
  (local old-base (. system base))
  (local old-user (. system user))
  (st.suspend system)
  (t.= [[:suspend user] [:suspend base]] events)
  (set events [])
  (local resumed (st.resume config system))
  (t.identical old-base (. resumed base))
  (t.identical old-user (. resumed user))
  (t.= [[:resume base 1 1]
            [:resume user {:dependency old-base} {:dependency old-base}]]
           events)
  ;; Halting a selection returns a copy and leaves the source system intact.
  (set events [])
  (local partial-halt (st.halt system [user]))
  (t.= [[:halt user]] events)
  (t.= nil (. partial-halt user))
  (t.identical old-user (. system user))
  ;; Halt and suspend visit only explicitly selected keys.
  (set events [])
  (local dependency-halt (st.halt system [base]))
  (t.= [[:halt base]] events)
  (t.identical old-user (. dependency-halt user))
  (t.= nil (. dependency-halt base))
  (set events [])
  (st.suspend system [base])
  (t.= [[:suspend base]] events)
  ;; Removed keys are halted during resume and omitted from the new system.
  (set events [])
  (local changed-config {})
  (tset changed-config base 2)
  (local changed (st.resume changed-config resumed))
  (t.is (= nil (. changed user)))
  (t.= 2 (. (. changed base) :value))
  (t.is (. events 1))
  (t.= :halt (. (. events 1) 1))
  (t.= user (. (. events 1) 2))
  ;; The default suspend method delegates to halt.
  (local fallback :test.lifecycle.default-suspend)
  (st.register fallback
               {:init (fn [_ value] value)
                :halt (fn [key _] (table.insert events [:halt key]))})
  (local fallback-config {})
  (tset fallback-config fallback 1)
  (local fallback-system (st.init fallback-config))
  (set events [])
  (st.suspend fallback-system)
  (t.= [[:halt fallback]] events)
  (values))

(fn test-resume-old-refs []
  (local events [])
  (local dependency :test.resume.dependency)
  (local consumer :test.resume.consumer)
  (st.register dependency
               {:init (fn [_ value] value)
                :halt (fn [key _] (table.insert events [:halt key]))})
  (st.register consumer {:init (fn [_ value] value.dependency)
                         :resume (fn [key value old-config _]
                                   (table.insert events
                                                 [:resume key value old-config])
                                   value)})
  (local original-config {})
  (tset original-config dependency 1)
  (tset original-config consumer {:dependency (st.ref dependency)})
  (local original (st.init original-config))
  (local new-config {})
  (tset new-config consumer [])
  (local resumed (st.resume new-config original))
  (t.= [[:halt dependency] [:resume consumer [] {:dependency 1}]] events)
  (t.= [] (. resumed consumer))
  (t.= nil (. resumed dependency))
  (values))

(fn test-wrapped-errors []
  (local failing-init :test.error.init)
  (st.register failing-init {:init (fn [_ _] (error "initializer failed"))})
  (local failing-config {})
  (tset failing-config failing-init 1)
  (local build-error
         (expect-error :build-threw-exception (fn [] (st.init failing-config))))
  (t.= failing-init build-error.data.key)
  (t.is build-error.cause.exception?)
  (t.= :unhandled-native-error build-error.cause.data.type)
  (t.match "initializer failed" build-error.cause.message)
  (local a :test.error.run.a)
  (local b :test.error.run.b)
  (local c :test.error.run.c)
  (each [_ key (ipairs [a b c])]
    (st.register key {:init (fn [_ value] value)}))
  (local run-config {})
  (tset run-config a 1)
  (tset run-config b 2)
  (tset run-config c 3)
  (local system (st.init run-config))
  (local run-error
         (expect-error :run-threw-exception
                       (fn []
                         (st.run system nil
                                 (fn [key _]
                                   (when (= key b) (error "runner failed")))))))
  (t.= [a] run-error.data.completed-keys)
  (t.= [c] run-error.data.remaining-keys)
  (t.= b run-error.data.key)
  (t.match "runner failed" run-error.cause.message)
  (local asserted :test.error.asserted)
  (st.assert-key:add-method asserted
                            (fn [_ _] (error "assertion rejected config")))
  (local asserted-config {})
  (tset asserted-config asserted 1)
  (local assert-error
         (expect-error :build-failed-spec (fn [] (st.init asserted-config))))
  (t.= asserted assert-error.data.key)
  (t.match "assertion rejected config" assert-error.cause.message)
  (local failed-halt :test.error.halt)
  (st.register failed-halt
               {:init (fn [_ value] value)
                :halt (fn [_ _] (error "halt failed"))})
  (st.log.set-writer (fn [_ _] nil))
  (local halt-config {})
  (tset halt-config failed-halt 1)
  (local halt-system (st.init halt-config))
  (local halt-error
         (expect-error :halt-threw-exception (fn [] (st.halt halt-system))))
  (t.= 1 (length halt-error.data.errors))
  (t.= failed-halt (. (. halt-error.data.errors 1) :key))
  (t.is (. halt-system failed-halt))
  (values))

(fn test-profiles-and-vars []
  (local profile (st.profile {:dev {:port 8080}
                              :test {:port 8000}
                              :prod {:port 80}}))
  (t.= {:port 8000} (st.deprofile profile [:missing :test :dev]))
  (t.= {:server {:port 8080}} (st.deprofile {:server profile} [:dev]))
  (t.= nil (st.deprofile (st.profile {:dev st.NIL}) [:dev]))
  (expect-error-text "Missing a valid key for profile"
                     (fn [] (st.deprofile profile [:missing])))
  (local input {:path (st.var :root) :nested {:owner (st.var :owner)}})
  (t.= {:path :/srv/app :nested {:owner :Ada}}
           (st.bind input {:root :/srv/app :owner :Ada}))
  (t.is (st.var? (. input :path)))
  (t.is (st.var? (. input :path)))
  (t.is (st.var? (. (st.bind {:missing (st.var :missing)} {}) :missing)))
  ;; Variables in table keys are substituted as well as variables in values.
  (local keyed {})
  (tset keyed (st.var :destination) :mapped)
  (local bound (st.bind keyed {:destination :resolved}))
  (t.= :mapped (. bound :resolved))
  (t.= nil (. bound (st.var :destination)))
  (expect-error :unbound-vars
                (fn []
                  (st.init {:test.vars.unbound {:first (st.var :first)
                                                 :second (st.var :second)}})))
  (st.expand-key:add-method :test.vars.module
                            (fn [_ name]
                              {:service {:port (st.var name)}}))
  (local expanded (st.expand {:test.vars.module :port}))
  (local bound-expansion (st.bind expanded {:port 8080}))
  (t.= 8080 (. (. bound-expansion :service) :port))
  (values))

(fn test-loaders []
  (st.log.set-writer (fn [_ _] nil))
  (set package.path (.. package.path ";./test/fixtures/?.lua"))
  (st.load-hierarchy :stitch.hierarchy)
  (t.is (st.isa? :test.fixture.child :test.fixture.parent))
  (t.is (st.isa? :test.fixture.child :test.fixture.other))
  (st.load-annotations :stitch.annotations)
  (t.= {:doc "Loaded from a fixture."}
           (st.describe :test.fixture.annotation))
  (t.= {:test.config.value 42} (st.read-config :test/fixtures/config.lua))
  ;; The loader registers lifecycle functions from modules found by key path.
  (local module-name :test-fixture.component)
  (local module {:init (fn [_ value] (* value 3))})
  (tset package.loaded module-name module)
  (set package.loaded.test-fixture {})
  (local module-config {})
  (tset module-config module-name 7)
  (local loaded (st.load-modules module-config))
  (local loaded-set (collect [_ name (ipairs loaded)] name true))
  (t.is (. loaded-set module-name))
  (local system (st.init module-config))
  (t.= 21 (. system module-name))
  ;; Selected loading includes a component's hierarchy ancestors only.
  (local child :test-test-modules.child)
  (local parent :test-test-modules.parent)
  (st.derive child parent)
  (set package.loaded.test-test-modules {})
  (tset package.loaded parent {:init (fn [_ value] (+ value 10))})
  (local selected-config {})
  (tset selected-config child 5)
  (tset selected-config :test-unused.component 99)
  (local selected-modules (st.load-modules selected-config [child]))
  (local selected-module-set (collect [_ name (ipairs selected-modules)] name
                               true))
  (t.is (. selected-module-set parent))
  (t.is (not (. selected-module-set :test-unused)))
  (t.is (not (. selected-module-set :test-unused)))
  (set package.loaded.test-unused {})
  (tset package.loaded :test-unused.component {:init (fn [_ value] value)})
  (local all-modules (st.load-modules selected-config))
  (local all-module-set (collect [_ name (ipairs all-modules)] name true))
  (t.is (. all-module-set parent))
  (t.is (. all-module-set :test-unused.component))
  (local inherited-system (st.init selected-config [child]))
  (t.= 15 (. inherited-system child))
  ;; The default init method can resolve a dotted function path.
  (set package.loaded.test-stitch-test {:component {:init (fn [value] value)}})
  (local function-key :test-stitch-test.component.init)
  (t.= :function (type (st.registry.find-var function-key)))
  (local function-config {})
  (tset function-config function-key {:answer 42})
  (local function-system (st.init function-config))
  (t.= 42 (. (. function-system function-key) :answer))
  (values))

(fn test-clone-dotted-halt []
  (local clone-events [])
  (local clone-meta
         {:__newindex (fn [target key value]
                        (table.insert clone-events key)
                        (rawset target key value))})
  (local clone-input (setmetatable {:value 7} clone-meta))
  (local clone-key :test.clone.metatable)
  (st.register clone-key {:init (fn [_ value] value)})
  (local clone-config {})
  (tset clone-config clone-key clone-input)
  (st.init clone-config)
  (t.= [:value] clone-events)
  (local dotted :test.halt.original)
  (local sibling :test.halt.sibling)
  (st.register dotted {:init (fn [_ value] value)})
  (st.register sibling {:init (fn [_ value] value)})
  (local dotted-config {})
  (tset dotted-config dotted 7)
  (tset dotted-config sibling 8)
  (local original (st.init dotted-config))
  (local halted (st.halt original [dotted]))
  (t.= 7 (. original dotted))
  (t.= 8 (. original sibling))
  (t.= nil (. halted dotted))
  (t.= 8 (. halted sibling))
  ;; Removing a very deep dotted path is iterative, not recursive.
  (set deep-path-module (.. :test.deep. (string.rep :node. 32768) :leaf))
  (tset package.loaded deep-path-module (fn [value] value))
  (local deep-config {})
  (tset deep-config deep-path-module true)
  (local deep-system (st.init deep-config))
  (local deep-halted (st.halt deep-system))
  (t.= nil (. deep-halted :test))
  (values))

(fn test-reloaded-workflow []
  (when workflow.system (workflow.halt))
  (local events [])
  (set workflow-callback (fn [payload] (table.insert events payload.op)))
  (workflow.on :after-system-change workflow-callback)
  (local dependency :test.workflow.dependency)
  (local consumer :test.workflow.consumer)
  (st.register dependency {:init (fn [_ value] (* value 2))})
  (st.register consumer {:init (fn [_ value] value.dependency)})
  (local config {})
  (tset config dependency 4)
  (tset config consumer {:dependency (st.ref dependency)})
  (workflow.set-config config)
  (local system (workflow.go))
  (t.= 8 (. system consumer))
  (workflow.halt)
  (t.= [:halt] events)
  (values))

{:setup nil
 : teardown
 : test-preconditions-keys
 : test-annotations-refs
 : test-hierarchy-lookups
 : test-dependency-order
 : test-cycle-detection
 : test-converge-and-expand
 : test-init-references
 : test-refset-selection
 : test-build-run-each-and-fold
 : test-halt-suspend-and-resume
 : test-resume-old-refs
 : test-wrapped-errors
 : test-profiles-and-vars
 : test-loaders
 : test-clone-dotted-halt
 : test-reloaded-workflow}
