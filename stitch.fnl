;;; Single-file Fennel port of Stitch.
;;; Copyright (c) 2016-2018 James Reeves
;;; Copyright (c) 2026 Wynand Winterbach
;;; Copyright (c) Rich Hickey & Wynand Winterbach
;;; Multi-method code is governed by EPL-1.0.
;;; All other code is MIT-licensed; see LICENSE.

(local fennel (require :fennel))
(local exception {})
(local log-state {:writer nil :level :debug})
(local annotation-registry {})
(local levels {:debug 1 :info 2 :warn 3 :error 4})

;;; Expand runtime preconditions into readable assertion failures.

(macro precondition [& conditions]
  (assert-compile (< 0 (length conditions))
                  "expected at least one precondition" conditions)
  `(do
     ,(unpack (icollect [_ condition (ipairs conditions)]
                `(assert ,condition
                         ,(.. "Precondition failed: " (view condition)))))))

(fn nil? [value]
  (= nil value))

;;; Structured errors

(local exception-meta {})
(set exception-meta.__index exception-meta)

(fn use-colors? []
  (and (not (os.getenv :NO_COLOR)) (not= :dumb (os.getenv :TERM))
       (os.getenv :TERM)))

(local colors
       (if (use-colors?)
           {:reset "\027[0m"
            :bold "\027[1m"
            :red "\027[31m"
            :cyan "\027[36m"
            :gray "\027[90m"
            :yellow "\027[33m"}
           {:reset "" :bold "" :red "" :cyan "" :gray "" :yellow ""}))

(fn inspect-value [value]
  (if (= :table (type value))
      (let [meta (getmetatable value)]
        (if (and (= :table (type meta)) meta.__tostring)
            (tostring value)
            (fennel.view value)))
      (tostring value)))

(fn exception-string [{: message : data :cause ?cause : trace}]
  (let [lines []
        rule (.. colors.gray (string.rep "─" 60) colors.reset)]
    (table.insert lines (.. "\n" colors.red colors.bold " ERROR " colors.reset))
    (table.insert lines (.. colors.bold message colors.reset))
    (table.insert lines rule)
    (when (next data)
      (table.insert lines (.. colors.cyan " Context:" colors.reset))
      (each [key value (pairs data)]
        (let [label (string.format "  %-12s" (.. (tostring key) ":"))]
          (table.insert lines
                        (.. colors.yellow label colors.reset
                            (inspect-value value))))))
    (when ?cause
      (table.insert lines rule)
      (table.insert lines (.. colors.cyan " Caused by: " colors.reset
                              (tostring ?cause))))
    (table.insert lines rule)
    (table.insert lines (.. colors.gray " " trace colors.reset))
    (table.insert lines rule)
    (table.concat lines "\n")))

(set exception-meta.__tostring exception-string)

(fn new-exception [message ?data ?cause]
  (let [err {: message :data (or ?data {}) :exception? true}]
    (setmetatable err exception-meta)
    (when ?cause
      (set err.cause (exception.to-exc ?cause)))
    (set err.trace (string.gsub (debug.traceback "" 3) "^%s*\n" ""))
    err))

(fn exception.to-exc [err]
  (if (and (= :table (type err)) err.exception?)
      err
      (new-exception (tostring err) {:type :unhandled-native-error} nil)))

(macro protect [& body]
  `(xpcall (fn [] ,(unpack body)) exception.to-exc))

(fn exception-meta.reraise [{: message &as self}]
  (error (new-exception (.. message " (reraised)") {:reason :reraised} self)))

;;; Logging

(fn normalize-level [level]
  (let [normalized (string.lower (tostring level))
        normalized (if (= normalized :warning) :warn normalized)]
    (assert (. levels normalized) (.. "unknown log level: " (tostring level)))
    normalized))

(fn default-write [level name ...]
  (print (string.format "[%s] [%s]" (string.upper level) name) ...))

(set log-state.writer default-write)

(local logger-meta {})
(set logger-meta.__index logger-meta)

(fn logger-log [self level ...]
  (let [normalized (normalize-level level)]
    (when (<= (. levels log-state.level) (. levels normalized))
      (log-state.writer normalized self.name ...))))

(set logger-meta.log logger-log)
(set logger-meta.debug (fn [self ...] (logger-log self :debug ...)))
(set logger-meta.info (fn [self ...] (logger-log self :info ...)))
(set logger-meta.warn (fn [self ...] (logger-log self :warn ...)))
(set logger-meta.error (fn [self ...] (logger-log self :error ...)))

(local log {:new (fn [name] (setmetatable {: name} logger-meta))
            :use (fn [?writer]
                   (precondition (or (nil? ?writer)
                                     (= :function (type ?writer))))
                   (set log-state.writer (or ?writer default-write)))
            :set-level (fn [level]
                         (set log-state.level (normalize-level level)))
            :get-level (fn [] log-state.level)
            :set-writer (fn [?writer]
                          (precondition (or (nil? ?writer)
                                            (= :function (type ?writer))))
                          (set log-state.writer (or ?writer default-write)))
            : normalize-level})

;;; Annotation metadata

(fn annotate [key metadata]
  "Attach descriptive metadata to a component key."
  (precondition (= :table (type metadata)))
  (tset annotation-registry key metadata))

(fn describe [key]
  "Return metadata previously attached to a component key."
  (. annotation-registry key))

;;; Table and key helpers
;;; Table and key helpers

(fn keys-of [table]
  (icollect [key _ (pairs table)] key))

(fn empty-table? [table]
  (= nil (next table)))

(fn unset-in [root path]
  "Remove a nested key and prune empty tables on its path."
  (let [path-length (length path)]
    (let [ancestors []]
      (fn prune [index]
        (if (= 0 index)
            root
            (let [[parent key child] (. ancestors index)]
              (when (empty-table? child)
                (tset parent key nil))
              (prune (- index 1)))))

      (fn descend [node index]
        (let [key (. path index)
              child (. node key)]
          (if (= index path-length)
              (do
                (tset node key nil)
                (prune (length ancestors)))
              (if (= :table (type child))
                  (do
                    (table.insert ancestors [node key child])
                    (descend child (+ index 1)))
                  root))))

      (if (< 0 path-length)
          (descend root 1)
          root))))

;;; Classify dense integer-keyed tables as sequences for tree walking.

(fn array-table? [value]
  (let [count (accumulate [count 0 _ (pairs value)] (+ count 1))
        valid-keys? (accumulate [valid? true key _ (pairs value)
                                 &until (not valid?)]
                      (and (= :number (type key)) (<= 1 key) (= 0 (% key 1))))]
    (and valid-keys? (= count (length value)))))

(fn postwalk [func value]
  (if (or (not= :table (type value)) (getmetatable value))
      (func value)
      (func (collect [key child (pairs value)]
              (if (= :table (type key))
                  (postwalk func key)
                  key)
              (postwalk func child)))))

(fn collect-values [tree ?predicate]
  (let [result []]
    (fn walk [node]
      (if (and (= :table (type node)) (not (getmetatable node)))
          (let [walk-keys? (and ?predicate (not (array-table? node)))]
            (each [key child (pairs node)]
              (when walk-keys? (walk key))
              (walk child)))
          (when (or (nil? ?predicate) (?predicate node))
            (table.insert result node))))

    (walk tree)
    result))

(fn array-comparator [order ?fallback]
  (let [positions (collect [index value (ipairs order)] value index)
        fallback (or ?fallback (fn [_ _] false))]
    (fn [left right]
      (let [left-index (. positions left)
            right-index (. positions right)]
        (if (and left-index right-index)
            (< left-index right-index)
            (if (not= left-index right-index)
                (not (nil? left-index))
                (fallback left right)))))))

(fn reversed [items]
  (let [result []]
    (for [index (length items) 1 -1]
      (table.insert result (. items index)))
    result))

(fn split-dotted [text]
  (icollect [part (string.gmatch text "[^%.]+")] part))

(fn dotted-prefixes [text]
  (let [parts (split-dotted text)]
    (icollect [index _ (ipairs parts)]
      (table.concat (icollect [i part (ipairs parts) &until (< index i)] part)
                    "."))))

(fn clone [value]
  (if (= :table (type value))
      (collect [key child (pairs value)] key child)
      value))

(fn deep-clone [value]
  (let [visited {}]
    (fn copy [object]
      (if (not= :table (type object))
          object
          (case (. visited object)
            cached cached
            _ (let [result {}]
                (let [meta (getmetatable object)]
                  (when meta (setmetatable result meta)))
                (tset visited object result)
                (each [key child (pairs object)]
                  (tset result key (copy child)))
                result))))

    (copy value)))

(fn split-composite-key [value]
  (if (or (not= :string (type value)) (not (string.find value "|" 1 true)))
      nil
      (icollect [part (string.gmatch value "([^|]+)")] part)))

;;; Qualified and compound keys

(fn composite-key? [key]
  (if (or (not= :table (type key)) (= 0 (length key)))
      false
      (accumulate [valid? true _ part (ipairs key) &until (not valid?)]
        (and valid? (= :string (type part))))))

(fn valid-config-key? [key]
  (if (= :string (type key))
      true
      (if (not (composite-key? key))
          false
          (accumulate [valid? true _ part (ipairs key) &until (not valid?)]
            (and valid? (not (string.find part "|" 1 true)))))))

(fn normalize-key [key]
  "Normalize a string or component list to a qualified key."
  (if (= :string (type key))
      key
      (if (not= :table (type key))
          (error (new-exception (.. (tostring key)
                                    " is neither a qualified key nor a composite key")
                                {:reason :key-not-qualified-nor-composite
                                 : key}))
          (if (= 0 (length key))
              (error (new-exception (.. (tostring key)
                                        " is neither a qualified key nor a composite key")
                                    {:reason :key-not-qualified-nor-composite
                                     : key}))
              (let [invalid (accumulate [invalid nil _ part (ipairs key)
                                         &until invalid]
                              (if (or (not= :string (type part))
                                      (string.find part "|" 1 true))
                                  [true part]
                                  nil))]
                (when invalid
                  (let [[_ invalid-part] invalid]
                    (error (new-exception (.. "Composite key : " (tostring key)
                                              " contains an invalid part : "
                                              (tostring invalid-part))
                                          {:reason :invalid-composite-key
                                           : key
                                           : invalid-part}))))
                (table.concat key "|"))))))

;;; Multiple inheritance hierarchy

(local hierarchy-meta {})
(set hierarchy-meta.__index hierarchy-meta)

(fn hierarchy-new []
  (setmetatable {:_parents {} :_ancestors {} :_is-a-cache {} :version 0}
                hierarchy-meta))

(fn hierarchy-reset-caches [self]
  (set self._ancestors {})
  (set self._is-a-cache {}))

(fn hierarchy-meta.clear [self]
  (set self._parents {})
  (hierarchy-reset-caches self)
  (set self.version 0))

(fn hierarchy-invalidate [self]
  (hierarchy-reset-caches self)
  (set self.version (+ self.version 1)))

(fn hierarchy-direct-parents [self node]
  (let [parents (clone (or (. self._parents node) []))]
    (each [_ parent (ipairs (or (split-composite-key node) []))]
      (table.insert parents parent))
    parents))

(fn hierarchy-traverse [self node callback]
  (let [queue [node]
        visited {[node] true}]
    (fn traverse [head]
      (if (< (length queue) head)
          false
          (let [current (. queue head)
                stopped? (accumulate [stopped? false _ parent (ipairs (hierarchy-direct-parents self
                                                                                                current))
                                      &until stopped?]
                           (if (. visited parent)
                               stopped?
                               (do
                                 (tset visited parent true)
                                 (table.insert queue parent)
                                 (callback parent))))]
            (if stopped? true
                (traverse (+ head 1))))))

    (traverse 1)))

(fn hierarchy-has-path? [self start target]
  (if (= start target)
      true
      (hierarchy-traverse self start (fn [parent] (= parent target)))))

(fn hierarchy-meta.derive [{:_parents parents-by-node &as self} child parent]
  (when (= child parent)
    (error (new-exception (.. "Cyclic dependency: Cannot derive "
                              (tostring child) " from itself.")
                          {:reason :cyclic-dependency : child : parent})))
  (let [cached-parents (. parents-by-node child)
        parents (or cached-parents [])]
    (when (nil? cached-parents)
      (tset parents-by-node child parents))
    (let [duplicate? (accumulate [duplicate? false _ existing (ipairs parents)
                                  &until duplicate?]
                       (or duplicate? (= existing parent)))]
      (when (not duplicate?)
        (when (hierarchy-has-path? self parent child)
          (error (new-exception (.. "Cyclic dependency detected: "
                                    (tostring parent) " already inherits from "
                                    (tostring child))
                                {:reason :cyclic-dependency : child : parent})))
        (table.insert parents parent)
        (hierarchy-invalidate self)))))

(fn hierarchy-meta.get-ancestors [{:_ancestors ancestors-by-node &as self}
                                  node]
  (case (. ancestors-by-node node)
    ancestors ancestors
    _ (let [ancestors {}]
        (hierarchy-traverse self node
                            (fn [parent]
                              (tset ancestors parent true)
                              false))
        (tset ancestors-by-node node ancestors)
        ancestors)))

(fn hierarchy-compute-isa? [self child parent]
  (let [components (split-composite-key parent)]
    (if components
        (accumulate [matches? true _ component (ipairs components)
                     &until (not matches?)]
          (and matches? (hierarchy-meta.isa? self child component)))
        (not (nil? (. (hierarchy-meta.get-ancestors self child) parent))))))

(fn hierarchy-meta.isa? [{:_is-a-cache is-a-cache &as self} child parent]
  (if (= child parent)
      true
      (let [cached (. is-a-cache child)]
        (case (if cached (. cached parent))
          result result
          _ (let [result (hierarchy-compute-isa? self child parent)
                  child-cache (or cached {})]
              (tset child-cache parent result)
              (tset is-a-cache child child-cache)
              result)))))

(local global-hierarchy (hierarchy-new))
(local hierarchy {:new hierarchy-new
                  :derive (fn [child parent]
                            (hierarchy-meta.derive global-hierarchy child
                                                   parent))
                  :isa? (fn [child parent]
                          (hierarchy-meta.isa? global-hierarchy child parent))
                  :get-ancestors (fn [node]
                                   (hierarchy-meta.get-ancestors global-hierarchy
                                                                 node))
                  :clear (fn [] (hierarchy-meta.clear global-hierarchy))
                  :global global-hierarchy})

;;; Dependency graph

(local graph-meta {})
(set graph-meta.__index graph-meta)

(fn graph-new [?dependencies ?dependents]
  (setmetatable {:_dependencies (or ?dependencies {})
                 :_dependents (or ?dependents {})} graph-meta))

(fn graph-transitive [roots expand]
  (let [seen {}
        queue []]
    (each [root _ (pairs roots)]
      (each [neighbor _ (pairs (or (expand root) {}))]
        (when (not (. seen neighbor))
          (tset seen neighbor true)
          (table.insert queue neighbor))))

    (fn visit [head]
      (if (< (length queue) head)
          seen
          (let [node (. queue head)]
            (each [neighbor _ (pairs (or (expand node) {}))]
              (when (not (. seen neighbor))
                (tset seen neighbor true)
                (table.insert queue neighbor)))
            (visit (+ head 1)))))

    (visit 1)))

(fn graph-cycle-from [start dependencies ?max-length]
  (let [queue [[start [start]]]
        visited {}]
    (fn visit-neighbor [neighbor path]
      (if (= neighbor start)
          (let [cycle (clone path)]
            (table.insert cycle neighbor)
            cycle)
          (if (or (. visited neighbor)
                  (and ?max-length (<= ?max-length (+ (length path) 2))))
              nil
              (do
                (tset visited neighbor true)
                (let [next-path (clone path)]
                  (table.insert next-path neighbor)
                  (table.insert queue [neighbor next-path])
                  nil)))))

    (fn search [head]
      (if (< (length queue) head)
          nil
          (let [[node path] (. queue head)]
            (if (and ?max-length (<= ?max-length (+ (length path) 1)))
                (search (+ head 1))
                (let [node-deps (or (. dependencies node) {})
                      cycle (accumulate [cycle nil neighbor _ (pairs node-deps)
                                         &until cycle]
                              (visit-neighbor neighbor path))]
                  (if cycle
                      cycle
                      (search (+ head 1))))))))

    (search 1)))

(fn graph-has-cycle? [dependencies dependents]
  (let [pending {}
        queue []
        node-count (accumulate [count 0 _ _node (pairs dependencies)]
                     (+ count 1))]
    (each [node node-dependencies (pairs dependencies)]
      (let [count (accumulate [count 0 _ (pairs node-dependencies)]
                    (+ count 1))]
        (tset pending node count)
        (when (= 0 count) (table.insert queue node))))

    (fn visit [head processed]
      (if (< (length queue) head)
          (< processed node-count)
          (let [node (. queue head)]
            (each [dependent _ (pairs (. dependents node))]
              (let [count (- (. pending dependent) 1)]
                (tset pending dependent count)
                (when (= 0 count) (table.insert queue dependent))))
            (visit (+ head 1) (+ processed 1)))))

    (visit 1 0)))

(fn graph-shortest-cycle [dependencies]
  (let [nodes (keys-of dependencies)]
    (table.sort nodes
                (fn [left right]
                  (< (tostring left) (tostring right))))
    (accumulate [best nil _ node (ipairs nodes)]
      ;; A self-loop is the shortest possible cycle.
      (if (and best (= 2 (length best)))
          best
          (let [cycle (graph-cycle-from node dependencies
                                        (if best (length best) nil))]
            (if (and cycle (or (nil? best) (< (length cycle) (length best))))
                cycle
                best))))))

(fn graph-verify-acyclic [dependencies dependents]
  (when (graph-has-cycle? dependencies dependents)
    (case (graph-shortest-cycle dependencies)
      cycle (error (new-exception (.. "Circular dependency detected: "
                                      (table.concat (icollect [_ node (ipairs cycle)]
                                                      (tostring node))
                                                    " -> "))
                                  {:reason :circular-dependency : cycle})))))

(fn graph-from-dependencies [builder]
  (let [dependencies {}
        dependents {}]
    (let [add-node (fn [node]
                     (when node
                       (when (nil? (. dependencies node))
                         (tset dependencies node {})
                         (tset dependents node {}))))]
      (let [add-edge (fn [node dependency]
                       (add-node node)
                       (when dependency
                         (add-node dependency)
                         (tset (. dependencies node) dependency true)
                         (tset (. dependents dependency) node true)))]
        (builder add-edge)))
    (graph-verify-acyclic dependencies dependents)
    (graph-new dependencies dependents)))

(fn graph-string [{:_dependencies dependencies}]
  (let [lines ["Graph[\n"]]
    (each [node neighbors (pairs dependencies)]
      (if (empty-table? neighbors)
          (table.insert lines
                        (string.format "  %s (no out edges)\n" (tostring node)))
          (each [neighbor _ (pairs neighbors)]
            (table.insert lines
                          (string.format "  %s --> %s\n" (tostring node)
                                         (tostring neighbor))))))
    (table.insert lines "]")
    (table.concat lines "")))

(set graph-meta.__tostring graph-string)

(fn graph-verify-node [{:_dependencies dependencies &as self} node]
  (when (nil? (. dependencies node))
    (error (new-exception (.. "Node " (tostring node) " is not in the graph")
                          {:reason :invalid-node : node :graph (tostring self)})))
  node)

(fn graph-meta.immediate-dependents [self node]
  (. self._dependents (graph-verify-node self node)))

(fn graph-meta.transitive-dependencies-set [self nodes]
  (each [node _ (pairs nodes)] (graph-verify-node self node))
  (graph-transitive nodes (fn [node] (. self._dependencies node))))

(fn graph-meta.transitive-dependents-set [self nodes]
  (each [node _ (pairs nodes)] (graph-verify-node self node))
  (graph-transitive nodes (fn [node] (. self._dependents node))))

(fn graph-meta.topological-sort [{:_dependencies dependencies
                                  :_dependents dependents}
                                 ?comparator]
  (let [sorted []
        pending {}
        queue []
        comparator (or ?comparator (fn [left right] (< left right)))]
    ; Initialize pending count for each node and enqueue nodes with no deps.
    (each [node node-dependencies (pairs dependencies)]
      (let [count (length (keys-of node-dependencies))]
        (tset pending node count)
        (when (= 0 count) (table.insert queue node))))
    (while (< 0 (length queue))
      (table.sort queue comparator)
      (let [node (table.remove queue 1)]
        (table.insert sorted node)
        (each [dependent _ (pairs (. dependents node))]
          (let [count (- (. pending dependent) 1)]
            (tset pending dependent count)
            (when (= 0 count)
              (table.insert queue dependent))))))
    sorted))

;;; Derived-key lookup

(fn find-derived [map key]
  "Return an iterator over keys derived from key and their values."
  (let [normalized (normalize-key key)]
    (let [iterator (fn [_ current]
                     (fn seek [candidate]
                       (if (nil? candidate)
                           (values nil nil)
                           (if (hierarchy-meta.isa? global-hierarchy
                                                    (normalize-key candidate)
                                                    normalized)
                               (values candidate (. map candidate))
                               (seek (next map candidate)))))

                     (seek (next map current)))]
      (values iterator map nil))))

(fn find-derived-1 [map key]
  "Return the unique matching key/value pair, or raise on ambiguity."
  (let [matches (icollect [candidate value (find-derived map key)]
                  [candidate value])]
    (case (length matches)
      1 (let [[matching-key value] (. matches 1)]
          (values matching-key value))
      (where count (< 1 count))
      (let [matching-keys (icollect [_ entry (ipairs matches)]
                            (. entry 1))]
        (error (new-exception (.. "Ambiguous key: " (tostring key)
                                  ". Found multiple candidates: "
                                  (table.concat (icollect [_ item (ipairs matching-keys)]
                                                  (tostring item))
                                                ", "))
                              {:reason :ambiguous-key
                               :config map
                               : key
                               : matching-keys})))
      _ nil)))

;;; Key-dispatched methods

(local method-meta {})
(set method-meta.__index method-meta)

(fn method-new [?name ?default ?normalize ?hierarchy-instance]
  "Create a method with hierarchy-based dispatch and optional key normalization."
  (let [hierarchy-instance (or ?hierarchy-instance global-hierarchy)]
    (setmetatable {:name ?name
                   :hierarchy hierarchy-instance
                   :_methods {}
                   :_preferences {}
                   :_cache {}
                   :_cache-version -1
                   :_default (or ?default (fn [value] value))
                   :_normalize (or ?normalize normalize-key)}
                  method-meta)))

(fn method-meta.add-method [{:_normalize normalize :_methods methods &as self}
                            dispatch-value
                            func]
  (let [dispatch-value (if normalize
                           (normalize dispatch-value)
                           dispatch-value)]
    (when (not= :function (type func))
      (error (new-exception "Method implementation must be a function"
                            {:reason :add-method-expected-func
                             :key dispatch-value
                             :function func})))
    (tset methods dispatch-value func)
    (set self._cache {})))

(fn method-meta.remove-method [{:_normalize normalize
                                :_methods methods
                                &as self}
                               dispatch-value]
  (let [dispatch-value (if normalize
                           (normalize dispatch-value)
                           dispatch-value)]
    (tset methods dispatch-value nil)
    (set self._cache {})))

(fn method-meta.clear-methods [self]
  (set self._methods {})
  (set self._cache {}))

(fn method-meta.prefer [{:_preferences all-preferences &as self} winner loser]
  (let [preferences (or (. all-preferences winner) {})]
    (tset preferences loser true)
    (tset all-preferences winner preferences)
    (set self._cache {})))

(fn method-check-cache [{: hierarchy :_cache-version cache-version &as self}]
  (when (not= hierarchy.version cache-version)
    (set self._cache {})
    (set self._cache-version hierarchy.version)))

(fn method-is-defeated? [{:_preferences preferences} candidate candidates]
  (accumulate [defeated? false _ other (ipairs candidates) &until defeated?]
    (let [method-preferences (. preferences other)]
      (or defeated? (and (not= candidate other) method-preferences
                         (. method-preferences candidate))))))

(fn method-is-dominated? [{: hierarchy} method-name methods]
  (accumulate [dominated? false _ other (ipairs methods) &until dominated?]
    (or dominated?
        (and (not= method-name other)
             (hierarchy-meta.isa? hierarchy other method-name)))))

(fn method-applicable [{: hierarchy :_methods methods} dispatch-value]
  (let [ancestors (hierarchy-meta.get-ancestors hierarchy dispatch-value)]
    (icollect [name _ (pairs methods)]
      (if (or (= name dispatch-value) (. ancestors name)
              (and (split-composite-key name)
                   (hierarchy-meta.isa? hierarchy dispatch-value name)))
          name
          nil))))

(fn method-resolve [{:_default default :_methods methods &as self}
                    dispatch-value]
  (let [applicable (method-applicable self dispatch-value)]
    (case (length applicable)
      0 default
      1 (let [[method-name] applicable]
          (. methods method-name))
      _ (let [candidates (icollect [_ method-name (ipairs applicable)]
                           (if (method-is-dominated? self method-name
                                                     applicable)
                               nil
                               method-name))
              candidates (if (< 1 (length candidates))
                             (icollect [_ candidate (ipairs candidates)]
                               (if (method-is-defeated? self candidate
                                                        candidates)
                                   nil
                                   candidate))
                             candidates)]
          (case (length candidates)
            1 (let [[method-name] candidates]
                (. methods method-name))
            _ (error (new-exception (.. "Ambiguous match for "
                                        (tostring dispatch-value)
                                        ". Candidates: "
                                        (table.concat (icollect [_ candidate (ipairs candidates)]
                                                        (tostring candidate))
                                                      ", ")
                                        ". Use (method:prefer winner loser) to resolve.")
                                    {:reason :ambiguous-match
                                     :key dispatch-value
                                     : candidates})))))))

(fn method-meta.dispatch [{:_normalize normalize &as self} dispatch-value ...]
  (let [dispatch-value (if normalize
                           (normalize dispatch-value)
                           dispatch-value)]
    (method-check-cache self)
    (let [func (or (. self._cache dispatch-value)
                   (let [resolved (method-resolve self dispatch-value)]
                     (tset self._cache dispatch-value resolved)
                     resolved))]
      (func dispatch-value ...))))

(fn method-meta.has-method? [{:_normalize normalize &as self} dispatch-value]
  (let [dispatch-value (if normalize
                           (normalize dispatch-value)
                           dispatch-value)]
    (< 0 (length (method-applicable self dispatch-value)))))

(set method-meta.__call
     (fn [self dispatch-value ...]
       (method-meta.dispatch self dispatch-value ...)))

;;; References, profiles, and runtime variables

(local ref-meta {:reflike? true})
(set ref-meta.__index ref-meta)

(fn ref-meta.ref-key [self]
  self.key)

(fn ref-meta.ref-resolve [{: key : type} config resolve-func]
  (if (= :ref type)
      (let [[key value] [(find-derived-1 config key)]]
        (if key (resolve-func key value) nil))
      (icollect [key value (find-derived config key)]
        (resolve-func key value))))

(fn make-ref [key type-name]
  (setmetatable {:key (normalize-key key) :type type-name} ref-meta))

(fn new-ref [key]
  (make-ref key :ref))

(fn new-refset [key]
  (make-ref key :refset))

(fn reflike? [value]
  (and (= :table (type value)) value.reflike?))

(fn ref? [value]
  (and (reflike? value) (= :ref value.type)))

(fn refset? [value]
  (and (reflike? value) (= :refset value.type)))

(local profile-meta {:profile? true})
(set profile-meta.__index profile-meta)

(fn new-profile [map]
  (setmetatable {:_map map} profile-meta))

(local NIL {})

(fn profile? [value]
  (and (= :table (type value)) value.profile?))

(local var-meta {:var? true})
(set var-meta.__index var-meta)

(fn new-var [name]
  (setmetatable {: name} var-meta))

(fn var? [value]
  (and (= :table (type value)) value.var?))

(fn bind [object bindings]
  "Replace each bound Var in a nested configuration value."
  (postwalk (fn [value]
              (if (and (var? value) (not (nil? (. bindings value.name))))
                  (. bindings value.name)
                  value)) object))

(fn find-derived-refs [config value include-refsets?]
  (let [predicate (if include-refsets? reflike? ref?)]
    (let [found []]
      (each [_ item (ipairs (collect-values value predicate))]
        (each [key _ (find-derived config item.key)]
          (table.insert found key)))
      found)))

(fn resolve-refs [config resolve-func value]
  (postwalk (fn [item]
              (if (reflike? item)
                  (ref-meta.ref-resolve item config resolve-func)
                  item)) value))

(fn dependency-graph [config include-refsets?]
  (let [include-refsets? (if (nil? include-refsets?) true include-refsets?)]
    (graph-from-dependencies (fn [add]
                               (each [key value (pairs config)]
                                 (let [normalized (normalize-key key)]
                                   (add normalized nil)
                                   (each [_ dependency (ipairs (find-derived-refs config
                                                                                  value
                                                                                  include-refsets?))]
                                     (add normalized (normalize-key dependency)))))))))

(fn key-comparator [dependency-graph ?input-keys]
  (let [fallback (fn [left right] (< (tostring left) (tostring right)))
        tie-breaker (array-comparator (or ?input-keys []) fallback)
        topology (graph-meta.topological-sort dependency-graph tie-breaker)]
    (array-comparator topology fallback)))

(fn sort-by-topo-order [keys dependency-graph ?input-keys]
  (table.sort keys (key-comparator dependency-graph ?input-keys))
  keys)

(fn find-keys [config keys traverse]
  (let [passive-graph (dependency-graph config false)
        keyset {}
        input-keys []]
    (each [_ key (ipairs keys)]
      (each [derived-key _ (find-derived config key)]
        (when (not (. keyset derived-key))
          (tset keyset derived-key true)
          (table.insert input-keys derived-key))))
    (let [normalized-keyset (collect [key _ (pairs keyset)] (normalize-key key)
                              true)]
      (each [node _ (pairs (traverse passive-graph normalized-keyset))]
        (when (not (. normalized-keyset node))
          (tset keyset node true))))
    (let [active-graph (dependency-graph config true)
          result (keys-of keyset)]
      (values (sort-by-topo-order result active-graph input-keys) active-graph))))

(fn init-ordered-dependency-keys [config keys]
  (find-keys config keys graph-meta.transitive-dependencies-set))

(fn init-ordered-dependent-keys [config keys]
  (find-keys config keys graph-meta.transitive-dependents-set))

;;; Component module registration

(local registry-log (log.new :stitch.registry))

(fn normalize-k [key]
  (if (= :default key) key (normalize-key key)))

(fn traverse-module [module parts start-index]
  (faccumulate [current module index start-index (length parts)]
    (if (= :table (type current))
        (. current (. parts index))
        nil)))

(fn find-var [path]
  (let [loaded (. package.loaded path)]
    (if (= :function (type loaded))
        loaded
        (let [parts (split-dotted path)]
          (faccumulate [result nil index (- (length parts) 1) 1 -1 &until result]
            (let [module (. package.loaded (table.concat parts "." 1 index))]
              (if (= :table (type module))
                  (let [value (traverse-module module parts (+ index 1))]
                    (if (= :function (type value)) value result))
                  result)))))))

(local resolve-key (method-new :resolve-key (fn [_ instance] instance)
                               normalize-k))

(local expand-key (method-new :expand-key nil normalize-k))
(local assert-key (method-new :assert-key (fn [_ _] nil) normalize-k))
(local init-key (method-new :init-key
                            (fn [key value]
                              (let [func (find-var key)]
                                (if (= :function (type func))
                                    (func value)
                                    (error (new-exception (.. "Unable to find an init-key method or function for "
                                                              (tostring key))
                                                          {:reason :missing-init-key
                                                           : key
                                                           : value})))))
                            normalize-k))

(method-meta.add-method init-key :stitch-expanded (fn [_ value] value))

(local halt-key (method-new :halt-key (fn [_ _] nil) normalize-k))
(local halt-key-with-error (method-new :halt-key-with-error
                                       (fn [key value _exception]
                                         (registry-log:warn (.. "Key "
                                                                (tostring key)
                                                                " halt with error"))
                                         (halt-key key value))
                                       normalize-k))

(local resume-key (method-new :resume-key
                              (fn [key value _instance-key _instance]
                                (init-key key value))
                              normalize-k))

(local suspend-key
       (method-new :suspend-key (fn [key value] (halt-key key value))
                   normalize-k))

(local registry {: resolve-key
                 : expand-key
                 : assert-key
                 : init-key
                 : halt-key
                 : halt-key-with-error
                 : resume-key
                 : suspend-key
                 :derive hierarchy.derive
                 :isa? hierarchy.isa?
                 :get-ancestors hierarchy.get-ancestors
                 : normalize-k
                 : traverse-module
                 : find-var})

(fn registry.method [?name ?default ?normalize]
  "Create and optionally register a lifecycle method."
  (let [new-method (method-new ?name ?default ?normalize)]
    (when ?name (tset registry ?name new-method))
    new-method))

(fn registry.register [key-name module]
  "Register a module's lifecycle functions for key-name."
  (each [name value (pairs module)]
    (when (= :function (type value))
      (let [stitch-method (. registry (.. (tostring name) :-key))]
        (when (and stitch-method stitch-method.add-method)
          (method-meta.add-method stitch-method key-name value)))))
  module)

;;; Configuration and component loading

(local loader-log (log.new :stitch.loader))

(fn check-resource [template path resources]
  (let [filename (string.gsub template "%?" path)
        file (io.open filename :r)]
    (when file
      (file:close)
      (table.insert resources filename))))

(fn find-resources [name]
  (let [path (string.gsub name "%." "/")
        resources []]
    (each [template (string.gmatch package.path "[^;]+")]
      (check-resource template path resources))
    resources))

(fn load-modules [config ?keys]
  "Load and register modules for selected components and dependencies."
  (let [all-keys (or ?keys (keys-of config))
        dependency-keys (init-ordered-dependency-keys config all-keys)
        loaded {}]
    (let [load-module (fn [module-name]
                        (when (nil? (. loaded module-name))
                          (let [[ok? module] [(pcall require module-name)]]
                            (tset loaded module-name ok?)
                            (when (and ok? (= :table (type module)))
                              (loader-log:info (.. "loaded " module-name))
                              (registry.register module-name module)))))]
      (let [keys-to-load {}]
        (each [_ key (ipairs dependency-keys)]
          (let [normalized (normalize-key key)]
            (tset keys-to-load normalized true)
            (each [ancestor _ (pairs (hierarchy.get-ancestors normalized))]
              (tset keys-to-load ancestor true))))
        (each [key _ (pairs keys-to-load)]
          (each [_ module-name (ipairs (dotted-prefixes (tostring key)))]
            (load-module module-name))))
      (let [result (icollect [module-name ok? (pairs loaded) &until false]
                     (if ok? module-name nil))]
        (table.sort result)
        result))))

(fn read-config [path]
  "Load a Lua configuration file and return its value."
  (let [[loader error-message] [(loadfile path)]]
    (if loader (loader) (error error-message))))

(fn process-hierarchy-entry [tag ?parents]
  (if (= :string (type ?parents))
      (hierarchy.derive tag ?parents)
      (when (= :table (type ?parents))
        (each [_ parent (ipairs ?parents)]
          (hierarchy.derive tag parent)))))

(fn process-hierarchy-file [filename]
  (let [[loader error-message] [(loadfile filename)]]
    (if (not loader)
        (loader-log:error (.. "failed to load hierarchy from " filename ": "
                              (tostring error-message)))
        (let [hierarchy-map (loader)]
          (when (= :table (type hierarchy-map))
            (each [tag parents (pairs hierarchy-map)]
              (process-hierarchy-entry tag parents)))))))

(fn load-hierarchy [?path]
  (each [_ filename (ipairs (find-resources (or ?path :stitch.hierarchy)))]
    (process-hierarchy-file filename)))

(fn process-annotation-file [path]
  (let [[loader error-message] [(loadfile path)]]
    (if (not loader)
        (loader-log:error (.. "failed to load annotations from " path ": "
                              (tostring error-message)))
        (let [annotations (loader)]
          (when (= :table (type annotations))
            (each [key metadata (pairs annotations)]
              (tset annotation-registry key metadata)))))))

(fn load-annotations [?path]
  (each [_ filename (ipairs (find-resources (or ?path :stitch.annotations)))]
    (process-annotation-file filename)))

;;; System ordering and construction

(fn system-meta [system]
  (let [meta (getmetatable system)]
    (and meta meta.__stitch)))

(fn system-preconditions [system]
  (precondition (= :table (type system)))
  (let [meta (system-meta system)]
    (precondition (and meta meta.origin meta.order meta.graph))))

(fn system-order [system]
  (let [meta (system-meta system)]
    (and meta meta.order)))

(fn run-exception [system keys index func err]
  (let [key (. keys index)
        value (. system key)
        completed (reversed (icollect [i item (ipairs keys)]
                              (if (< i index) item)))
        remaining (icollect [i item (ipairs keys)]
                    (if (< index i) item))]
    (new-exception (.. "Error on key " (tostring key) " when running system")
                   {:reason :run-threw-exception
                    : system
                    :completed-keys completed
                    :remaining-keys remaining
                    :function func
                    : key
                    : value} err)))

(fn run-loop [system keys func]
  (each [index key (ipairs keys)]
    (case [(protect (func key (. system key)))]
      [false err] (error (run-exception system keys index func err)))))

(fn sort-by-order [order ?keys]
  (if (nil? ?keys)
      order
      (let [sorted (clone ?keys)]
        (table.sort sorted
                    (array-comparator order
                                      (fn [left right]
                                        (< left right))))
        sorted)))

(fn reverse-run [system ?keys func]
  (system-preconditions system)
  (run-loop system (sort-by-order (reversed (system-order system)) ?keys) func))

(fn run [system ?keys func]
  (system-preconditions system)
  (run-loop system (sort-by-order (system-order system) ?keys) func))

(fn each-system [system ?keys]
  (system-preconditions system)
  (let [order (sort-by-order (system-order system) ?keys)]
    ;;; Iterator calls share this cursor.
    (var index 0)
    (fn []
      (set index (+ index 1))
      (let [key (. order index)]
        (if key (values key (. system key)) nil)))))

(fn fold [system keys-or-func ?func-or-initial ?maybe-initial]
  "Fold over system entries in initialization order."
  (system-preconditions system)
  (let [[?keys func ?initial] [(if (= :function (type keys-or-func))
                                   (values nil keys-or-func ?func-or-initial)
                                   (values keys-or-func ?func-or-initial
                                           ?maybe-initial))]
        ordered (sort-by-order (system-order system) ?keys)]
    (accumulate [result ?initial _ key (ipairs ordered)]
      (func result key (. system key)))))

(fn normalize-config [config]
  (collect [key value (pairs config)]
    (normalize-key key)
    (deep-clone value)))

(fn build-ambiguous-key-exception [config key]
  (let [matches (icollect [matching-key _ (find-derived config key)]
                  (tostring matching-key))]
    (new-exception "Ambiguous key"
                   {:reason :ambiguous-key
                    : config
                    : key
                    :matching-keys matches})))

(fn store-system-value [system key value]
  (tset system key value)
  (when (and (= :string (type key)) (string.find key "." 1 true))
    (let [parts (split-dotted key)]
      (let [target (faccumulate [target system index 1 (- (length parts) 1)]
                     (let [part (. parts index)
                           child (or (. target part) {})]
                       (tset target part child)
                       child))]
        (tset target (. parts (length parts)) value)))))

(fn build-key [build-f assert-f resolve-f system key value]
  (let [{: origin :build build-data} (system-meta system)
        bound-resolve (fn [ref-key]
                        (resolve-f ref-key (. system ref-key)))
        resolved (resolve-refs origin bound-resolve value)]
    (assert-f system key resolved)
    (case [(protect (build-f key resolved))]
      [false err] (error (new-exception (.. "Error on key " (tostring key)
                                            " when building system")
                                        {:reason :build-threw-exception
                                         : system
                                         :function build-f
                                         : key
                                         :value resolved}
                                        err))
      [true ?result] (do
                       (store-system-value system key ?result)
                       (tset build-data (tostring key) resolved)
                       system))))

(fn build [config keys build-f assert-f resolve-f]
  "Build a selected dependency subgraph from a configuration."
  (precondition (= :table (type config)))
  (let [normalized-config (normalize-config config)
        normalized-keys (icollect [_ key (ipairs keys)]
                          (normalize-key key))]
    (table.sort normalized-keys
                (fn [left right] (< (tostring left) (tostring right))))
    (let [[ordered-keys dependency-graph] [(init-ordered-dependency-keys normalized-config
                                                                         normalized-keys)]
          selected-config (collect [_ key (ipairs ordered-keys)]
                            key
                            (. normalized-config key))
          missing []]
      (each [_ ref (ipairs (collect-values selected-config ref?))]
        (let [matches (icollect [key _ (find-derived normalized-config ref.key)]
                        key)]
          (case (length matches)
            0 (table.insert missing (tostring ref.key))
            1 nil
            _ (error (build-ambiguous-key-exception normalized-config ref.key)))))
      (when (< 0 (length missing))
        (table.sort missing)
        (error (new-exception "Missing definitions for refs"
                              {:reason :missing-refs
                               :config normalized-config
                               :missing-refs missing})))
      (let [unbound (icollect [_ variable (ipairs (collect-values selected-config
                                                                  var?))]
                      variable.name)]
        (when (< 0 (length unbound))
          (table.sort unbound)
          (error (new-exception "Unbound vars"
                                {:reason :unbound-vars
                                 :config normalized-config
                                 :unbound-vars unbound}))))
      (let [system (setmetatable {}
                                 {:__stitch {:origin normalized-config
                                             :build {}
                                             :order ordered-keys
                                             :graph dependency-graph}})]
        (each [_ key (ipairs ordered-keys)]
          (build-key build-f assert-f resolve-f system key
                     (. normalized-config key)))
        system))))

;;; Profiles and expansion

(fn deprofile-one [{:_map map &as profile-value} keys]
  (let [value (accumulate [value nil _ key (ipairs keys)
                           &until (not (nil? value))]
                (. map key))]
    (if (not (nil? value))
        (if (= value NIL) nil value)
        (error (.. "Missing a valid key for profile " (tostring profile-value)
                   " and keys " (fennel.view keys))))))

(fn deprofile [object ?profile-keys]
  "Select profile values throughout a nested configuration."
  (if (nil? ?profile-keys)
      (let [keys object]
        (fn [value] (deprofile value keys)))
      (postwalk (fn [value]
                  (if (profile? value)
                      (deprofile-one value ?profile-keys)
                      value)) object)))

(fn mergeable-table? [value]
  (and (= :table (type value)) (nil? (getmetatable value))
       (= :string (type (next value)))))

(fn first-seen-source [entry]
  (var source entry)
  (while (= :table (type source))
    (let [key (next source)]
      (set source (. source key))))
  source)

(fn merge-conflict [maps conflicts]
  (let [{: index} (. conflicts 1)
        keys (icollect [_ conflict (ipairs conflicts)] conflict.key)]
    (error (new-exception (.. "Conflicting values at index "
                              (table.concat index ".") " when converging: "
                              (table.concat keys ", ") ".")
                          {:reason :conflicting-expands
                           :config maps
                           :conflicting-index index
                           :expand-keys keys}))))

(fn merge-missing [destination source]
  (each [key value (pairs source)]
    (let [normalized (if (= :table (type key))
                         (normalize-key key)
                         key)
          current (. destination normalized)]
      (if (nil? current)
          (tset destination normalized value)
          (when (and (mergeable-table? value) (mergeable-table? current))
            (merge-missing current value))))))

(fn normalize-tree [tree]
  (if (not (mergeable-table? tree))
      tree
      (collect [key value (pairs tree)]
        (if (= :table (type key)) (normalize-key key) key)
        (normalize-tree value))))

(fn override-leaf? [value]
  (and (not (nil? value)) (not (mergeable-table? value))
       (not (and (= :table (type value)) (empty-table? value)
                 (nil? (getmetatable value))))))

(fn check-merge-conflict [maps path seen-value value source-key]
  ;;; Provenance, not value equality, decides conflicts.
  (when (and seen-value (not= (first-seen-source seen-value) source-key)
             (or (not= :table (type seen-value)) (not (mergeable-table? value))))
    (merge-conflict maps
                    [{:index (clone path) :key (first-seen-source seen-value)}
                     {:index (clone path) :key source-key}])))

(fn merge-key [destination maps key value override path seen source-key]
  (let [override-value (if (= :table (type override))
                           (. override key)
                           nil)]
    (if (override-leaf? override-value)
        (tset destination key override-value)
        ;;; Each recursive call owns its path; siblings need no cleanup.
        (let [path (clone path)
              seen-value (. seen key)]
          (table.insert path key)
          (check-merge-conflict maps path seen-value value source-key)
          ;;; Empty source tables leave an existing value in place.
          (if (not (mergeable-table? value))
              (when (or (not (and (= :table (type value)) (empty-table? value)))
                        (nil? (. destination key)))
                (tset destination key value)
                (tset seen key source-key))
              (do
                (when (not= :table (type (. destination key)))
                  (tset destination key {}))
                (when (not= :table (type (. seen key)))
                  (tset seen key {}))
                (each [child-key child-value (pairs value)]
                  (merge-key (. destination key) maps (normalize-key child-key)
                             child-value override-value path (. seen key)
                             source-key))))))))

(fn converge [maps ?override-map]
  "Merge expansion maps, rejecting conflicting leaves unless overridden."
  (precondition (= :table (type maps)))
  (let [overrides (normalize-tree (or ?override-map {}))
        result {}
        seen {}]
    (each [source-key source (pairs maps)]
      (each [key value (pairs source)]
        (merge-key result maps (normalize-key key) value overrides [] seen
                   source-key)))
    (merge-missing result overrides)
    result))

(fn inject-expanded-deps [result key]
  (if (or (not= :table (type result)) (not (nil? (. result key))))
      result
      (let [copy (clone result)
            dependencies (icollect [sub-key _ (pairs result)]
                           (new-ref (normalize-key sub-key)))
            derived-key (.. (normalize-key key) :|stitch-expanded)]
        (table.sort dependencies (fn [left right] (< left.key right.key)))
        (tset copy derived-key {:deps dependencies})
        (let [meta (getmetatable result)]
          (when (= :table (type meta)) (setmetatable copy meta)))
        copy)))

(fn expand [config ?wrap-f ?config-keys]
  "Expand registered modules and merge their configurations."
  (precondition (= :table (type config)))
  (let [wrap-f (or ?wrap-f (fn [value] value))
        config-keys (or ?config-keys config)
        expanded {}
        terminal {}]
    (each [key value (pairs config)]
      (if (and (not (nil? (. config-keys key)))
               (method-meta.has-method? expand-key key))
          (let [result (inject-expanded-deps (wrap-f (expand-key key value))
                                             key)]
            (tset expanded key result))
          (tset terminal key value)))
    (converge expanded terminal)))

;;; Lifecycle operations

(fn build-failed-spec-exception [system key value err]
  (new-exception (.. "Assertion failed on key " (tostring key)
                     " when building system")
                 {:reason :build-failed-spec : system : key : value} err))

(fn wrapped-assert-key [system key value]
  (case [(protect (assert-key key value))]
    [false err] (error (build-failed-spec-exception system key value err))))

(local lifecycle-log (log.new :stitch.lifecycle))

(fn halt-system-key [system key errors failed-keys opts]
  (let [force? (and opts opts.force)
        metadata (system-meta system)
        failed-dependent? (and (not force?)
                               (accumulate [failed? false dependent _ (pairs (graph-meta.immediate-dependents metadata.graph
                                                                                                              key))
                                            &until failed?]
                                 (or failed? (. failed-keys dependent))))]
    (if failed-dependent?
        false
        (if (nil? (. system key))
            true
            (case [(protect (if (and opts opts.exception)
                                (halt-key-with-error key (. system key)
                                                     opts.exception)
                                (halt-key key (. system key))))]
              [true _] true
              [false err] (do
                            (lifecycle-log:error (.. "Failed to halt key "
                                                     (tostring key) ": "
                                                     (tostring err)))
                            (table.insert errors {: key :error err})
                            false))))))

(fn unset-system-value [system key]
  (tset system key nil)
  (when (and (= :string (type key)) (string.find key "." 1 true))
    (let [parts (split-dotted key)]
      (var source system)
      (var target system)
      ;;; Copy only the dotted index path; component values retain identity.
      (var index 1)
      (while (and (< index (length parts))
                  (= :table (type (. source (. parts index)))))
        (let [part (. parts index)
              child (. source part)
              copy (clone child)
              meta (getmetatable child)]
          (when (= :table (type meta)) (setmetatable copy meta))
          (tset target part copy)
          (set source child)
          (set target copy)
          (set index (+ index 1))))
      (unset-in system parts))))

(fn halt [system ?keys ?opts]
  "Halt selected components in reverse dependency order."
  (precondition (or (nil? ?keys) (= :table (type ?keys))))
  (system-preconditions system)
  (let [metadata (getmetatable system)
        halted (setmetatable (clone system) metadata)
        errors []
        failed-keys {}
        opts (or ?opts {})]
    (reverse-run halted ?keys
                 (fn [key]
                   (if (halt-system-key halted key errors failed-keys opts)
                       (unset-system-value halted key)
                       (tset failed-keys key true))))
    (when (< 0 (length errors))
      (error (new-exception (.. "Halt completed with " (length errors)
                                " errors.")
                            {:reason :halt-threw-exception
                             : errors
                             : failed-keys
                             :system halted})))
    halted))

(fn init [config ?keys ?opts]
  "Initialize all components or the selected dependency subgraph."
  (precondition (= :table (type config))
                (or (nil? ?keys) (= :table (type ?keys))))
  (let [keys (if (and ?keys ?opts ?opts.include-transitive-dependents)
                 (init-ordered-dependent-keys config ?keys)
                 (or ?keys (keys-of config)))]
    (build config keys init-key wrapped-assert-key resolve-key)))

(fn resume [config system ?keys ?opts]
  "Resume existing instances against a new configuration."
  (precondition (= :table (type config))
                (or (nil? ?keys) (= :table (type ?keys))))
  (system-preconditions system)
  (let [?keys (if (and ?keys ?opts ?opts.include-transitive-dependents)
                  (init-ordered-dependent-keys config ?keys)
                  ?keys)
        all-keys (or ?keys (keys-of config))
        dependency-keys (init-ordered-dependency-keys config all-keys)
        dependency-set (collect [_ key (ipairs dependency-keys)] key true)
        metadata (system-meta system)
        targeted-old-set {}]
    (if (nil? ?keys)
        (each [key _ (pairs metadata.origin)]
          (tset targeted-old-set key true))
        (each [_ key (ipairs (init-ordered-dependency-keys metadata.origin
                                                           all-keys))]
          (tset targeted-old-set key true)))
    (let [missing (icollect [key _ (pairs metadata.origin)]
                    (if (and (. targeted-old-set key)
                             (not (nil? (. system key)))
                             (not (. dependency-set key)))
                        key
                        nil))
          missing-set (collect [_ key (ipairs missing)] key true)]
      (reverse-run system missing (fn [key value] (halt-key key value)))
      (let [build-f (fn [key value]
                      (if (nil? (. system key))
                          (init-key key value)
                          (or (resume-key key value
                                          (. metadata.build (tostring key))
                                          (. system key))
                              (. system key))))
            resumed (build config all-keys build-f wrapped-assert-key
                           resolve-key)
            resumed-meta (system-meta resumed)
            merged-order []
            seen {}]
        (each [_ key (ipairs (system-order system))]
          (when (and (not (nil? (. system key))) (not (. missing-set key))
                     (not (. dependency-set key)))
            (store-system-value resumed key (. system key))
            (tset resumed-meta.build (tostring key)
                  (. metadata.build (tostring key)))
            (tset seen key true)
            (table.insert merged-order key)))
        (each [_ key (ipairs resumed-meta.order)]
          (when (not (. seen key))
            (tset seen key true)
            (table.insert merged-order key)))
        (set resumed-meta.order
             (sort-by-topo-order merged-order resumed-meta.graph merged-order))
        resumed))))

(fn suspend [system ?keys]
  "Suspend selected components in reverse dependency order."
  (precondition (or (nil? ?keys) (= :table (type ?keys))))
  (system-preconditions system)
  (reverse-run system ?keys
               (fn [key value]
                 (when (not (nil? value))
                   (suspend-key key value)))))

;;; Reloadable workflow facade

(local reloaded {:system nil
                 :config nil
                 :callbacks {:before-reset []
                             :after-reset []
                             :after-system-change []
                             :reset-failed []}})

(fn normalize-workflow-keys [?keys]
  (if (= :string (type ?keys)) [?keys] ?keys))

(fn reloaded.set-config [config]
  "Set the configuration used by the reloadable workflow."
  (set reloaded.config config))

(fn ensure-workflow-config [?keys]
  (when (nil? reloaded.config)
    (error (new-exception "Configuration not set. Call REPL.set-config(config) first."
                          {:reason :config-not-set :keys ?keys}))))

(fn workflow-callbacks [event]
  (let [callbacks (. reloaded.callbacks event)]
    (if callbacks
        callbacks
        (error (new-exception (.. "Unknown reloaded-workflow callback event: "
                                  (tostring event))
                              {:reason :unknown-callback-event : event})))))

(local workflow-log (log.new :stitch.reloaded-workflow))

(fn workflow-emit [event payload]
  (each [_ callback (ipairs (workflow-callbacks event))]
    (case [(protect (callback payload))]
      [false err]
      (workflow-log:error (.. "Callback failure for " (tostring event) ": "
                              (tostring err))))))

(fn emit-system-change [operation
                        ?keys
                        ?dependencies
                        ?context
                        ?previous
                        system]
  (workflow-emit :after-system-change
                 {:event :after-system-change
                  :op operation
                  :keys ?keys
                  :deps ?dependencies
                  :context ?context
                  :previous-system ?previous
                  : system
                  :config reloaded.config}))

(fn all-keys-initialized? [keys]
  (if (not reloaded.system)
      false
      (accumulate [initialized? true _ key (ipairs keys)
                   &until (not initialized?)]
        (if (. reloaded.system key) initialized? false))))

(fn workflow-go [?keys]
  (let [system (if reloaded.system
                   (resume reloaded.config reloaded.system ?keys nil)
                   (init reloaded.config ?keys nil))]
    (set reloaded.system system)
    system))

(fn reloaded.go [?keys]
  "Initialize configured components, preserving an existing system."
  (let [?keys (normalize-workflow-keys ?keys)]
    (ensure-workflow-config ?keys)
    (let [selected (if ?keys
                       (init-ordered-dependent-keys reloaded.config ?keys)
                       (keys-of reloaded.config))]
      (if (all-keys-initialized? selected)
          reloaded.system
          (workflow-go selected)))))

(fn workflow-halt [?keys]
  (if (not reloaded.system)
      nil
      (case [(protect (halt reloaded.system ?keys {:force true}))]
        [true result] (let [system (if (empty-table? result) nil result)]
                        (set reloaded.system system)
                        system)
        [false err] (if (= :halt-threw-exception err.data.reason)
                        (let [system err.data.system]
                          (set reloaded.system system)
                          system)
                        (err:reraise)))))

(fn reloaded.halt [?keys ?opts]
  "Halt configured components and notify observers."
  (let [?keys (normalize-workflow-keys ?keys)
        opts (or ?opts {})]
    (if (not reloaded.system)
        nil
        (let [dependencies (if ?keys
                               (init-ordered-dependent-keys reloaded.config
                                                            ?keys)
                               nil)
              previous reloaded.system
              result (workflow-halt dependencies)]
          (emit-system-change :halt ?keys dependencies opts.context previous
                              reloaded.system)
          result))))

(fn reloaded.reset [?keys ?opts]
  "Halt and rebuild the requested components."
  (let [?keys (normalize-workflow-keys ?keys)]
    (ensure-workflow-config ?keys)
    (let [opts (or ?opts {})
          dependencies (if ?keys
                           (init-ordered-dependent-keys reloaded.config ?keys)
                           nil)
          ?context opts.context
          previous reloaded.system]
      (workflow-emit :before-reset
                     {:event :before-reset
                      :keys ?keys
                      :deps dependencies
                      :context ?context
                      :system previous
                      :config reloaded.config})
      (case [(protect (workflow-halt dependencies)
                      (when (nil? dependencies)
                        (set reloaded.system nil))
                      (workflow-go dependencies))]
        [false err] (do
                      (workflow-emit :reset-failed
                                     {:event :reset-failed
                                      :keys ?keys
                                      :deps dependencies
                                      :context ?context
                                      :previous-system previous
                                      :system reloaded.system
                                      :config reloaded.config
                                      :error err})
                      (error err))
        [true result] (do
                        (workflow-emit :after-reset
                                       {:event :after-reset
                                        :keys ?keys
                                        :deps dependencies
                                        :context ?context
                                        :previous-system previous
                                        :system result
                                        :config reloaded.config})
                        (emit-system-change :reset ?keys dependencies ?context
                                            previous result)
                        result)))))

(fn reloaded.on [event callback]
  "Subscribe to a workflow lifecycle event."
  (precondition (= :function (type callback)))
  (table.insert (workflow-callbacks event) callback)
  callback)

(fn reloaded.off [event callback]
  "Remove a callback from a workflow lifecycle event."
  (let [callbacks (workflow-callbacks event)]
    (for [index (length callbacks) 1 -1]
      (when (= (. callbacks index) callback)
        (table.remove callbacks index)))))

(fn reloaded.suspend [?keys ?opts]
  "Suspend selected running components and notify observers."
  (let [?keys (normalize-workflow-keys ?keys)
        opts (or ?opts {})]
    (when reloaded.system
      (let [dependencies (if ?keys
                             (init-ordered-dependent-keys reloaded.config ?keys)
                             nil)
            previous reloaded.system]
        (suspend reloaded.system dependencies)
        (emit-system-change :suspend ?keys dependencies opts.context previous
                            reloaded.system)))))

(fn reloaded.resume [?keys ?opts]
  "Resume selected components and notify observers."
  (let [?keys (normalize-workflow-keys ?keys)]
    (ensure-workflow-config ?keys)
    (let [opts (or ?opts {})
          dependencies (if ?keys
                           (init-ordered-dependent-keys reloaded.config ?keys)
                           (keys-of reloaded.config))
          previous reloaded.system
          system (workflow-go dependencies)]
      (emit-system-change :resume ?keys dependencies opts.context previous
                          system)
      system)))

(fn reloaded.reload [?load-config-f ?keys ?opts]
  "Suspend, load a new configuration, then resume selected components."
  (let [?keys (normalize-workflow-keys ?keys)
        opts (or ?opts {})]
    (when (not= :function (type ?load-config-f))
      (error (new-exception "reload requires a configuration loader function"
                            {:reason :missing-config-loader})))
    (let [previous reloaded.system
          suspend-deps (if ?keys
                           (init-ordered-dependent-keys reloaded.config ?keys)
                           nil)]
      (when reloaded.system
        (suspend reloaded.system suspend-deps))
      (reloaded.set-config (?load-config-f))
      (let [dependencies (if ?keys
                             (init-ordered-dependent-keys reloaded.config ?keys)
                             (keys-of reloaded.config))
            system (workflow-go dependencies)]
        (emit-system-change :reload ?keys dependencies opts.context previous
                            system)
        system))))

;;; Public API

{: annotate
 : describe
 : valid-config-key?
 : normalize-key
 : find-derived
 : find-derived-1
 :ref new-ref
 :refset new-refset
 : ref?
 : refset?
 : reflike?
 :profile new-profile
 : profile?
 :var new-var
 : var?
 : bind
 : dependency-graph
 : key-comparator
 : init-ordered-dependency-keys
 : init-ordered-dependent-keys
 : read-config
 : load-modules
 : load-hierarchy
 : load-annotations
 : run
 : reverse-run
 :each each-system
 : fold
 : build
 : resolve-key
 : expand-key
 : init-key
 : halt-key
 : resume-key
 : suspend-key
 : assert-key
 : NIL
 : deprofile
 : converge
 : expand
 : init
 : halt
 : resume
 : suspend
 :method registry.method
 :register registry.register
 :derive hierarchy.derive
 :isa? hierarchy.isa?
 : log
 :exception new-exception
 : registry
 : composite-key?
 :reloaded-workflow reloaded}
