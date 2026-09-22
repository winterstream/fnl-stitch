# Stitch for Fennel

Stitch builds applications from components and manages their lifecycles. This
repository contains its Fennel implementation. It uses ideas from the Lua
[Stitch](https://github.com/winterstream/lua-stitch) project and Clojure's
[Integrant](https://github.com/weavejester/integrant).

Describe the components and dependencies in a configuration table instead of
building the system in code. Stitch resolves dependencies, starts components in
order, and stops them in reverse order.

## Getting started

From the repository root, run the examples and tests:

```sh
./examples/basic/run.sh
./examples/profiles/run.sh
./test.sh
```

The module is `stitch.fnl`. Add it to Fennel's module path to use it in another
project:

```fennel
(local st (require :stitch))
```

## Usage

Register lifecycle functions for component keys and describe the system in a
configuration table. Use `st.ref` to declare a dependency. When starting the
dependent, Stitch replaces the reference with the initialized component.

```fennel
(local st (require :stitch))

(st.register :app.store
             {:init (fn [_ config] {:name config.name})
              :halt (fn [_ store]
                      (print (.. "Closed store for " store.name)))})

(st.register :app.greeter
             {:init (fn [_ config]
                      (let [store config.store]
                        {:greet (fn [] (.. "Hello, " store.name "!"))}))})

(local config {:app.store {:name "Fennel programmers"}
               :app.greeter {:store (st.ref :app.store)}})

(local system (st.init config))
(local greeter (. system :app.greeter))
(print (greeter.greet))
(st.halt system)
```

`st.init` starts every configured component. Pass a list of keys to initialize
specific components and their dependencies. `st.halt` stops components in
reverse dependency order.

## Concepts

### References and partial systems

A **Ref** (`st.ref`) creates a hard dependency: selecting its consumer also
selects the referenced component. A **Refset** (`st.refset`) collects the
matching components that are already selected without selecting or
initializing them.

```fennel
(local st (require :stitch))

(st.register :logger {:init (fn [_ config] config)})
(st.register :app.server {:init (fn [_ config] config)})

(local config {"logger|console" {}
               "logger|file" {}
               :app.server {:loggers (st.refset :logger)}})

(local system (st.init config [:app.server "logger|console"]))
(local server (. system :app.server))
;; server.loggers contains the selected console logger.
(st.halt system)
```

Use `st.init-ordered-dependent-keys` to list a component and its hard-reference
dependents in initialization order. `st.reloaded-workflow` uses this order to
suspend, resume, reload, or reset selected components.

### Compound keys

Compound keys represent multiple instances of a component type. Each key derives
from its base key, so a refset can match all instances:

```fennel
(local config {"adapter|app1" {:port 8080}
               "adapter|app2" {:port 8081}})
```

### Profiles and runtime values

Profiles select configuration for an environment. Vars mark values to supply at
runtime via `st.bind`.

```fennel
(local st (require :stitch))

(st.register :app.store {:init (fn [_ config] config)})

(local config {:app.store
               (st.profile {:dev {:driver :memory}
                            :prod {:driver :file
                                   :path (st.var :database-path)}})})

(local selected (st.deprofile config [:prod]))
(local bound (st.bind selected {:database-path "app.db"}))
(local system (st.init bound))
(st.halt system)
```

`st.deprofile` checks profile names in order and uses the first match.

### Metadata and validation

Attach metadata to keys with `st.annotate` and get it with `st.describe`.
Component registrations can also define `assert` and `resolve` hooks. The
`assert` hook validates resolved configuration; `resolve` can adapt the value a
component receives through a reference.

```fennel
(local st (require :stitch))

(st.annotate :app.server {:doc "Public HTTP entrypoint"
                          :tags [:networking :public]})

(local metadata (st.describe :app.server))
```

### Modules and expansion

`st.load-modules` loads component modules named by dotted keys with `require`,
then registers their lifecycle functions. `st.expand` combines configuration
fragments from registered modules' `expand` functions before initialization.

```fennel
(local st (require :stitch))

(local config (st.read-config "config.lua"))
(st.load-modules config)
(local expanded (st.expand config))
(local system (st.init expanded))
```

### Suspending and resuming

Use `st.suspend` and `st.resume` to reload part of an application without
restarting every resource. By default, `suspend` works like `halt`, and `resume`
works like `init`. Register custom `suspend` and `resume` functions when a
resource needs different behavior.

```fennel
(st.suspend system)
;; Reload application code or configuration here.
(local resumed (st.resume config system))
```

### Custom lifecycle methods

Define extra methods with `st.method` and register their implementations for
components. Use `st.run` to call a method for components in a system.

```fennel
(local st (require :stitch))

(local check-key (st.method :check-key))
(st.register :app.store
             {:init (fn [_ config] config)
              :check (fn [_ store] (assert store.healthy))})
(local system (st.init {:app.store {:healthy true}}))
(st.run system nil (fn [key value] (check-key key value)))
(st.halt system)
```

### Logging

Stitch includes a simple logger you can configure:

```fennel
(local st (require :stitch))

(local logger (st.log.new :app.startup))
(logger:info "Starting up")
(st.log.set-level :info)
```

Use `st.log.use` to set a custom log writer.

## Examples

The [`examples`](examples) directory contains Fennel examples you can run:

- [Basic lifecycle](examples/basic/main.fnl): register components, declare a
  reference, and start and stop the system.
- [Profiles and vars](examples/profiles/main.fnl): select configuration by
  profile and bind a runtime value.

For more features and larger applications, see the examples in
[Lua Stitch](https://github.com/winterstream/lua-stitch/tree/main/examples).
Those examples use the Lua version's API and syntax.

## License

MIT. Multi-method code is under EPL-1.0; see [LICENSE](LICENSE).
