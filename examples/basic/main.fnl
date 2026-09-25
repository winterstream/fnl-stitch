;;; Register components once, then wire them with data.

(local st (require :stitch))

;; Each component provides its behavior through lifecycle functions.
(st.register :example.store
             {:init (fn [_ config]
                      {:name config.name})
              :halt (fn [_ store]
                      (print (.. "Closed store for " store.name)))})

(st.register :example.greeter
             {:init (fn [_ config]
                      (let [store config.store]
                        {:greet (fn [] (.. "Hello, " store.name "!"))}))})

;; The ref tells Stitch to initialize the store before the greeter.
(local config {:example.store {:name "Fennel programmers"}
               :example.greeter {:store (st.ref :example.store)}})

(local system (st.init config))
(local greeter (. system :example.greeter))

(print (greeter.greet))
(st.halt system)
