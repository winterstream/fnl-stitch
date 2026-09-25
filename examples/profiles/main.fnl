;;; Choose a profile and bind runtime values before initialization.

(local st (require :stitch))

;; Stand-in adapters keep the example runnable without a database.
(st.register :example.store
             {:init (fn [_ config]
                      (case config.driver
                        :memory {:kind :memory}
                        :file {:kind :file :path config.path}
                        _ (error (.. "Unknown store driver: "
                                     (tostring config.driver)))))
              :halt (fn [_ store]
                      (print (.. "Stopped " store.kind " store")))})

(local profile-config
       {:example.store (st.profile {:dev {:driver :memory}
                                    :prod {:driver :file
                                           :path (st.var :database-path)}})})

;; Profiles choose the adapter; vars supply deployment-specific values.
(fn run-profile [profile bindings]
  (let [profiled (st.deprofile profile-config [profile])
        bound (st.bind profiled bindings)
        system (st.init bound)
        store (. system :example.store)]
    (print (.. (tostring profile) ": using " store.kind " store"
               (if store.path (.. " at " store.path) "")))
    (st.halt system)))

(run-profile :dev {})
(run-profile :prod {:database-path :app.db})
