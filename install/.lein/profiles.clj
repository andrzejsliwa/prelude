{:user
 {:plugins      [[lein-kibit "0.1.3"]
                 [jonase/eastwood "0.2.3"]
                 [lein-ancient "0.6.10" :exclusions [org.clojure/clojure]]]
  :dependencies
                [[im.chit/lucid.mind "1.3.7"]
                 [im.chit/lucid.space "1.2.0"]
                 [im.chit/lucid.core.debug "1.3.7"]
                 [im.chit/lucid.core.inject "1.3.7"]
                 [im.chit/lucid.core.namespace "1.3.7"]
                 [com.gfredericks/debug-repl "0.0.8"]

                 [spyscope "0.1.6"]
                 [thalia "0.1.0"]
                 [aprint "0.1.3"]
                 [slamhound "1.5.5"]]

  :aliases      {"slamhound" ["run" "-m" "slam.hound"]}

  :repl-options {:init             (do
                                     (use '[aprint.core])
                                     (intern 'clojure.core 'aprint aprint.core/aprint)
                                     (set! *print-length* 100))
                 :nrepl-middleware [com.gfredericks.debug-repl/wrap-debug-repl]}

  :injections
                [(require '[lucid.core.inject :as inject])
                 (require 'spyscope.core)
                 (use '[aprint.core])
                 (use '[thalia.doc])
                 (inject/in [lucid.core.inject :refer [inject [in inject-in]]]
                            ;; pretty print
                            [clojure.pprint pprint]
                            ;; shell
                            [clojure.java.shell sh]
                            ;; doc & source
                            [clojure.repl doc source]
                            ;; pulling libraries directly from repl
                            [lucid.space pull]
                            ;; nice helpers for namespaces
                            [lucid.core.namespace run clear-aliases clear-mappings]

                            clojure.core
                            ;; reflection helpers
                            [lucid.mind .& .> .? .* .% .%> .>var .>ns]

                            clojure.core >
                            ;; aprint for pretty printing things
                            [aprint.core aprint ap]
                            ;; debugging repl
                            [com.gfredericks.debug-repl break! unbreak! catch-break!]

                            clojure.core
                            ;; debugging helpers
                            [lucid.core.debug :refer [[dbg-> *->] [dbg->> *->>]]])
                 ;; Enable thalia extended docs
                 (thalia.doc/add-extra-docs! :language "en_US")]}}
