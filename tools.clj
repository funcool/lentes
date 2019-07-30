(require '[clojure.java.shell :as shell])
(require '[figwheel.main.api :as figwheel])
(require '[cljs.build.api :as api])

(require '[rebel-readline.core]
         '[rebel-readline.clojure.main]
         '[rebel-readline.clojure.line-reader]
         '[rebel-readline.clojure.service.local])

(defmulti task first)

(defmethod task :default
  [args]
  (let [all-tasks  (-> task methods (dissoc :default) keys sort)
        interposed (->> all-tasks (interpose ", ") (apply str))]
    (println "Unknown or missing task. Choose one of:" interposed)
    (System/exit 1)))

(defmethod task "repl"
  [args]
  (rebel-readline.core/with-line-reader
    (rebel-readline.clojure.line-reader/create
     (rebel-readline.clojure.service.local/create))
    (clojure.main/repl
     :prompt (fn []) ;; prompt is handled by line-reader
     :read (rebel-readline.clojure.main/create-repl-read))))

(def build-options
  {:main 'lentes.tests
   :output-to "target/tests.js"
   :source-map "target/tests.js.map"
   :output-dir "target/tests"
   :target :nodejs
   :pretty-print false
   :pseudo-names false
   :verbose true})

(defn build
  [optimizations]
  (api/build (api/inputs "src" "test")
             (cond->  (assoc build-options :optimizations optimizations)
               (= optimizations :none) (assoc :source-map true))))

(defmethod task "build"
  [[_ type]]
  (case type
    (nil "none") (build :none)
    "simple"     (build :simple)
    "advanced"   (build :advanced)
    (do (println "Unknown argument to test task:" type)
        (System/exit 1))))

(require '[badigeon.jar])
(require '[badigeon.deploy])

(defmethod task "jar"
  [args]
  (badigeon.jar/jar 'funcool/lentes
                    {:mvn/version "1.3.0-SNAPSHOT"}
                    {:out-path "target/lentes.jar"
                     :mvn/repos '{"clojars" {:url "https://repo.clojars.org/"}}
                     :allow-all-dependencies? false}))

(defmethod task "deploy"
  [args]
  (let [;; Artifacts are maps with a required :file-path key and an optional :extension key
        artifacts [{:file-path "target/lentes.jar" :extension "jar"}
                   {:file-path "pom.xml" :extension "pom"}]]
    (badigeon.deploy/deploy
     'funcool/lentes "1.3.0-SNAPSHOT"
     artifacts
     {:id "clojars" :url "https://repo.clojars.org/"}
     {:allow-unsigned? true})))

;;; Build script entrypoint. This should be the last expression.

(task *command-line-args*)
