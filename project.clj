(defproject funcool/lentes "1.2.0"
  :description "Functional references for Clojure and ClojureScript"
  :url "https://github.com/funcool/lentes"
  :license {:name "BSD (2-Clause)"
            :url "http://opensource.org/licenses/BSD-2-Clause"}
  :dependencies [[org.clojure/clojure "1.8.0" :scope "provided"]
                 [org.clojure/clojurescript "1.9.89" :scope "provided"]
                 [org.clojure/test.check "0.9.0" :scope "test"]]

  :deploy-repositories {"releases" :clojars
                        "snapshots" :clojars}

  :source-paths ["src"]
  :test-paths ["test"]
  :jar-exclusions [#"\.swp|\.swo|user.clj"]

  :profiles
  {:dev
   {:codeina {:sources ["src"]
              :reader :clojurescript
              :target "doc/dist/latest/api"
              :src-uri "http://github.com/funcool/lentes/blob/master/"
              :src-uri-prefix "#L"}

    :plugins [[funcool/codeina "0.5.0"]
              [lein-ancient "0.6.10"]]}})
