(defproject funcool/lentes "1.0.0"
  :description "Functional references for Clojure and ClojureScript"
  :url "https://github.com/funcool/lentes"
  :license {:name "Public Domain" :url "http://unlicense.org/"}

  :dependencies [[org.clojure/clojure "1.8.0" :scope "provided"]
                 [org.clojure/clojurescript "1.8.34" :scope "provided"]
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

    :plugins [[funcool/codeina "0.3.0"]
              [lein-ancient "0.6.7"]]}})
