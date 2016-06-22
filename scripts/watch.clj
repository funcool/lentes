(require '[cljs.build.api :as b])

(b/watch
 (b/inputs "test" "src")
 {:main 'lentes.tests
  :parallel-build false
  :output-to "out/tests.js"
  :output-dir "out/tests"
  :target :nodejs
  :optimizations :none
  :pretty-print true
  :language-in  :ecmascript6
  :language-out :ecmascript5
  :verbose true})
