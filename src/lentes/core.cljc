(ns lentes.core
  (:refer-clojure :exclude [nth key keys vals filter select-keys cat derive]))

;; constructors

(defn lens
  "Given a function for getting the focused value from a state (getter)
  and a function that takes the state and and update function (setter),
  constructs a lens."
  ([getter]
   (fn [next]
     (fn
       ([s]
        (next (getter s)))
       ([s f]
        (throw (ex-info "Read only lense!" {}))))))
  ([getter setter]
   (fn [next]
     (fn
       ([s]
        (next (getter s)))
       ([s f]
        (setter s #(next % f)))))))

;; base lenses

(defn id-setter
  "The identity setter, applies the function to the state."
  [s f]
  (f s))

(defn const-setter
  "The constant setter, returns the state unaltered."
  [s _]
  s)

(def id
  "Identity lens."
  (lens identity id-setter))

;; API

(defn focus
  "Given a lens and a state, return the value focused by the lens."
  [lens s]
  (let [getter (lens identity)]
    (getter s)))

(defn over
  "Given a setter, a function and a state, apply the function over
  the value focused by the setter."
  [st f s]
  (let [setter (st id-setter)]
    (setter s f)))

(defn put
  "Given a setter, a new value and a state, replace the value focused by
  the lens with the new one."
  [st v s]
  (over st (constantly v) s))

;; combinators

(defn units
  "Given a function from unit A to unit B and another in the
  opposite direction, construct a lens that focuses and updates
  a converted value."
  [one->other other->one]
  (lens
   one->other
   (fn [s f]
     (other->one (f (one->other s))))))

;; lenses

(defn passes
  "Given a predicate, return a lens that focuses in an element only
  if passes the predicate.

  The lens is not well-behaved, depens on the outcome of the predicate."
  [applies?]
  (lens
   (fn [s]
     (when (applies? s)
       s))
   (fn [s f]
     (if (applies? s)
       (f s)
       s))))

(defn nth
  "Given a number, returns a lens that focuses on the given index of
  a collection."
  [n]
  (lens
    (fn [s]
      (clojure.core/nth s n))
    (fn [s f]
      (update s n f))))

(def fst (nth 0))
(def snd (nth 1))

(defn sequential-empty
  {:internal true
   :no-doc true}
  [coll]
  (cond
    (map? coll) {}
    (set? coll) #{}
    :else []))

(def tail
  "A lens into the tail of a collection."
  (lens
   rest
   (fn [s f]
     (into (sequential-empty s)
           (cons (first s)
                 (f (rest s)))))))

(defn key
  "Given a key, returns a lens that focuses on the given key of
  an associative data structure."
  [k]
  (lens
   (fn [s]
     (get s k))
   (fn [s f]
     (update s k f))))

(def ^:private core-select-keys
  #?(:clj clojure.core/select-keys
     :cljs cljs.core/select-keys))

(defn select-keys
  "Return a lens focused on the given keys in an associative data
  structure."
  [ks]
  (lens
   (fn [s]
     (core-select-keys s ks))
   (fn [s f]
     (merge (apply dissoc s ks)
            (-> (core-select-keys s ks)
                f
                (core-select-keys ks))))))

(defn in
  "Given a path and optionally a default value, return a lens that
  focuses the given path in an associative data structure."
  ([path]
   (in path nil))
  ([path default]
   (lens
    (fn [s]
      (get-in s path default))
    (fn [s f]
      (update-in s path f)))))

;; interop

(defn- prefix-key
  {:internal true :no-doc true}
  [key id]
  (keyword (str id "-" (name key))))

(def ^:private +empty+
  #?(:clj (Object.) :cljs (js/Object.)))

#?(:clj
   (deftype RWFocus [id lens src check-equals?
                     ^:volatile-mutable watchers
                     ^:volatile-mutable srccache
                     ^:volatile-mutable oldcache
                     ^:volatile-mutable cache]
     clojure.lang.IDeref
     (deref [self]
       (locking self
         (if (identical? srccache @src)
           cache
           (let [source (deref src)
                 result (focus lens source)]
             (set! (.-srccache self) source)
             (set! (.-oldcache self) (.-cache self))
             (set! (.-cache self) result)
             result))))

     clojure.lang.IAtom
     (reset [self newval]
       (swap! src #(put lens newval %))
       (deref self))
     (swap [self f]
       (swap! src (fn [s] (over lens f s)))
       (deref self))
     (swap [self f x]
       (swap! src (fn [s] (over lens #(f % x) s)))
       (deref self))
     (swap [self f x y]
       (swap! src (fn [s] (over lens #(f % x y) s)))
       (deref self))
     (swap [self f x y more]
       (swap! src (fn [s] (over lens #(apply f % x y more) s)))
       (deref self))

     clojure.lang.IRef
     (addWatch [self key cb]
       (letfn [(run-watchers [oldv newv]
                 (doseq [[key wf] (.-watchers self)]
                   (wf key self oldv newv)))
               (main-watcher [_ _ oldv newv]
                 (locking self
                   (if (identical? newv (.-srccache self))
                     (if-not check-equals?
                       (run-watchers (.-oldcache self)
                                     (.-cache self)))
                     (let [old' (focus lens oldv)
                           new' (focus lens newv)]
                       (set! (.-cache self) new')
                       (set! (.-oldcache self) old')
                       (set! (.-srccache self) newv)
                       (if (or (not check-equals?)
                               (not= old' new'))
                         (run-watchers old' new'))))))]
         (locking self
           (set! (.-watchers self) (assoc watchers key cb))
           (when (= (count (.-watchers self)) 1)
             (add-watch src id main-watcher)))
         self))

     (removeWatch [self key]
       (locking self
         (set! (.-watchers self) (dissoc watchers key))
         (when (empty? watchers)
           (remove-watch src id)))))

   :cljs
   (deftype RWFocus [id lens src check-equals?
                     ^:mutable watchers
                     ^:mutable srccache
                     ^:mutable oldcache
                     ^:mutable cache]
     IAtom
     IDeref
     (-deref [self]
       (if (identical? srccache @src)
         cache
         (let [source (deref src)
               result (focus lens source)]
           (set! (.-srccache self) source)
           (set! (.-oldcache self) (.-cache self))
           (set! (.-cache self) result)
           result)))

     IReset
     (-reset! [self newval]
       (swap! src #(put lens newval %))
       (deref self))

     ISwap
     (-swap! [self f]
       (swap! src (fn [s] (over lens f s)))
       (deref self))
     (-swap! [self f x]
       (swap! src (fn [s] (over lens #(f % x) s)))
       (deref self))
     (-swap! [self f x y]
       (swap! src (fn [s] (over lens #(f % x y) s)))
       (deref self))
     (-swap! [self f x y more]
       (swap! src (fn [s] (over lens #(apply f % x y more) s)))
       (deref self))

     IWatchable
     (-add-watch [self key cb]
       (letfn [(run-watchers [oldv newv]
                 (doseq [[key wf] (.-watchers self)]
                   (wf key self oldv newv)))
               (main-watcher [_ _ oldv newv]
                 (if (identical? newv (.-srccache self))
                   (if-not check-equals?
                     (run-watchers (.-oldcache self)
                                   (.-cache self)))
                   (let [old' (focus lens oldv)
                         new' (focus lens newv)]
                     (set! (.-cache self) new')
                     (set! (.-oldcache self) old')
                     (set! (.-srccache self) newv)
                     (if (or (not check-equals?)
                             (not= old' new'))
                       (run-watchers old' new')))))]
         (set! (.-watchers self) (assoc watchers key cb))
         (when (= (count (.-watchers self)) 1)
           (add-watch src id main-watcher))
         self))

     (-remove-watch [self key]
       (set! (.-watchers self) (dissoc watchers key))
       (when (empty? watchers)
         (remove-watch src id)))))

#?(:clj
   (deftype ROFocus [id lens src check-equals?
                     ^:volatile-mutable watchers
                     ^:volatile-mutable srccache
                     ^:volatile-mutable oldcache
                     ^:volatile-mutable cache]
     clojure.lang.IDeref
     (deref [self]
       (locking self
         (if (identical? srccache @src)
           cache
           (let [source (deref src)
                 result (focus lens source)]
             (set! (.-srccache self) source)
             (set! (.-oldcache self) (.-cache self))
             (set! (.-cache self) result)
             result))))

     clojure.lang.IRef
     (addWatch [self key cb]
       (letfn [(run-watchers [oldv newv]
                 (doseq [[key wf] (.-watchers self)]
                   (wf key self oldv newv)))
               (main-watcher [_ _ oldv newv]
                 (locking self
                   (if (identical? newv (.-srccache self))
                     (if-not check-equals?
                       (run-watchers (.-oldcache self)
                                     (.-cache self)))
                     (let [old' (focus lens oldv)
                           new' (focus lens newv)]
                       (set! (.-cache self) new')
                       (set! (.-oldcache self) old')
                       (set! (.-srccache self) newv)
                       (if (or (not check-equals?)
                               (not= old' new'))
                         (run-watchers old' new'))))))]
         (locking self
           (set! (.-watchers self) (assoc watchers key cb))
           (when (= (count (.-watchers self)) 1)
             (add-watch src id main-watcher)))
         self))

     (removeWatch [self key]
       (locking self
         (set! (.-watchers self) (dissoc watchers key))
         (when (empty? watchers)
           (remove-watch src id)))))

   :cljs
   (deftype ROFocus [id lens src check-equals?
                     ^:mutable watchers
                     ^:mutable srccache
                     ^:mutable oldcache
                     ^:mutable cache]
     IDeref
     (-deref [self]
       (if (identical? srccache @src)
         cache
         (let [source (deref src)
               result (focus lens source)]
           (set! (.-srccache self) source)
           (set! (.-oldcache self) (.-cache self))
           (set! (.-cache self) result)
           result)))

     IWatchable
     (-add-watch [self key cb]
       (let [key (prefix-key key id)]
         (add-watch src key (fn [key _ oldval newval]
                                 (let [old' (focus lens oldval)
                                       new' (focus lens newval)]
                                   (when (or (not check-equals?)
                                             (not= old' new'))
                                     (cb key self old' new')))))))
     (-remove-watch [self key]
       (set! (.-watchers self) (dissoc watchers key))
       (when (empty? watchers)
         (remove-watch src id)))))

(defn derive
  "Create a derived atom from an other atom with the provided lense.

  The returned atom is lazy, so no code is executed until user requires it.

  By default the deriveed atom does not trigger updates if the data does not
  affects to it (determined by lense), but this behavior can be deactivated
  passing `:check-equals?` to `false` on the third options parameter.

  You can create expliclitly read only refs (not atoms, because the returned
  object satisifies watchable and ref but not atom interface) passing
  `:read-only?` as `true` as option on the optional third parameter."
  ([lens src]
   (derive lens src nil))
  ([lens src {:keys [read-only? check-equals?]
              :or {read-only? false
                   check-equals? true}}]
   (let [id (keyword (gensym "lentes-derived-atom"))]
     (if read-only?
       (ROFocus. id lens src check-equals? nil +empty+ +empty+ +empty+)
       (RWFocus. id lens src check-equals? nil +empty+ +empty+ +empty+)))))
