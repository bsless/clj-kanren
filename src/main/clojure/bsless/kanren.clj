(ns bsless.kanren
  (:import
   (clojure.lang IPersistentCollection)
   (clojure.lang IPersistentMap Named)
   (java.io Writer)))

;;; Utility functions

(set! *warn-on-reflection* true)

(defmacro -append [sb x]
  (cond
    (symbol? x)
    `(let [x# ~x] (if (nil? x#) nil (.append ~sb x#)))
    (nil? x) nil
    (seqable? x) `(.append ~sb ~x)
    :else `(.append ~sb ~(.toString ^Object x))
    ))

(defmacro -str
  ([a]
   (cond
     (symbol? a) `(let [a# ~a] (if (nil? a#) "" (.toString ^Object a#)))
     (nil? a) ""
     :else (.toString ^Object a)))
  ([a & args]
   (let [sb (with-meta (gensym "sb__") {:tag 'StringBuilder})]
     `(let [~sb (StringBuilder.)]
        ~@(map (fn [x] `(-append ~sb ~x)) (cons a args))
        (.toString ~sb)))))

;;; Micro Kanren

;;; Protocols

(defprotocol IMPlus
  "Add two streams of results.
  A fair implementation interleaves them."
  (-mplus [this g]))

(defprotocol IBind
  "Interweave to streams of results.
  If you want to understand this read Kiselyov's paper on LogicM."
  (-bind [this g]))

(defmacro -inc [& rest]
  `(fn* ~'-inc [] ~@rest))

;;; MZero

(extend-type nil
  IBind
  (-bind [_ _] nil)
  IMPlus
  (-mplus [_ f] (f)))

(extend-type clojure.lang.Fn
  IBind
  (-bind [this g]
    (-inc (-bind (this) g)))
  IMPlus
  (-mplus [this f]
    ;; Interleaving is achieved by switching the order of arguments
    ;; The stream is ensured to be "cold" because it's wrapped in [[-inc]]
    (-inc (-mplus (f) this))))

;;; If a stream is delayed in a thunk, invoke it

(defn pull
  [f]
  (if (instance? clojure.lang.Fn f)
    (recur (f))
    f))

(defprotocol IUnify
  "Type based dispatch for terms unification.
  Every term specifies how it can unify with other objects in a given
  substitution."
  (-unify-terms [u v s] "Unify `u` with `v` in substitution `s`."))

(defn unify-terms
  [u v s]
  (-unify-terms u v s))

(definterface ITreeTerm)

(defn tree-term? [x]
  (or
   (instance? clojure.lang.IPersistentCollection x)
   (instance? ITreeTerm x)))

(defprotocol ISubstitution
  "Base protocol for substitution behavior.
  A substitution implementation is defined by:
  [[-walk]]: triangular search semantics
  [[-unify-in]]: How to unify two variables in the substitution.
  [[-ext]]: Extending the substitution.
  [[-find]] and [[-lookup]]: lookup semantics."
  (-walk [s u] "Chase down `u` in `s` to its ground value.")
  (-unify-in [s u v] "Given that `u` and `v` need to be unified, unify
  them in the specific implementation of the substitution `s`.")
  (-ext [s u v] "Extend the substitution `s` to associate `v` to `u`.")
  (-find [s v] "Find `v` in `s`, returns entry.")
  (-lookup [s v] "Get `v` in `s` or nil."))

(defn ext [s u v] (-ext s u v))

(defprotocol IBindable
  (-bindable? [this]))

(extend-protocol IBindable
  nil (-bindable? [_] false)
  Object (-bindable? [_] false))

;;; Choice

(deftype Choice [a f]
  Object
  (toString [_] (str "<choice: " a ", ...>"))

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [_ k not-found]
    (if (.equals :a k)
      a
      not-found))
  IBind
  (-bind [_ g]
    (-mplus (g a) (-inc (-bind f g))))
  IMPlus
  (-mplus [_ fp]
    (Choice. a (-inc (-mplus (fp) f))))
  clojure.lang.Seqable
  (seq [_]
    (lazy-seq (cons a (lazy-seq (pull f)))))
  clojure.lang.IReduceInit
  (reduce [_ g init]
    (let [r (g init a)]
      (if (reduced? r)
        @r
        (reduce g r (pull f)))))
  clojure.lang.IReduce
  (reduce [_ g]
    (reduce g a (pull f))))

(defn choice
  {:inline
   (fn [a f]
     `(new Choice ~a ~f))}
  ^Choice [a f]
  (Choice. a f))

;;; Unit

(extend-type Object
  IMPlus
  (-mplus [this f]
    (Choice. this f)))

;;; LVar

(definterface IVar)

(deftype LVar [^long id
               ^boolean unique
               ^:unsynchronized-mutable ^String name
               ^String oname
               ^:unsynchronized-mutable ^int hash
               meta]
  IVar
  clojure.lang.ILookup
  Named
  (getName [_]
    (when (nil? name)
      (set! name (str id)))
    name)
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :name (.getName ^Named this)
      :oname oname
      :id id
      not-found))

  clojure.lang.IObj
  (meta [_]
    meta)
  (withMeta [_ new-meta]
    (LVar. id unique name oname hash new-meta))

  Object
  (toString [_] (str "<lvar:" name ">"))

  (equals [this o]
    (if (instance? LVar o)
      (if unique
        (if (= id (.-id ^LVar o)) true false)
        (if (.equals (.getName this) (.getName ^Named o)) true false))
      false))

  (hashCode [this]
    (when (== hash -1)
      (set! hash (.hashCode (.getName ^Named this))))
    hash)

  IUnify
  (-unify-terms [u v s] (ext s u v))

  IBindable
  (-bindable? [_] true)

  #_#_
  IOccursCheckTerm
  (occurs-check-term [v x s] (= (walk s v) x))

  #_#_
  IBuildTerm
  (build-term [u s]
    (let [m (:s s)
          cs (:cs s)
          lv (lvar 'ignore) ]
      (if (contains? m u)
        s
        (make-s (assoc m u lv) cs)))))


(defn lvar
  #_{:inline
   (fn
     ([] `(let [id# (. clojure.lang.RT (nextID))
                name# (str id#)]
            (LVar. id# true name# nil (.hashCode name#) nil)))
     ([name] `(lvar ~name true))
     ([name unique]
      `(let [name# ~name
             oname# name#
             unique# ~unique
             id#   (if unique#
                    (. clojure.lang.RT (nextID))
                    name#)
             name# (if unique#
                    (-str name# "__" id#)
                    (-str name#))]
         (LVar. id# unique# name# oname# (.hashCode name#) nil))))
   :inline-arities #{1 2 3}}
  ([]
   (let [id (. clojure.lang.RT (nextID))
         #_#_name (str id)]
     (LVar. id true nil nil -1 nil)))
  ([name]
   (lvar name true))
  ([name unique]
   (let [oname name
         id   (if unique
                (. clojure.lang.RT (nextID))
                name)
         name (if unique
                (-str name "__" id)
                (-str name))]
     (LVar. id unique name oname (.hashCode name) nil))))

(defmethod print-method LVar [x ^Writer writer]
  (.write writer (str "<lvar:" (:name x) ">")))

(definline lvar? [x]
  `(instance? LVar ~x))

(defn lvars [n]
  (repeatedly n lvar))

;;; LCons

(defprotocol LConsSeq
  "Clojure doesn't have improper lists but we need them.
  This is a good old pair."
  (lfirst [this])
  (lnext [this]))

(defprotocol LConsPrint
  (toShortString [this]))

(declare unify-lcons-terms)

(deftype LCons [a d ^{:unsynchronized-mutable true :tag int} cache meta]
  ITreeTerm

  clojure.lang.IObj
  (meta [_] meta)
  (withMeta [_ new-meta] (LCons. a d cache new-meta))

  LConsSeq
  (lfirst [_] a)
  (lnext [_] d)

  LConsPrint
  (toShortString [this]
    (if (.. this getClass (isInstance d))
      (-str a " " (toShortString d))
      (-str a " . " d )))

  Object
  (toString [this]
    (if (.. this getClass (isInstance d))
      (-str "(" a " " (toShortString d) ")")
      (-str "(" a " . " d ")")))

  (equals [this o]
    (if (identical? this o)
      true
      (and (.. this getClass (isInstance o))
           (loop [me this
                  you o]
             (cond
               (nil? me) (nil? you)
               (lvar? me) true
               (lvar? you) true
               (and (instance? LCons me) (instance? LCons you))
               (let [mef  (lfirst me)
                     youf (lfirst you)]
                 (when (or (= mef youf)
                           (lvar? mef)
                           (lvar? youf))
                   (recur (lnext me) (lnext you))))
               :else (= me you))))))

  (hashCode [_]
    (if (clojure.core/== cache -1)
      (do
        (set!
         cache
         (-> 31
             unchecked-int
             (unchecked-multiply-int (clojure.lang.Util/hash d))
             (unchecked-add-int
              (clojure.lang.Util/hash a))))
        cache)
      cache))

  IUnify
  (-unify-terms [u v s] (unify-lcons-terms u v s))

  #_#_
  IOccursCheckTerm
  (occurs-check-term [v x s]
    (loop [v v x x s s]
      (if (instance? LCons v)
        (or (occurs-check s x (lfirst v))
            (recur (lnext v) x s))
        (occurs-check s x v))))

  #_#_
  IBuildTerm
  (build-term [u s]
    (loop [u u s s]
      (if (instance? LCons u)
        (recur (lnext u) (build s (lfirst u)))
        (build s u)))))

(defn unify-lcons-terms
  [u v s]
  (cond

    (sequential? v)
    (loop [u u
           v (seq v)
           s s]
      (if (nil? v)
        (if (lvar? u)
          (if-let [s (-unify-in s u '())]
            s
            (-unify-in s u nil))
          nil)
        (if (instance? LCons u)
          (if-let [s (-unify-in s (lfirst u) (first v))]
            (recur (lnext u) (next v) s)
            nil)
          (-unify-in s u v))))

    (instance? LCons v)
    (loop [u u
           v v
           s s]
      (if (lvar? u)
        (-unify-in s u v)
        (cond
          (lvar? v) (-unify-in s v u)
          (and (instance? LCons u) (instance? LCons v))
          (if-let [s (-unify-in s (lfirst u) (lfirst v))]
            (recur (lnext u) (lnext v) s)
            nil)
          :else (-unify-in s u v))))

    :else nil))

(defmethod print-method LCons [x ^Writer writer]
  (.write writer (str x)))

(definline lcons
  "Constructs a sequence a with an improper tail d if d is a logic variable."
  [a d]
  (if (number? d)
    `(LCons. ~a ~d (unchecked-int -1) nil)
    `(let [a# ~a d# ~d]
       (if (instance? IPersistentCollection d#)
         (.cons (.seq ^IPersistentCollection d#) a#)
         (if (nil? d#)
           (new clojure.lang.PersistentList a#)
           (LCons. a# d# (unchecked-int -1) nil))))))

(definline lcons? [x] `(instance? LCons ~x))

(defmacro llist
  "Constructs a sequence from 2 or more arguments, with the last argument as the
   tail. The tail is improper if the last argument is a logic variable."
  ([f s] `(lcons ~f ~s))
  ([f s & rest] `(lcons ~f (llist ~s ~@rest))))

;;; Micro kanren API

(defmacro bind
  ([a g] `(-bind ~a ~g))
  ([a g & g-rest]
   `(bind (-bind ~a ~g) ~@g-rest)))

(defmacro mplus
  ([e] e)
  ([e & e-rest]
   `(-mplus ~e (-inc (mplus ~@e-rest)))))

(defn call-fresh
  {:inline (fn [f] `(fn ~'goal-fresh [s#] ((~f (lvar)) s#)))}
  [f]
  (fn goal-fresh [s] ((f (lvar)) s)))

(defn -disj
  {:inline
   (fn [g1 g2]
     `(fn ~'goal-disjunction [s#] (mplus (~g1 s#) (~g2 s#))))}
  [g1 g2]
  (fn goal-disjunction [s] (mplus (g1 s) (g2 s))))

(defn -conj
  {:inline
   (fn [g1 g2]
     `(fn ~'goal-conjunction [s#] (bind (~g1 s#) ~g2)))}
  [g1 g2]
  (fn goal-conjunction [s] (bind (g1 s) g2)))

(definline walk [s u] `(-walk ~s ~u))

(definline bindable? [x] `(-bindable? ~x))

(defn unify
  [s u v]
  (-unify-in s u v))

(defn ===
  "Goal that will attempt to unify `u` and `v`."
  {:inline (fn [u v] `(fn [s#] (unify s# ~u ~v)))}
  [u v]
  (fn goal-unify [s] (unify s u v)))

;;; Hash map substitution

(defn- -walk-map
  [^IPersistentMap s v]
  (if (-bindable? v)
    (if-let [vp (-lookup s v)] (recur s vp) v)
    v))

(defn- -lookup-map
  [^IPersistentMap s v]
  (let [ret (.valAt s v ::not-found)]
    (if (identical? ret ::not-found)
      nil
      ret)))

(defn- -unify-in-map
  [s u v]
  (if (identical? u v)
    s
    (let [u (-walk-map s u) v (-walk-map s v)]
      (if (and (lvar? u) (= u v))
        s
        (if (and (not (lvar? u)) (lvar? v))
          (unify-terms v u s)
          (unify-terms u v s))))))

(extend IPersistentMap
  ISubstitution
  {:-walk -walk-map
   :-ext (fn -ext [^IPersistentMap m u v] (.assoc m u v))
   :-lookup -lookup-map
   :-find (fn -find [^IPersistentMap s v] (.entryAt s v))
   :-unify-in -unify-in-map})

(defn make-empty-map-state [] {})

(defprotocol ICountedSubs
  (-empty-sub? [_]))

(extend-protocol ICountedSubs
  IPersistentCollection (-empty-sub? [coll] (zero? (count coll))))

(defrecord TrivialSubstitution [^IPersistentMap state]
  ICountedSubs
  (-empty-sub? [_] (zero? (count state)))
  ISubstitution
  (-ext [_ u v] (new TrivialSubstitution (.assoc state u v)))
  (-walk [this v]
    (loop [this this v v]
      (if (-bindable? v)
        (if-let [vp (let [ret (.valAt state v ::not-found)]
                      (if (identical? ret ::not-found)
                        nil
                        ret))]
          (recur this vp) v)
        v)))
  (-lookup [_ v]
    (let [ret (.valAt state v ::not-found)]
      (if (identical? ret ::not-found)
        nil
        ret)))
  (-unify-in [s u v]
    (if (identical? u v)
      s
      (let [u (-walk s u) v (-walk s v)]
        (if (and (lvar? u) (= u v))
          s
          (if (and (not (lvar? u)) (lvar? v))
            (unify-terms v u s)
            (unify-terms u v s)))))))

(defn make-empty-substitution [] (new TrivialSubstitution {}))

(extend-protocol IBind ;; Use hash map as substitution
  clojure.lang.IPersistentMap
  (-bind [this g] (g this)))

;;; Micro Kanren

;;; Protocols


(defprotocol IWalkTerm
  "Ad-hoc implementation of tree walk for various data structures.
  Used in the implementation of reification."
  (-walk-term [v f]))

(defprotocol IReifyTerm
  "Behavior for reifying a specific term in substitution.
  The no-op case returns the substitution unchanged."
  (-reify-term [v s]))

(defn walk-term [f v] (-walk-term v f))

(extend-protocol IWalkTerm

  nil (-walk-term [_ f] (f nil))

  Object (-walk-term [v f] (f v))

  clojure.lang.ISeq
  (-walk-term [v f]
    (with-meta
      (into [] (map #(walk-term f (f %))) v)
      #_(doall (map #(-walk-term (f %) f) v))
      (meta v)))

  clojure.lang.IPersistentVector
  (-walk-term [v f]
    (into [] (map #(walk-term f (f %))) v)
    #_
    (->
     (fn [r v] (conj! r (-walk-term (f (first v)) f)))
     (reduce (transient []) v)
     persistent!
     (with-meta (meta v))))

  clojure.lang.IPersistentMap
  (-walk-term [v f]
    (->
     (fn [r vfk vfv]
       (assoc!
        r
        (walk-term f (f vfk))
        (walk-term f (f vfv))))
     (reduce-kv (transient {}) v)
     persistent!
     (with-meta (meta v))))

  LCons
  (-walk-term [v f]
    (lcons (f (lfirst v))
           (f (lnext v))))

  #_#_clojure.lang.IRecord
  (-walk-term [v f]
    (walk-record-term v f)))

;;; η - look! Eta!
(defmacro -delay [g] `(fn ~'goal-delay [s#] (-inc (~g s#))))

(defmacro conj*
  ([g] `(-delay ~g))
  ([g & gs] `(-conj (-delay ~g) (conj* ~@gs))))

(defmacro disj*
  ([g] `(-delay ~g))
  ([g & gs] `(-disj (-delay ~g) (disj* ~@gs))))

(defmacro conde
  [gs & gss]
  `(disj*
    ~@(map
       (fn conjoin [gs] `(conj* ~@gs))
       (cons gs gss))))

(defmacro fresh
  [[lvar & lvars] & goals]
  (if lvar
    `(call-fresh
      (fn [~lvar]
        (fresh [~@lvars] ~@goals)))
    `(conj* ~@goals)))

(defonce pull-xf (map pull))

(defn take-all [$]
  (->Eduction pull-xf (pull $)))

(defn take-n [n $]
  (->Eduction (comp pull-xf (take n)) (pull $)))

(defn call-empty-state
  [g make-empty-state]
  (g (make-empty-state)))

(defn -run-goal
  [{:keys [n goal make-empty-state]}]
  (let [make-empty-state (or make-empty-state make-empty-map-state)
        ret (call-empty-state goal make-empty-state)]
    (if (nil? n)
      (take-all ret)
      (take-n n ret))))

(defmacro run*
  [{:keys [n bindings goals make-empty-state]
    :or {make-empty-state make-empty-map-state}}]
  (let [[x & xs] bindings]
    (if (seq xs)
      `(run*
         {:n ~n
          :bindings [q#]
          :goals [(fresh [~@bindings]
                    (=== q# (list ~@bindings))
                    ~@goals)]
          :make-empty-state ~make-empty-state})
      `(-run-goal
        {:n ~n
         :goal (fresh [~x] ~@goals (reifyg ~x ~make-empty-state))
         :make-empty-state ~make-empty-state}))))

(defmacro run
  [n bindings & goals]
  `(run* {:n ~n :bindings [~@bindings] :goals [~@goals]}))

;;; Goal reification

(def ^:const unbound-names-count 128)

(def ^:const unbound-names
  (let [r (range unbound-names-count)]
    (zipmap r (map (comp symbol str) (repeat "_") r))))

(defn reify-lvar-name [s]
  (let [c (count s)]
    (if (< c unbound-names-count)
      (unbound-names c)
      (clojure.lang.Symbol/intern (-str "_" (count s))))))

(defn walk* [s v]
  (let [v (walk s v)]
    (-walk-term
     v
     (fn [x]
       (if (tree-term? x)
         (walk* s x)
         (walk s x))
       #_
       (let [x (walk s x)]
         (if (tree-term? x)
           (walk* s x)
           x))))))

(defn -reify* [s v]
  (let [v (walk s v)]
    (-reify-term v s)))

(defn -reify
  #_([s v]
   (let [v (walk* s v)]
     (walk* (-reify* (with-meta (make-empty-state) (meta s)) v) v)))
  ([s v r]
   (let [v (walk* s v)]
     (walk* (-reify* r v) v))))

(extend-protocol IReifyTerm
  LVar
  (-reify-term [v s]
    (let [rf (-> s clojure.core/meta :reify-vars)]
      (if (fn? rf)
        (rf v s)
        (if rf
          (ext s v (reify-lvar-name s))
          (ext s v (:oname v))))))
  LCons
  (-reify-term [v s]
    (loop [v v s s]
      (if (instance? LCons v)
        (recur (lnext v) (-reify* s (lfirst v)))
        (-reify* s v))))

  nil (-reify-term [_ s] s)

  Object (-reify-term [_ s] s)

  clojure.lang.IPersistentCollection
  (-reify-term [v s]
    (reduce -reify* s v)
    #_(loop [v v s s]
      (if (seq v)
        (recur (next v) (-reify* s (first v)))
        s))))

(defonce empty-f (fn []))

(defn reifyg
  "Reification as a goal."
  ([x] (reifyg x make-empty-map-state))
  ([x make-empty-state]
   (fn goal-reify [a]
     (let [v (walk* a x)
           r (-reify* (with-meta (make-empty-state) (meta a)) v)]
       (if (-empty-sub? r)
         (choice v empty-f)
         (choice (walk* r v) empty-f))))))

(comment
  (run 1 [x] (=== x 5))
  (run 1 [x] (=== x x))
  (run 1 [x] (=== x [[1]]))
  (run 1 [x y] (=== x [[1]]) (=== [[y]] x))
  (run 2 [x] (conde [(=== x 5)] [(=== x 6)]))
  (run 2 [a b] (=== a 1) (=== b 2))
  (require '[clj-async-profiler.core :as prof] '[criterium.core :as cc])
  (cc/quick-bench (into [] (run 2 [a b] (=== a 1) (=== b 2))))
  (cc/quick-bench (into [] (run* {:goals [(=== a 1) (=== b 2)], :bindings [a b], :n 2 :make-empty-state make-empty-substitution})))
  (prof/clear-results)
  (prof/serve-files 7777)
  (prof/profile
   {:event :alloc}
   (time
    (dotimes [_ 1e7]
      (into [] (run* {:goals [(=== a 1) (=== b 2)], :bindings [a b], :n 2 :make-empty-state make-empty-substitution}))
      #_(into [] (run 2 [a b] (=== a 1) (=== b 2)))))))
