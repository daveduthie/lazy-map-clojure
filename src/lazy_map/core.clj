(ns lazy-map.core
  "Create lazy-maps, whose values are only calculated when they are
  looked up for the first time, see [[lazy-map]]"
  {:author "Artur Malabarba"}
  (:import clojure.lang.Delay))

(defprotocol Holder
  "Hold a value."
  (getv [a] "Return object, resolving it if delayed."))

(extend-protocol Holder
  Delay
  (getv [a] (force a)))

(defn- getval
 "Wrapper around `Holder` protocol, so we don't have to extend the protocol to
  every type we might want to put in a map."
  [x]
  (if (satisfies? Holder x)
    (getv x)
    x))

;;; Map Definition
(deftype LazyMap [^clojure.lang.IPersistentMap contents]
  clojure.lang.IPersistentMap
  (assoc [_ k v]
    (LazyMap. (.assoc contents k v)))
  (assocEx [_ k v]
    (LazyMap. (.assocEx contents k v)))
  (without [_ k]
    (LazyMap. (.without contents k)))

  java.lang.Iterable
  (iterator [this]
    (.iterator
     ^java.lang.Iterable
     (into {} (map (fn [[k v]] [k (getval v)]) contents))))

  clojure.lang.Associative
  (containsKey [_ k]
    (.containsKey contents k))
  (entryAt [_ k]
    (getval (.entryAt contents k)))

  clojure.lang.IPersistentCollection
  (count [_] (.count contents))
  (empty [_] (.empty contents))
  (cons [_ o]
    (LazyMap. (.cons contents o)))
  (equiv [_ o]
    (and (isa? (class o) LazyMap)
         (.equiv contents (.contents ^LazyMap o))))

  clojure.lang.Seqable
  (seq [_] (.seq contents))

  clojure.lang.ILookup
  (valAt [_ k]
    (getval (.valAt contents k)))
  (valAt [_ k not-found]
    (getval (.valAt contents k not-found))))

;;; Map creation
(defmacro lazy-map
  "Return a LazyMap created from a map `m`.
  The values in `m` are only evaluated when accessed.

  ```clojure
  user> (def my-map
          (lazy-map
           {:cause (do (println \"Getting Cause\")
                       :major-failure)
            :name (do (println \"Getting Name\")
                      \"Some Name\")}))
  #'user/my-map

  user> (:name my-map)
  Getting Name
  \"Some Name\"

  user> (:name my-map)
  \"Some Name\"
  ```"
  [m]
  `(LazyMap.
    ~(into {} (map (fn [[k v]] [k `(delay ~v)]) m))))
