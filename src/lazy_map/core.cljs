(ns lazy-map.core
  "Create lazy-maps, whose values are only calculated when they are
  looked up for the first time, see [[lazy-map]]"
  {:author "Artur Malabarba"}
  (:require [cljs.core :refer [Delay]])
  (:require-macros [lazy-map.core :refer [lazy-map]]))

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
(deftype LazyMap [contents]
  IAssociative
  (-assoc [_ k v]
    (LazyMap. (assoc contents k v)))
  (-contains-key? [_ k]
    (-contains-key? contents k))

  IMap
  (-dissoc [_ k]
    (-> contents
        (-dissoc k)
        LazyMap.))

  IIterable
  (-iterator [this]
    (-iterator (into {} (map (fn [[k v]] [k (getval v)]) contents))))

  ICounted
  (-count [_]
    (-count contents))

  IEmptyableCollection
  (-empty [_]
    (-empty contents))

  ICollection
  (-conj [_ o]
    (LazyMap. (-conj contents o)))

  IEquiv
  (-equiv [_ other]
    (-equiv contents other))

  ISeqable
  (-seq [_]
    (-seq contents))

  ILookup
  (-lookup [_ k]
    (getval (-lookup contents k)))
  (-lookup [_ k not-found]
    (getval (-lookup contents k not-found)))

  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-pr-writer contents writer opts)))
