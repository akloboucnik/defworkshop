(ns defworkshop.state-to-recursion
  (:require [workshoplib.tdd :refer […]]))

;; More often than not, state can be avoided by using recursion. Let's see how it works

(defn reverse-coll
  "Reverse the collection `coll`.

   You can use `loop/recur` construct to loop over the sequence.
   `cons` prepends items to the list, try that out."
  [coll]
  (loop [coll coll
         acc []]
    (if (empty? coll)
      acc
      (recur (rest coll) (cons (first coll) acc)))))

(defn recursive-sum
  "We've already implemented sum using reduce, now let's move to implementing it via recursion!"
  [[head & tail]]
  (if (empty? tail)
    (or head 0)
    (+ head (recursive-sum tail))))

(defn recursive-sum-tc
  "with a tail-recursive version of sum, we can avoid stack overflows."
  ([coll]
     (recursive-sum-tc coll 0))
  ([[head & tail] acc]
     (if (nil? head)
       acc
       (recursive-sum-tc tail (+ acc head)))))

(defn max-from-list
  "Get the maximum from list using recursion"
  [[head & tail]]
  (if (empty? tail)
    head
    (let [max-in-list (max-from-list tail)]
      (if (> head max-in-list)
        head
        max-in-list))))

(defn my-reduce
  "generalizing the recursive sum example, write your own implementation of reduce! (for empty coll, just return nil.)"
  ([f [head & tail]]
     (my-reduce f head tail))
  ([f acc-init coll]
    (let [new-head (first coll)]
      (if (nil? new-head)
        acc-init
        (my-reduce f (f acc-init new-head) (rest coll))))))

(defn max-from-list-tc
  "Get the maximum from list using tail recursion (avoid stack overflow)"
  ([coll]
     (max-from-list-tc coll 0))
  ([[head & tail] max]
     (if (nil? head)
       max
       (if (> head max)
         (max-from-list-tc tail head)
         (max-from-list-tc tail max)))))

(defn loop-recur-sum
  "This implementation is somewhat easier to understand for people coming from imperative style."
  [numbers]
  (loop [acc 0
         [head & tail] numbers]
    (if (nil? head)
      acc
      (recur (+ head acc) tail))))

(defn map-from-keys-and-vals
  "Something that we've already implemented previously in terms of `zipmap`, but are going to implement again
   in terms of recursion. Usually you use `loop/recur` construct whenever you have a one or multiple accumulators
   or several collections you iterate over."
  [keys vals]
  (loop [ map {}
         [key & keys-tail] keys
         [val & vals-tail] vals]
    (if (nil? key)
      map
      (recur (assoc map key val) keys-tail vals-tail))))

(defn parentheses-balanced?
  "Check wether the given string has balanced parantheses or no.

   You can use `cond` statement to avoid deeply nested ints.
   It's a recursive problem, so you'll have to build up stack to solve it.

   `inc` increments a number, `dec` decrements a number."
  ([str] (parentheses-balanced? str 0))
  ([[current & tail] count]
     (if (nil? current)
       (= count 0)
       (cond (= current \() (parentheses-balanced? tail (inc count))
             (= current \)) (parentheses-balanced? tail (dec count))
             true (parentheses-balanced? tail count)))))

