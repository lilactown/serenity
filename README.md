# serenity

> "Can't stop the signal, Mal. Everything goes somewhere, and I go everywhere." - _Serenity_

Serenity is a library for doing reactive programming. It allows you to build up
a graph of calculations, similar in concept to a spreadsheet, that automatically
re-compute on changes to inputs.

It's primary use case is to provide UIs the ability to incrementally compute updates.
It's focus is on performance, ergonomics, fault tolerance and reliability.

## Example

```clojure
(ns my-app.feature
  (:require
   [serenity.core :as s]))

;; create a source, which is a mutable container that we can send messages to
(def state 
  (s/source (fn [state message]
              (case message
                :inc (update state :count inc)
                :dec (update state :count inc)))
            :initial {:count 0}))


;; create a signal, which is a calculation based on any number of sources
;; or other signals
(def count (s/signal #(:count @state)))


;; create a sink, which is a listener on a source or signal
(def count-logger 
  (s/sink! count (fn [signal old new]
                   (prn new))))

;; immediately prints: 0


;; evaluating this returns the source, and queues the update to be run
(s/send state :inc)

;; messages and sink listeners are batched; running this multiple times will
;; only fire our `count-logger` once
(-> state
  (s/send :inc)
  (s/send :inc))

;; before next tick, prints: 3
```

## Installation

## Features

- [x] Incrementally compute new values based on updates
- [x] Only computes what's listend to
- [x] Tolerant to errors - never get into an inconsistent state
- [x] Batches changes
- [x] Transducers
- [ ] Collect values over time
- [ ] Async time slicing
- [ ] Concurrent prioritization

## API docs

## License

Copyright 2020 Will Acton

Distributed under the EPL 2.0
