# serenity

> "Can't stop the signal, Mal. Everything goes somewhere, and I go everywhere." - _Serenity_

Serenity is a library for doing reactive programming. It allows you to build up
a graph of calculations, similar in concept to a spreadsheet, that automatically
re-calculates on changes to inputs.

It is intended for use with UI frameworks such as React when you need to store
state outside of the framework, and is built with performance, ergonomics and
fault-tolerance in mind.

## Example

```clojure
(ns my-app.feature
  (:require
   [serentiy.core :as s]))

;; create a source, which is a mutable container that we can send messages to
(def state (s/source
            (fn [state message]
              (case message
                :inc (update state :count inc)
                :dec (update state :count inc)))
            :initial {:count 0}))


;; create a signal, which is a calculation based on any number of sources
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
(s/send state :inc)
(s/send state :inc)

;; before next tick, prints: 3
```

## Installation

## API docs

## License

