# serenity

> "Can't stop the signal, Mal. Everything goes somewhere, and I go everywhere." - _Serenity_

Serenity is a library for doing reactive programming. It allows you to build up
a graph of calculations, similar in concept to a spreadsheet, that automatically
re-compute on changes to inputs. [Computation is incremental](https://en.wikipedia.org/wiki/Incremental_computing),
meaning that only when a value that is listened to has actually changed will its
dependents be recomputed.

It's primary use case is to provide UIs the ability to incrementally compute updates.
It's focus is on performance, ergonomics, fault tolerance and reliability.

## Example

## Installation

## Features

First release will include:

- [x] Incrementally compute new values based on updates
- [x] Only computes what's listened to
- [x] Tolerant to errors - never get into an inconsistent state
- [ ] Batching
- [ ] Transactions
- [ ] Transducers
- [ ] Higher order operators

Stretch:

- [ ] Async time slicing
- [ ] Concurrent prioritization

## API docs

## License

Copyright 2020 Will Acton

Distributed under the EPL 2.0
