# io.github.bsless/clj-kanren

Learning the Kanren framework by doing.

## What (the)

core.logic is big and mature. It's a lot to take in. I decided to
experiment with a layered implementation starting from microKanren.

The project is built for my learning and for future extension.

## Usage

Require core namespaces:

```clojure
(require '[io.github.bsless.kanren.micro as uk]
         '[io.github.bsless.kanren.mini as mk])
```

Require a substitution implementation:

```clojure
(require 
  '[io.github.bsless.kanren.micro.impl.substitution :as s])
```

Set the empty substitution "factory":

```clojure
(uk/set-empty-state-fn! s/make-empty-state)
```

Run stuff:

```clojure
(mk/run 2 [a b] (uk/=== a 1) (uk/=== b 2))
```

## Design

The Kanren framework is built by a collection of works. Each layer
exposes the APIs it requires by a namespace with a name matching it
(micro, mini).

The implementations are under `LAYER.impl/*`.

Additional protocols are in the `protocols.clj` file.

Extensions for these protocols or new types will also be in their own
namespaces, see:
- `micro.impl.lcons`
- `mini.impl.reify`

### Protocols

Because of the layered design, we can (ab)use protocols and extend them
to type which implement them as methods in core.logic.

- pros: 
  - Smaller implementation of core types (lcons, lvar, etc)
  - Additional interfaces are easier to take in
- cons:
  - performance
  - more to keep track of?


## Differences from core.logic

- Not complete
- Streams are implemented as reducibles and not lazy sequences.
- Names are changed

## License

Copyright © 2021 Ben Sless

Rich Hickey and David Nolen for core.logic which served as a reference.

Byrd et al 

Jason Hemann and Daniel P. Friedman

Distributed under the Eclipse Public License version 1.0.
