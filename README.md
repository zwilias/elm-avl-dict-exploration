# Alternative Dict implementation backed by AVL tree [![Build Status](https://travis-ci.org/zwilias/elm-avl-dict-exploration.svg?branch=master)](https://travis-ci.org/zwilias/elm-avl-dict-exploration)

## Elevator pitch

Red-black trees as used in core's Dict require quite a few modifications for
rebalancing. Worse; they require quite a few comparisons to do the correct
modifications, which is notoriously slow in Elm. AVL trees offer similar
worst-case performance characteristics, but in this case, much better insertion
performance.

## Using Dict.AVL

`Dict.AVL` has the exact same API as core Dict, so using it can be accomplished
by simply doing something like this:

```elm
import Dict.AVL as Dict
```
