# cl-history
*Those who don't remember are doomed to repeat*

### The Basics

A history-aware data structure is one that keeps track of its complete set of history states, rather than just a particular current state. A simple example would be representing a textual document as a list of insertions/deletions starting with the empty string rather than merely a string representing the current document. Or, think about a toy bank account represented as a list of deposit/withdrawal transactions rather than merely a numeric balance.

In general, these data structures are useful anywhere you can express a particular construct in terms of a list of steps necessary to construct it, rather than a flat value. You get some good things out of looking at the world this way. Firstly, the ability to see what the state of the construct was at any point in its history, rather than only what its current state is. Secondly, the possibility to implement

- easy, incremental disk saves and distributed storage
- easy history navigation, including undo, branching and tagging operations
- easy-ish concurrent editing capability (it's never *easy*, but seems to be relatively simple with this model. Take a look at [`cl-distributed`](https://github.com/Inaimathi/cl-distributed) to see a small proof-of-concept)

`cl-history` is not a history-aware data-structure, it's a minimal toolkit meant to help you build history-aware data-structures.

### Usage (TODO - maybe a tutorial?)

1. Figure out what your `part` *(a single event in the history of your structure)* and `whole` *(the collapsed form of your structure at a particular point in time)* look like
2. Define your own `apply-payload`, dealing with the above
3. Optionally, define your own `reconcile`, if you're looking to create a concurrently editable structure

### External Symbol Documentation (TODO)

`current-id`
`current`
`zero`
`apply-payload`
`insert!`
`update!`
`load-from!`

`event`
`id`
`timestamp`
`payload`

`archive`
`mk-archive`
`project`
`events`
`events-since`

`reconcile`
