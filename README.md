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

### TODO

- What we currently call `base-archive` should really be `archive`, and what we currently call `archive` should really be `in-memory-archive`. Alternatively, `base-archive` could be `disk-archive`, which would let us keep `archive` as is. In either case, you need a `mk-base-archive` (whatever it happens to be called in the end)
- In order to duplicate the stuff we've got in `fact-base`, we'll also need a derivative of `archive` called `indexed-archive` with in-memory indices on parts of `payload`. No idea how we could define that; it might be simpler to keep this as an implementation-specific piece of functionality.
- Think about implementing the `fact-base` style delta clustering. That is, `insert!` and `update!` apply things to an in-memory delta that only periodically gets written to disk. It's a slightly more complicated setup, but might be worth it in some situations. It may be worth implementing as a separate archive type (`clustering-archive` or something. the only worry I have about that is: what happens when we then want a `clustering`+`indexed` archive? Inheritance can take us so far, but something tells me pushing it too hard would end up biting me in the ass somewhere down the road. And actually, it should work fine for that particular use-case, because `clustering` and `indexed` don't overlap anywhere.)
