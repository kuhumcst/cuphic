Cuphic
======
Cuphic<sup>[†](#note-pronunciation)</sup> is a macro-free, declarative DSL for performing [Hiccup](https://github.com/weavejester/hiccup) data transformations in Clojure/ClojureScript using patterns that look like Hiccup. It can also be used to scrape Hiccup data. Cuphic is designed to be both easy to use and simple to understand.

> _<a name="note-pronunciation"><sup>†</sup></a> The name is pronounced *CUP*-hic, not *QUEUE*-fig._

Introduction
------------
Cuphic is essentially a superset of Hiccup where certain symbols have a special meaning:

* `?` - a single-value placeholder.
* `*` - a multi-value placeholder (0 or more).
* `+` - a multi-value placeholder (1 or more).
* `[:<> ...]` - a fragment (repeating pattern among child nodes, 1 or more). This syntax is inspired by [Reagent's](https://github.com/reagent-project/reagent) version of React fragments.

Used on their own, they will stand in for a mix of values when doing comparisons between Hiccup and Cuphic. However, they can also be used as _prefixes_ for named values that may be captured or inserted using Cuphic, e.g. `?tag`, `*content`, `+items`.

Note that currently only a _single_ fragment may exist within a piece of Cuphic. The captured fragment section bindings always map to the symbol `<>` in the resulting bindings map.

---

Cuphic _looks_ like Hiccup and can _only_ transform Hiccup, but in return respects the set of assumptions that come with looking like Hiccup, e.g. treating attribute maps as optional. Cuphic data can be conformed with [clojure.spec](https://clojure.org/about/spec) and spec validation is also performed sporadically as part of the core algorithm.

The Cuphic library is based on two primary functions:

* `get-bindings` - captures values from Cuphic.
* `apply-bindings` - inserts values into Cuphic.

These are in turn used by the following functions:

* `matches` - checks if the given Hiccup matches the Cuphic.
* `transform` - transforms Hiccup based on Cuphic from/to patterns.
* `->transformer` - returns a function to transform Hiccup based on Cuphic from/to patterns.
* `rewrite` - rewrites a Hiccup tree based one or more stages of transformations.

There is also separate search functionality:

* `scan` - scans a Hiccup tree and returns matches based on Cuphic patterns.
* `scrape` - groups the return values of `scan` inside a slighly more palatable map.

Basic usage
-----------
> _Note: consider the examples prepended with `(require '[cuphic.core :refer :all])`_
>
This is a Cuphic pattern that can be used to either **bind** or **insert** two values:

```clojure
[?tag {:id ?id} "some text"]
```

Symbols prefixed with a question mark such as `?tag` and `?id` are bound to the values of a matching Hiccup structure.

````clojure
(bindings '[?tag {:id ?id} "some text"]                   ; cuphic
          [:div {:id "my-id"} "some text"])               ; hiccup

;;=> {?tag :div, ?id "my-id"}
````

These `symbol->value` bindings can be used _as-is_ or inserted into another Cuphic pattern:

```clojure
(apply-bindings '{?tag :p, ?id "my-id"}                   ; symbol->value
                '[:p {:id ?id} "some other text"])        ; cuphic

;;=> [:p {:id "my-id"} "some other text"]
```

You don't have to use all the bindings. Omitting a name from a special symbol can be used to ignore values when producing bindings from a Cuphic pattern, i.e. in the previous example `?tag` should probably have been `?` since we don't actually care about the value of `?tag`.

The two functions can also be combined:

```clojure
(transform '[?tag {:id ?id} "some text"]                  ; from cuphic
           '[:p {:id ?id} "some other text"]              ; to cuphic
           [:div {:id "my-id"} "some text"])              ; hiccup

;;=> [:p {:id "my-id"} "some other text"]
```

> Note: Cuphic works recursively, so you can also match against and extract values from more complex Hiccup structures than shown here.

Functions as an escape hatch
----------------------------
Cuphic is not dogmatic about being declarative. If you ever need to veer into algorithm territory, you can just leave the declarative DSL and substitute either of the two Cuphic patterns with an equivalent function.

The `from` pattern can be replaced with a `hiccup->bindings` function:

```clojure
(transform (fn [hiccup]                                   ; hiccup->bindings
             (when (and (map? (second hiccup))
                        (contains? (second hiccup) :id)
                        (= (last hiccup) "some text"))
               {'?id (:id (second hiccup))}))
           '[:p {:id ?id} "some other text"]              ; to cuphic
           [:div {:id "my-id"} "some text"])              ; hiccup
```

The `to` pattern can be replaced with a `bindings->hiccup` function:

```clojure
(transform '[?tag {:id ?id} "some text"]                  ; from cuphic
           (fn [{:syms [?id]}]                            ; bindings->hiccup
             [:p {:id ?id} "some other text"])
           [:div {:id "my-id"} "some text"])              ; hiccup
```

Functions can be useful in certain tricky situations, but you should also be able to see the value of using Cuphic for doing most of your Hiccup transformations.

In cases where you need to _postprocess_ the bound values, using a function does become necessary. Fortunately, wrapping an existing Cuphic pattern with `(fn [{:syms [...]}] ...)` is enough to let you do function calls inside it.

More examples
-------------
_TODO: expand this section, e.g. `matches`, `rewrite` when it's more stable, `...` when it's done, `select` when it's been converted._

Architecture and design
-----------------------
This section contains relevant documentation pertaining to the architecture and design of Cuphic.

### Capturing bindings
When capturing bound values among child nodes, a certain order of operations is observed.

| Order of operations |
|---------------------|
| `[:<> ...]`         |
| `?` and `"text"`    |
| `*` and `+`         |


Before any other matching/capturing can occur, the node head itself - its HTML tag and attr - will be matched against the head of the Cuphic and its values potentially captured. This constant time check is the primary way Cuphic can stay somewhat performant. Only when that check is successful will the algorithm move on to the child nodes.

Within the child nodes, instances of any available fragment (`[:<> ...]`) will be located and captured. This is be done in approximately `O(n+m)` time where `m` is the size of the fragment itself and `n` is the amount of child nodes. Note that there can only be a single fragment among the child nodes!

The remaining nodes to either side of the captured fragment section are then handled separately. The nodes are sequentially matched against alternating fixed-length and quantifier patterns. The quantifier patterns feature some lookahead as they need to capture all the way until the appearance of the next fixed-length pattern.
 
 ```clojure
;; Values lined up with their capturing symbols. 
(get-bindings '[:p  ?x   *between   ?y   [:<> 0 ?a ?b]   +remainder]
            [:p  1,   2 3 4 5,   6,   0 1 2  0 1 2,   7 8 9     ])

;;=> {?x 1, *between [2 3 4 5], ?y 6, <> [{?a 1, ?b 2} {?a 1, ?b 2}], +remainder [7 8 9]}
```

Performant Cuphic should be written to be as specific as possible with the outer head (tag and attr) serving as the primary way of limiting scope, thereby improving performance.

#### Dealing with ambiguity
For the bindings extraction algorithm to work best, there must be little ambiguity in the Cuphic.

```clojure
(get-bindings '[?tag ?y +middle [:<> ?x]]
           [:div 1 2 3 4 5])

;;=> {?tag :div, ?y 1, +middle [2], <> [{?x 3} {?x 4} {?x 5}]}
```

In somes cases there is no clear boundary between the different values.

1. The algorithm determines the fragment boundary is by calculating a `min-count` for the sections on either side of the fragment. This creates a bounded context for the fragment nodes.
2. The algorithm will then try to find repeated instances of the fragment pattern, capturing every single child node inside this bounded context.
3. Finally, the remaining nodes outside the fragment context are matched/captured by the other parts of the Cuphic vector.

### Use of zippers
Zippers are used for two separate purposes in Cuphic:

* As part of the `rewrite` function where a **single** pass through a Hiccup tree is used to compare every single node to the given list of transformers, inserting changes into the tree when applicable.
* As part of the `bindings` function where **two** zippers are traversed in **parallel**, comparing each individual part of the Hiccup to the Cuphic. This allows for an early break out of the loop once the Hiccup doesn't match.

Miscellaneous 
-------------
### Why not Meander?
After researching various alternatives, I first started using [Meander](https://github.com/noprompt/meander) for doing `hiccup->hiccup` data transformations in my other project, [rescope](https://github.com/kuhumcst/rescope).

While Meander is quite capable, its more universal DSL didn't _seem_ easier to read or write (to me) than normal Clojure code. The main reason to prefer a declarative DSL is because it makes things clearer. Unlike Cuphic, Meander has to accommodate completely heterogeneous Clojure data, so its DSL can't rely on any implicit assumptions about the shape of the data.

I also prefer avoiding macros when it's feasible to do so.

Development
-----------
See [development.md](doc/development.md) for how to develop the Cuphic library itself.
