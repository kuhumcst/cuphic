Cuphic
======
Cuphic<sup>[†](#note-pronunciation)</sup> is a macro-free, declarative DSL for performing [Hiccup](https://github.com/weavejester/hiccup) data transformations in Clojure/ClojureScript. It is designed to be both easy to use and simple to understand.

> _<a name="note-pronunciation"><sup>†</sup></a> The name is pronounced *CUP*-hic, not *QUEUE*-fig._

Introduction
------------
Cuphic is essentially a superset of Hiccup where certain symbols have a special meaning:

* `?` - a single-value placeholder.
* `*` - a multi-value placeholder (0 or more).
* `+` - a multi-value placeholder (1 or more).

Used on their own, they will stand in for a mix of values when doing comparisons between Hiccup and Cuphic. However, they can also be used as _prefixes_ for named values that may be captured or inserted using Cuphic, e.g. `?tag`, `*content`, `+items`.

Cuphic _looks_ like Hiccup and can _only_ transform Hiccup, but in return respects the set of assumptions that come with looking like Hiccup, e.g. treating attribute maps as optional. Cuphic data can be conformed with [clojure.spec](https://clojure.org/about/spec) and spec validation is also performed sporadically as part of the core algorithm.

---

The Cuphic library is based on two primary functions:

* `bindings` - captures values from Cuphic.
* `apply-bindings` - inserts values into Cuphic.

These are in turn used by the following functions:

* `matches` - checks if the given Hiccup matches the Cuphic.
* `transform` - transforms Hiccup based on Cuphic from/to templates.
* `transformer` - returns a function to transform Hiccup based on Cuphic from/to templates.
* `rewrite` - takes a sequence of transformers and rewrites a Hiccup tree based on their return values.

Basic usage
-----------
> _Note: consider the examples prepended with `(require '[cuphic.core :refer :all])`_
>
This is a Cuphic template that can be used to either **bind** or **insert** two values:

```clojure
[?tag {:id ?id} "some text"]
```

Symbols prefixed with a question mark such as `?tag` and `?id` are bound to the values of a matching Hiccup structure.

````clojure
(bindings '[?tag {:id ?id} "some text"]                   ; cuphic
          [:div {:id "my-id"} "some text"])               ; hiccup

;;=> {?tag :div, ?id "my-id"}
````

These `symbol->value` bindings can be used _as-is_ or inserted into another Cuphic template:

```clojure
(apply-bindings '{?tag :p, ?id "my-id"}                   ; symbol->value
                '[:p {:id ?id} "some other text"])        ; cuphic

;;=> [:p {:id "my-id"} "some other text"]
```

You don't have to use all the bindings. Omitting a name from a special symbol can be used to ignore values when producing bindings from a Cuphic template, i.e. in the previous example `?tag` should probably have been `?` since we don't actually care about the value of `?tag`.

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
Cuphic is not dogmatic about being declarative. If you ever need to veer into algorithm territory, you can just leave the declarative DSL and substitute either of the two Cuphic templates with an equivalent function.

The `from` template can be replaced with a `hiccup->bindings` function:

```clojure
(transform (fn [hiccup]                                   ; hiccup->bindings
             (when (and (map? (second hiccup))
                        (contains? (second hiccup) :id)
                        (= (last hiccup) "some text"))
               {'?id (:id (second hiccup))}))
           '[:p {:id ?id} "some other text"]              ; to cuphic
           [:div {:id "my-id"} "some text"])              ; hiccup
```

The `to` template can be replaced with a `bindings->hiccup` function:

```clojure
(transform '[?tag {:id ?id} "some text"]                  ; from cuphic
           (fn [{:syms [?id]}]                            ; bindings->hiccup
             [:p {:id ?id} "some other text"])
           [:div {:id "my-id"} "some text"])              ; hiccup
```

Functions can be useful in certain tricky situations, but you should also be able to see the value of using Cuphic for doing most of your Hiccup transformations.

In cases where you need to _postprocess_ the bound values, using a function does become necessary. Fortunately, wrapping an existing Cuphic template with `(fn [{:syms [...]}] ...)` is enough to let you do function calls inside it.

More examples
-------------
_TODO: expand this section, e.g. `matches`, `rewrite` when it's more stable, `...` when it's done, `select` when it's been converted._

Miscellaneous 
-------------
### Use of zippers
Zippers appear in two separate places in Cuphic:

* As part of the `rewrite` function where a **single** pass through a Hiccup tree is used to compare every single node to the given list of transformers, inserting changes into the tree when applicable.
* As part of the `bindings` function where **two** zippers are traversed in **parallel**, comparing each individual part of the Hiccup to the Cuphic. This allows for an early break out of the loop once the Hiccup doesn't match.

### Why not Meander?
After researching various alternatives, I first started using [Meander](https://github.com/noprompt/meander) for doing `hiccup->hiccup` data transformations in my other project, [rescope](https://github.com/kuhumcst/rescope).

While Meander is quite capable, its more universal DSL didn't _seem_ easier to read or write (to me) than normal Clojure code. The main reason to prefer a declarative DSL is because it makes things clearer. Unlike Cuphic, Meander has to accommodate completely heterogeneous Clojure data, so its DSL can't rely on any implicit assumptions about the shape of the data.

I also prefer avoiding macros when it's feasible to do so.

Development
-----------
See [development.md](doc/development.md) for how to develop the Cuphic library itself.
