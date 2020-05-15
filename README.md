Cuphic
======
Cuphic is a macro-free, declarative DSL for performing [Hiccup](https://github.com/weavejester/hiccup) data transformations in Clojure/ClojureScript. It is designed to be easy to use, but also simple to understand. It strongly resembles Hiccup.

The name is pronounced *CUP*-hic, not *QUEUE*-fig.


Cuphic is only for Hiccup
-------------------------
Cuphic can **only** transform Hiccup and is designed to look pretty much like it (with some logic variables sprinkled on). It respects the set of assumptions that come with looking like Hiccup.
 
It's not dogmatic about being declarative, either. If you ever need to veer into algorithm territory, you can just leave the declarative DSL and substitute either of the two Cuphic templates with an equivalent function. The _from_ template can be replaced with a `hiccup->bindings` function; the _to_ template with a `bindings->hiccup` function.

In most common cases, you should be able to rely solely on Cuphic templates for doing your transformations.

### What about Meander?
After researching various alternatives, I started using [Meander](https://github.com/noprompt/meander) for doing `hiccup->hiccup` data transformations in my other project, [rescope](https://github.com/kuhumcst/rescope).

While Meander is quite capable, its universal DSL didn't _seem_ easier to read or write (to me) than normal Clojure code. The main reason to prefer a declarative DSL is because it makes things clearer. I think the issue is that Meander has to accommodate completely heterogeneous data, so its DSL can't rely on any implicit assumptions about the shape of the data.

Example usage
-------------
TODO!!!

Development prerequisites
-------------------------
_NOTE: this part is only relevant if you're developing the Cuphic library itself._

The development workflow of the project itself is built around the [Clojure CLI](https://clojure.org/reference/deps_and_cli) for managing dependencies and [shadow-cljs](https://github.com/thheller/shadow-cljs) for compiling ClojureScript code and providing a live-reloading development environment.

In this project, the dependency management feature of shadow-cljs is not used directly. Rather, I leverage the built-in support in shadow-cljs for the Clojure CLI/deps.edn to download dependencies and build a classpath.

I personally use IntelliJ with the Cursive plugin which [integrates quite well with the Clojure CLI](https://cursive-ide.com/userguide/deps.html).

### macOS setup
(assuming [homebrew](https://brew.sh/) has already been installed)


I'm not sure which JDK version you need, but anything 8+ is probably fine! I personally just use the latest from AdoptOpenJDK (currently JDK 13):

```
brew cask install adoptopenjdk
```

The following will get you the Clojure CLI and shadow-cljs, along with NodeJS:

```
brew install clojure/tools/clojure
brew install node
npm install -g shadow-cljs
```

Make sure that shadow-cljs can infer and install react-dom:
```
npm init
```

Workflow
--------
Development of the library is done using the live-reloading capabilities of shadow-cljs:

```
shadow-cljs watch app
```

This will start a basic web server at `localhost:6000` serving the `:app` build as specified in the `shadow-cljs.edn` file.

It's possible to execute unit tests while developing by also specifying the `:test` build:

```
shadow-cljs watch app test
```

This will make test output available at `localhost:6100`. It's quite convenient to keep a separate browser tab open just for this. The favicon will be coloured green or red depending on the state of the assertions.

Personally, I use the Clojure CLI integration in Cursive to calculate a classpath and download dependencies. Something like this command is being executed behind the scenes:

```
clj -A:app:test -Spath
```

I have also set up some aliases in my personal [~/.clojure/deps.edn](https://github.com/simongray/dotfiles/blob/master/dot/clojure/deps.edn) file to perform certain common tasks such as listing/updating outdated packages:

```
clj -A:outdated
clj -A:update
```
