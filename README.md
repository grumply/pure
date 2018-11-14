# <a href='https://github.com/grumply/pure'><img src='https://github.com/grumply/pure/blob/ab5517253f689e5f40e3d4c5a074281764046203/assets/logo.svg' height='60'></a>

A haskell web framework.

## Sample

### Hello, World!

```haskell
module Main where

import Pure

main = inject body "Hello, World!"
```

### Counting

```haskell
module Main where

import Pure

data Counter = Counter

instance Pure Counter where
  view = ComponentIO $ \self -> def
    { construct = return 0
    , render = \_ n -> let tick = const . modify_ self . const in
        Div <||>
          [ Button <| OnClick (tick succ) |> [ "Increment" ]
          , text n
          , Button <| OnClick (tick pred) |> [ "Decrement" ]
          ]
    }

main = inject body (View Counter)
```

## About

This repository is a meta-library that re-exports a set of modules to simplify pure development.

Work has been moved to the following repositories:

[pure-platform](https://github.com/grumply/pure-platform) 

[pure-bench](https://github.com/grumply/pure-bench)

[pure-cond](https://github.com/grumply/pure-cond)

[pure-core](https://github.com/grumply/pure-core)

[pure-css](https://github.com/grumply/pure-css)

[pure-default](https://github.com/grumply/pure-default)

[pure-dom](https://github.com/grumply/pure-dom)

[pure-ease](https://github.com/grumply/pure-ease)

[pure-events](https://github.com/grumply/pure-events)

[pure-forms](https://github.com/grumply/pure-forms)

[pure-grid](https://github.com/grumply/pure-grid)

[pure-html](https://github.com/grumply/pure-html)

[pure-json](https://github.com/grumply/pure-json)

[pure-lazyloader](https://github.com/grumply/pure-lazyloader)

[pure-lifted](https://github.com/grumply/pure-lifted)

[pure-limiter](https://github.com/grumply/pure-limiter)

[pure-loader](https://github.com/grumply/pure-loader)

[pure-localstorage](https://github.com/grumply/pure-localstorage)

[pure-modal](https://github.com/grumply/pure-modal)

[pure-paginate](https://github.com/grumply/pure-paginate)

[pure-popup](https://github.com/grumply/pure-popup)

[pure-portal](https://github.com/grumply/pure-portal)

[pure-prop](https://github.com/grumply/pure-prop)

[pure-proxy](https://github.com/grumply/pure-proxy)

[pure-queue](https://github.com/grumply/pure-queue)

[pure-random-pcg](https://github.com/grumply/pure-random-pcg)

[pure-readfile](https://github.com/grumply/pure-readfile)

[pure-render](https://github.com/grumply/pure-render)

[pure-responsive](https://github.com/grumply/pure-responsive)

[pure-router](https://github.com/grumply/pure-router)

[pure-scroll-loader](https://github.com/grumply/pure-scroll-loader)

[pure-search](https://github.com/grumply/pure-search)

[pure-semantic-ui](https://github.com/grumply/pure-semantic-ui)

[pure-server](https://github.com/grumply/pure-server)

[pure-spacetime](https://github.com/grumply/pure-spacetime)

[pure-spinners](https://github.com/grumply/pure-spinners)

[pure-sticky](https://github.com/grumply/pure-sticky)

[pure-styles](https://github.com/grumply/pure-styles)

[pure-svg](https://github.com/grumply/pure-svg)

[pure-tagsoup](https://github.com/grumply/pure-tagsoup)

[pure-template](https://github.com/grumply/pure-template)

[pure-test](https://github.com/grumply/pure-test)

[pure-time](https://github.com/grumply/pure-time)

[pure-timediff-simple](https://github.com/grumply/pure-timediff-simple)

[pure-theme](https://github.com/grumply/pure-theme)

[pure-tlc](https://github.com/grumply/pure-tlc)

[pure-transition](https://github.com/grumply/pure-transition)

[pure-try](https://github.com/grumply/pure-try)

[pure-txt](https://github.com/grumply/pure-txt)

[pure-txt-interpolate](https://github.com/grumply/pure-txt-interpolate)

[pure-txt-search](https://github.com/grumply/pure-txt-search)

[pure-txt-trie](https://github.com/grumply/pure-txt-trie)

[pure-uri](https://github.com/grumply/pure-uri)

[pure-variance](https://github.com/grumply/pure-variance)

[pure-visibility](https://github.com/grumply/pure-visibility)

[pure-websocket](https://github.com/grumply/pure-websocket)

[pure-xml](https://github.com/grumply/pure-xml)

[excelsior](https://github.com/grumply/excelsior)
