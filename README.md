# <a href='https://github.com/grumply/pure'><img src='https://github.com/grumply/pure/blob/ab5517253f689e5f40e3d4c5a074281764046203/assets/logo.svg' height='60'></a>

A haskell web framework.

## Sample

### Hello, World!

```haskell
module Main where

import Pure

main = inject body (string "Hello, World!")
```

### Counting

```haskell
module Main where

import Pure

data Counter = Counter

instance Pure Counter where
  view = ComponentIO $ \self -> let upd = setStatePure_ self . const in def
    { construct = return (0 :: Int)
    , render = \_ n ->
        Div <||>
          [ Button <| OnClick (upd succ) |> [ string "Increment" ]
          , Br
          , text n
          , Br
          , Button <| OnClick (upd pred) |> [ string "Decrement" ]
          ]
    }

main = inject body (View Counter)
```

## About

This repository is a meta-library that re-exports a set of modules to simplify pure development.

Work has been moved to the following repositories:

[pure-platform](https://github.com/grumply/pure-platform) 

[pure-cond](https://github.com/grumply/pure-cond)

[pure-core](https://github.com/grumply/pure-core)

[pure-css](https://github.com/grumply/pure-css)

[pure-default](https://github.com/grumply/pure-default)

[pure-dom](https://github.com/grumply/pure-dom)

[pure-ease](https://github.com/grumply/pure-ease)

[pure-events](https://github.com/grumply/pure-events)

[pure-html](https://github.com/grumply/pure-html)

[pure-json](https://github.com/grumply/pure-json)

[pure-lifted](https://github.com/grumply/pure-lifted)

[pure-limiter](https://github.com/grumply/pure-limiter)

[pure-localstorage](https://github.com/grumply/pure-localstorage)

[pure-queue](https://github.com/grumply/pure-queue)

[pure-render](https://github.com/grumply/pure-render)

[pure-router](https://github.com/grumply/pure-router)

[pure-server](https://github.com/grumply/pure-server)

[pure-styles](https://github.com/grumply/pure-styles)

[pure-svg](https://github.com/grumply/pure-svg)

[pure-tagsoup](https://github.com/grumply/pure-tagsoup)

[pure-time](https://github.com/grumply/pure-time)

[pure-try](https://github.com/grumply/pure-try)

[pure-txt](https://github.com/grumply/pure-txt)

[pure-txt-trie](https://github.com/grumply/pure-txt-trie)

[pure-websocket](https://github.com/grumply/pure-websocket)

[pure-xml](https://github.com/grumply/pure-xml)

[excelsior](https://github.com/grumply/excelsior)
