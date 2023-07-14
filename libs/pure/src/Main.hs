{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Main where

import Pure hiding (rows)
import qualified Data.View as View

import Data.Maybe (fromJust)
import Data.Monoid ((<>))

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

{-

Performance is pretty good (as of 0.7.0.0), scoring ~1.5 on the
js-framework-benchmark (result image included). Pure has been benchmarked
against, and should be competetive with, Angular, React, Vue, and Elm.

A note on the performance of `clear rows`:
  Pure beats vanillajs on the clear rows benchmark because listener cleanup is
  deferred to an idle worker. This was a design choice to avoid long frames.

-}

data Row = Row
  { ident     :: !Int
  , label     :: !Txt
  , selected  :: !Bool
  , select    :: !(View -> View)
  , remove    :: !(View -> View)
  }

data Model = Model
  { seed    :: !Seed
  , rows    :: !(V.Vector Row)
  , lastId  :: !Int
  }

data Msg
  = CreateM !Int
  | AppendM !Int
  | UpdateEveryM !Int
  | ClearM
  | SwapM
  | SelectM !Int
  | RemoveM !Int
  | ConstM

unsafeChoose :: V.Vector Txt -> Generator Txt
unsafeChoose = fmap fromJust . sampleVector

adjectives :: Generator Txt
adjectives = unsafeChoose
  [  "pretty",  "large",  "big",  "small",  "tall",  "short",  "long"
  ,  "handsome",  "plain",  "quaint",  "clean",  "elegant",  "easy",  "angry"
  ,  "crazy",  "helpful",  "mushy",  "odd",  "unsightly",  "adorable"
  ,  "important",  "inexpensive",  "cheap",  "expensive",  "fancy"
  ]

colors :: Generator Txt
colors = unsafeChoose
  [  "red",  "yellow",  "blue",  "green",  "pink",  "brown"
  ,  "purple",  "brown",  "white",  "black",  "orange"
  ]

nouns :: Generator Txt
nouns = unsafeChoose
  [  "table",  "chair",  "house",  "bbq",  "desk",  "car"
  ,  "pony",  "cookie",  "sandwich",  "burger",  "pizza"
  ,  "mouse",  "keyboard"
  ]

createRows :: Int -> Int -> (Msg -> IO ()) -> Generator (V.Vector Row)
createRows n newId update = V.generateM n $ \n -> do
  adjective <- adjectives
  color <- colors
  noun <- nouns
  let i = newId + n
  pure $ Row i (adjective <> " " <> color <> " " <> noun) False (OnClick (const $ update (SelectM i))) (OnClick (const $ update (RemoveM i)))

bang :: Row -> Row
bang row = row { label = label row <> " !!!" }

updateEvery :: Int -> (a -> a) -> V.Vector a -> V.Vector a
updateEvery n f = V.modify (flip update 0)
  where
    update v = go
      where
        go x
          | x >= MV.length v = return ()
          | otherwise = do
            MV.unsafeModify v f x
            go (x + n)

swap :: Int -> Int -> V.Vector a -> V.Vector a
swap i j = V.modify (\v -> MV.unsafeSwap v i j)

selectRow :: Int -> Row -> Row
selectRow i row
  | i == ident row = row { selected = True  }
  | selected row   = row { selected = False }
  | otherwise      = row

data Button = Button
  { bId    :: Txt
  , bLabel :: Txt
  , bEvt   :: IO ()
  }

button :: (Msg -> IO ()) -> (Txt,Txt,Msg) -> View
button f (ident,label,msg) =
  Div <| Class "col-sm-6 smallpad" |>
    [ Pure.Button
      <| Attribute "ref" "text"
       . Class "btn btn-primary btn-block"
       . OnClick (\_ -> f msg)
       . Type "button"
       . Id ident
       |> [ txt label ]
    ]

buttons :: [(Txt,Txt,Msg)]
buttons =
    [ ( "run_small" , "Create 10 rows"       , CreateM 10        )
    , ( "run"       , "Create 1,000 rows"    , CreateM 1000      )
    , ( "runlots"   , "Create 10,000 rows"   , CreateM 10000     )
    , ( "add"       , "Append 1,000 rows"    , AppendM 1000      )
    , ( "update"    , "Update every 10th row", UpdateEveryM 10   )
    , ( "clear"     , "Clear"                , ClearM            )
    , ( "swaprows"  , "Swap Rows"            , SwapM             )
    , ( "const"     , "Const"                , ConstM            )
    ]

buildRow :: Row -> View
buildRow (Row i l s select remove) =
  Tr <| (if s then Class "danger" else id) |>
    [ Td <| Class "col-md-1" |> [ txt i ]
    , Td <| Class "col-md-4" |>
      [ A <| select |> [ txt l ]
      ]
    , Td <| Class "col-md-1" |>
      [ A <| remove |>
        [ Span <| Class "glyphicon glyphicon-remove" . Attribute "aria-hidden" "true"
        ]
      ]
    , Td <| Class "col-md-6"
    ]

buildRows :: V.Vector Row -> [(Int,View)]
buildRows = fmap ((,) <$> ident <*> buildRow) . V.toList

main :: IO ()
main = do
  inject body $ flip Component () $ \self ->
    let
        upd msg = void $ View.modify self $ \_ mdl -> do
          case msg of
            CreateM amount ->
              let (seed',newRows) = generate (createRows amount (lastId mdl) upd) (seed mdl)
              in mdl { seed = seed', rows = newRows, lastId = lastId mdl + amount }

            AppendM amount ->
              let (seed',newRows) = generate (createRows amount (lastId mdl) upd) (seed mdl)
              in mdl { seed = seed', rows = rows mdl <> newRows, lastId = lastId mdl + amount }

            UpdateEveryM amount ->
              mdl { rows = updateEvery 10 bang (rows mdl) }

            ClearM ->
              mdl { rows = V.empty }

            SwapM ->
              mdl { rows = swap 1 998 (rows mdl) }

            RemoveM i ->
              mdl { rows = V.filter ((/= i) . ident) (rows mdl) }

            SelectM i ->
              mdl { rows = fmap (selectRow i) (rows mdl) }

            ConstM ->
              mdl

    in
        def
            { onConstruct = do
                seed <- newSeed
                return (Model seed V.empty 1)
            , render = \_ model ->
                Div <| Id "main" |>
                    [ Div <| Class "container" |>
                        [ Div <| Class "jumbotron" |>
                            [ Div <| Class "row" |>
                                  [ Div <| Class "col-md-6" |> [ H1 <||> [ "pure-v0.7-keyed" ] ]
                                  , Div <| Class "col-md-6" |> (fmap (Main.button upd) buttons)
                                  ]
                            ]
                        , Table <| Class "table table-hover table-striped test-data" |>
                            [ Keyed Tbody <||#> buildRows (rows model) ]
                        , Span <| Class "preloadicon glyphicon glyphicon-remove" . Property "aria-hidden" "true"
                        ]
                    ]
            }
