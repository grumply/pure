{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
module Pure.Lifted where

import Pure.Data
import Pure.Data.JSV

import Data.Int

#ifdef __GHCJS__
import GHCJS.Types hiding (isNull)
import GHCJS.Marshal.Pure
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "$r = document" document :: Doc

foreign import javascript unsafe
  "$r = window" window :: Win

foreign import javascript unsafe
  "$r = window.history" history :: History

-- note this is only for objects
foreign import javascript unsafe
  "$r = $1 === $2" reference_equality_js :: JSV -> JSV -> Bool

foreign import javascript unsafe
  "$1.appendChild($2)" append_child_js :: Node -> Node -> IO ()

foreign import javascript unsafe
  "$1.insertBefore($2,$3)" insert_before_js :: Element -> Node -> Node -> IO ()

foreign import javascript unsafe
  "$1.insertBefore($2, $1.children[$3]);" insert_at_js :: Element -> Node -> Int -> IO ()

foreign import javascript unsafe
  "$1.innerHTML = $2" set_inner_html_js :: Element -> Txt -> IO ()

foreign import javascript unsafe
  "$1.parentNode.replaceChild($2,$1)" replace_nodes_js :: Node -> Node -> IO ()

foreign import javascript unsafe
  "$1.textContent=$2" replace_text_js :: Text -> Txt -> IO ()

foreign import javascript unsafe
  "$1.innerHTML = ''" clear_node_js :: Node -> IO ()

foreign import javascript unsafe
  "$1[$2] = $3" set_property_js :: Element -> Txt -> Txt -> IO ()

foreign import javascript unsafe
  "$1.removeAttribute($2)" remove_attribute_js :: Element -> Txt -> IO ()

foreign import javascript unsafe
  "$1.removeAttributeNS($2,$3)" remove_attribute_ns_js :: Element -> Txt -> Txt -> IO ()

foreign import javascript unsafe
  "$1.style[$2] = null" remove_style_js :: Element -> Txt -> IO ()

foreign import javascript unsafe
  "$1[$2] = null" remove_property_js :: Element -> Txt -> IO ()

foreign import javascript unsafe
  "document.createElement($1)" create_element_js :: Txt -> IO Element

foreign import javascript unsafe
  "document.createElementNS($1,$2)" create_element_ns_js :: Txt -> Txt -> IO Element

foreign import javascript unsafe
  "document.createTextNode($1)" create_text_js :: Txt -> IO Text

foreign import javascript unsafe
  "document.createDocumentFragment()" create_frag_js :: IO Frag

foreign import javascript unsafe
  "document.getElementById($1)" get_element_by_id_js :: Txt -> IO Element

foreign import javascript unsafe
  "$r = $1 == null" is_null_js :: JSV -> Bool

foreign import javascript unsafe
  "document.getElementsByTagName($1)[0]" get_first_element_by_tag_name_js :: Txt -> IO Element

foreign import javascript unsafe
  "$1.parentNode.removeChild($1)" remove_js :: Node -> IO ()

-- A description of childNodes vs children:
-- https://stackoverflow.com/questions/7935689/what-is-the-difference-between-children-and-childnodes-in-javascript
foreign import javascript unsafe
  "$1.childNodes[$2]" get_child_js :: Node -> Int -> IO Node

foreign import javascript unsafe
  "$1.textContent" text_content_js :: Node -> IO Txt

foreign import javascript unsafe
  "$1.setAttribute($2,$3)" set_attribute_js :: Element -> Txt -> Txt -> IO ()

foreign import javascript unsafe
  "$1.style[$2] = $3" set_style_js :: Element -> Txt -> Txt -> IO ()

foreign import javascript unsafe
  "$1.preventDefault()" prevent_default_js :: JSV -> IO ()

foreign import javascript unsafe
  "$1.stopPropagation()" stop_propagation_js :: JSV -> IO ()

foreign import javascript unsafe
  "$1.addEventListener($2,$3,{passive:$4})" add_event_listener_js :: JSV -> Txt -> CB (JSV -> IO ()) -> Bool -> IO ()

foreign import javascript unsafe
  "$1.removeEventListener($2,$3)" remove_event_listener_js :: JSV -> Txt -> CB (JSV -> IO ()) -> IO ()

foreign import javascript unsafe
  "window.requestAnimationFrame($1)" request_animation_frame_js :: CB (JSV -> IO ()) -> IO Int

foreign import javascript unsafe
  "window.cancelAnimationFrame($1)" cancel_animation_frame_js :: Int -> IO ()

foreign import javascript unsafe
  "history.pushState({},'',$1)" push_state_js :: Txt -> IO ()

foreign import javascript unsafe
  "$1.setAttributeNS($2,$3,$4)" set_attribute_ns_js :: Element -> Txt -> Txt -> Txt -> IO ()

foreign import javascript unsafe
  "dispatchEvent(new PopStateEvent('popstate',{}))" pop_state_js :: IO ()

foreign import javascript unsafe
  "window.scrollTo(0,0)" scroll_to_top_js :: IO ()

foreign import javascript unsafe
  "$1.click()" click_js :: Node -> IO ()

foreign import javascript unsafe
  "$1.blur()" blur_js :: Node -> IO ()

foreign import javascript unsafe
  "$1.focus()" focus_js :: Node -> IO ()

foreign import javascript unsafe
  "while ($1.firstChild) $1.removeChild($1.firstChild)" clear_js :: Node -> IO ()

foreign import javascript unsafe
  "$r = location.pathname" pathname_js :: IO Txt

foreign import javascript unsafe
  "$r = location.search" search_js :: IO Txt

{-# INLINE create #-}
create :: Txt -> IO Element
create tag = create_element_js tag

{-# INLINE createNS #-}
createNS :: Txt -> Txt -> IO Element
createNS ns tag = create_element_ns_js ns tag

{-# INLINE createText #-}
createText :: Txt -> IO Text
createText txt = create_text_js txt

{-# INLINE createFrag #-}
createFrag :: IO Frag
createFrag = create_frag_js

{-# INLINE append #-}
append :: (IsNode child) => Node -> child -> IO ()
append !parent (toNode -> !child) = append_child_js parent child

{-# INLINE insertBefore #-}
insertBefore :: Element -> Node -> Node -> IO ()
insertBefore !parent !new !ref = insert_before_js parent new ref

{-# INLINE insertAt #-}
insertAt :: Element -> Node -> Int -> IO ()
insertAt !parent !new !idx = insert_at_js parent new idx

{-# INLINE setInnerHTML #-}
setInnerHTML :: Element -> Txt -> IO ()
setInnerHTML e t = set_inner_html_js e t

{-# INLINE replaceNodes #-}
replaceNodes :: Node -> Node -> IO ()
replaceNodes !n1 !n2 = replace_nodes_js n1 n2

{-# INLINE replaceText #-}
replaceText :: Text -> Txt -> IO ()
replaceText !n !t = replace_text_js n t

findByTag :: Txt -> IO (Maybe Element)
findByTag tag = do
  e <- get_first_element_by_tag_name_js tag
  if isNull e
    then return Nothing
    else return (Just e)

findById :: Txt -> IO (Maybe Element)
findById i = do
  e <- get_element_by_id_js i
  if isNull e
    then return Nothing
    else return (Just e)

removeNode :: IsNode n => n -> IO ()
removeNode (toNode -> n) = remove_js n

same :: (IsJSV j1, IsJSV j2) => j1 -> j2 -> Bool
same (toJSV -> j1) (toJSV -> j2) = reference_equality_js j1 j2

isNull :: (IsJSV j) => j -> Bool
isNull (toJSV -> j) = is_null_js j

getChild :: IsNode n => n -> Int -> IO (Maybe Node)
getChild (toNode -> n) i = do
  mn <- get_child_js n i
  if isNull mn
    then return Nothing
    else return (Just mn)

textContent :: IsNode n => n -> IO Txt
textContent (toNode -> n) = text_content_js n

{-# INLINE setAttribute #-}
setAttribute :: Element -> Txt -> Txt -> IO ()
setAttribute e k v = set_attribute_js e k v

{-# INLINE setProperty #-}
setProperty :: Element -> Txt -> Txt -> IO ()
setProperty e k v = set_property_js e k v

{-# INLINE setStyle #-}
setStyle :: Element -> Txt -> Txt -> IO ()
setStyle e k v = set_style_js e k v

{-# INLINE removeAttribute #-}
removeAttribute :: Element -> Txt -> IO ()
removeAttribute e k = remove_attribute_js e k

{-# INLINE removeAttributeNS #-}
removeAttributeNS :: Element -> Txt -> Txt -> IO ()
removeAttributeNS e k v = remove_attribute_ns_js e k v

{-# INLINE removeProperty #-}
removeProperty :: Element -> Txt -> IO ()
removeProperty e p = remove_property_js e p

{-# INLINE removeStyle #-}
removeStyle :: Element -> Txt -> IO ()
removeStyle e s = remove_style_js e s

preventDefault :: JSV -> IO ()
preventDefault e = prevent_default_js e

stopPropagation :: JSV -> IO ()
stopPropagation e = stop_propagation_js e

releaseCB :: CB a -> IO ()
releaseCB cb = releaseCallback cb

addEventListener :: IsJSV target => target -> Txt -> CB (JSV -> IO ()) -> Bool -> IO ()
addEventListener (toJSV -> target) e cb p = add_event_listener_js target e cb p

cancelAnimationFrame :: Int -> IO ()
cancelAnimationFrame af = cancel_animation_frame_js af

removeEventListener :: IsJSV target => target -> Txt -> CB (JSV -> IO ()) -> IO ()
removeEventListener (toJSV -> target) e cb = remove_event_listener_js target e cb

requestAnimationFrame :: CB (JSV -> IO ()) -> IO Int
requestAnimationFrame cb = request_animation_frame_js cb

getWindow :: IO Win
getWindow = return window

getDocument :: IO Doc
getDocument = return document

getHistory :: IO History
getHistory = return history

pushState :: Txt -> IO ()
pushState h = push_state_js h

setAttributeNS :: Element -> Txt -> Txt -> Txt -> IO ()
setAttributeNS e ns k v = set_attribute_ns_js e ns k v

popState :: IO ()
popState = pop_state_js

scrollToTop :: IO ()
scrollToTop = scroll_to_top_js

clickNode :: Node -> IO ()
clickNode n = click_js n

blurNode :: Node -> IO ()
blurNode n = blur_js n

focusNode :: Node -> IO ()
focusNode n = focus_js n

clear :: Node -> IO ()
clear n = clear_js n

getPathname :: IO Txt
getPathname = pathname_js

getSearch :: IO Txt
getSearch = search_js
#else

----------------------------------------
-- GHC
--
-- We can get away with mostly noops for
-- diffing, creation, append, etc....
--
-- Not sure which noops will be needed,
-- though, so I'm implementing them all
-- just to be safe.

create :: Txt -> IO Element
create _ = return (Element ())

createNS :: Txt -> Txt -> IO Element
createNS _ _ = return (Element ())

createText :: Txt -> IO Text
createText _ = return (Text ())

createFrag :: IO Frag
createFrag = return (Frag ())

append :: (IsNode child) => Node -> child -> IO ()
append _ _ = return ()

insertBefore :: Element -> Node -> Node -> IO ()
insertBefore _ _ _ = return ()

insertAt :: Element -> Node -> Int -> IO ()
insertAt _ _ _ = return ()

setInnerHTML :: Element -> Txt -> IO ()
setInnerHTML _ _ = return ()

replaceNodes :: Node -> Node -> IO ()
replaceNodes _ _ = return ()

replaceText :: Text -> Txt -> IO ()
replaceText _ _ = return ()

findByTag :: Txt -> IO (Maybe Element)
findByTag _ = return $ Just $ Element ()

findById :: Txt -> IO (Maybe Element)
findById _ = return $ Just $ Element ()

removeNode :: IsNode n => n -> IO ()
removeNode _ = return ()

same :: (IsJSV j1, IsJSV j2) => j1 -> j2 -> Bool
same _ _ = True

isNull :: (IsJSV j) => j -> Bool
isNull _ = False

getChild :: IsNode n => n -> Int -> IO (Maybe Node)
getChild _ _ = return (Just $ Node ())

setAttribute :: Element -> Txt -> Txt -> IO ()
setAttribute _ _ _ = return ()

setProperty :: Element -> Txt -> Txt -> IO ()
setProperty _ _ _ = return ()

setStyle :: Element -> Txt -> Txt -> IO ()
setStyle _ _ _ = return ()

removeAttribute :: Element -> Txt -> IO ()
removeAttribute _ _ = return ()

removeAttributeNS :: Element -> Txt -> Txt -> IO ()
removeAttributeNS _ _ _ = return ()

removeProperty :: Element -> Txt -> IO ()
removeProperty _ _ = return ()

removeStyle :: Element -> Txt -> IO ()
removeStyle _ _ = return ()

preventDefault :: JSV -> IO ()
preventDefault _ = return ()

stopPropagation :: JSV -> IO ()
stopPropagation _ = return ()

releaseCB :: CB a -> IO ()
releaseCB _ = return ()

addEventListener :: IsJSV target => target -> Txt -> CB (JSV -> IO ()) -> Bool -> IO ()
addEventListener _ _ _ _ = return ()

removeEventListener :: IsJSV target => target -> Txt -> CB (JSV -> IO ()) -> IO ()
removeEventListener _ _ _ = return ()

requestAnimationFrame :: CB (JSV -> IO ()) -> IO Int64
requestAnimationFrame _ = return 0

cancelAnimationFrame :: Int64 -> IO ()
cancelAnimationFrame _ = return ()

getWindow :: IO Win
getWindow = return $ Win ()

getDocument :: IO Doc
getDocument = return $ Doc ()

getHistory :: IO History
getHistory = return $ History ()

pushState :: Txt -> IO ()
pushState _ = return ()

popState :: IO ()
popState = return ()

setAttributeNS :: Element -> Txt -> Txt -> Txt -> IO ()
setAttributeNS _ _ _ _ = return ()

scrollToTop :: IO ()
scrollToTop = return ()

clickNode :: Node -> IO ()
clickNode _ = return ()

blurNode :: Node -> IO ()
blurNode _ = return ()

focusNode :: Node -> IO ()
focusNode _ = return ()

clear :: Node -> IO ()
clear _ = return ()

getPathname :: IO Txt
getPathname = return ""

getSearch :: IO Txt
getSearch = return ""
#endif

