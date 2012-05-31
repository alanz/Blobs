{-# OPTIONS -fglasgow-exts  #-}
{-
  | Module      :  XTC
    Maintainer  :  martijn@cs.uu.nl
    
    eXtended & Typed Controls for wxHaskell
    
    
    TODO: - how to handle duplicates (up to presentation) in item lists
          - check (!!) error that occured in Dazzle
          - implement tSelecting and other events
          - Check: instance selection etc. <Control> () or <Control> a
               - Maybe it should be () to prevent subclassing (which may cause a problem
                 with the client data field
          - Items w String??
          - value of selection when nothing selected? add Maybe?
          - WxObject vs Object?
-}

module XTC ( Labeled( toLabel ),
           , TValued( tValue ),
           , TItems( tItems ),
           , TSelection( tSelection ),
           , TSelections( tSelections ),
           , RadioView, mkRadioView, mkRadioViewEx
           , ListView, mkListView, mkListViewEx
           , MultiListView, mkMultiListView, mkMultiListViewEx
           , ChoiceView, mkChoiceView, mkChoiceViewEx
           , ComboView, mkComboView, mkComboViewEx
           , ValueEntry, mkValueEntry, mkValueEntryEx
           , change -- TODO wx should take care of this
--           , ObservableVar, mkObservableVar  -- temporarily disabled due to name clash
           , xtc -- for testing, exported to avoid a warning in Dazzle
           ) where

import Graphics.UI.WX hiding (window, label)
import qualified Graphics.UI.WX
import Graphics.UI.WXCore hiding (label, Event)
import List
import Maybe

class Labeled x where
  toLabel :: x -> String

instance Labeled String where
  toLabel str = str

class Selection w => TSelection x w | w -> x where
  tSelection :: Attr w x

class Selections w => TSelections x w | w -> x where
  tSelections :: Attr w [x]

class Items w String => TItems x w | w -> x where
  tItems :: Attr w [x]


-- RadioView

data CRadioView x b

type RadioView x b = RadioBox (CRadioView x b)

-- TODO: instance of tItems?
instance Labeled x => TSelection x (RadioView x ()) where
  tSelection
    = newAttr "tSelection" viewGetTSelection viewSetTSelection

mkRadioView :: Labeled x => Window a -> Orientation -> [x] -> [Prop (RadioView x ())] -> IO (RadioView x ())
mkRadioView window orientation viewItems props = 
  mkRadioViewEx window toLabel orientation viewItems props

mkRadioViewEx :: Window a -> (x -> String) -> Orientation -> [x] -> [Prop (RadioView x ())] -> IO (RadioView x ())
mkRadioViewEx window present orientation viewItems props = 
 do { model <- varCreate viewItems 
    ; radioView <- fmap objectCast $ radioBox window orientation (map present viewItems) []
    ; objectSetClientData radioView (return ()) (model, present)
    ; set radioView props
    ; return radioView
    } -- cannot use mkViewEx because items must be set at creation (items is not writeable)

-- ListView

data CListView a b

type ListView a b = SingleListBox (CListView a b)

instance TSelection x (ListView x ()) where
  tSelection = newAttr "tSelection" viewGetTSelection viewSetTSelection

instance TItems x (ListView x ()) where
  tItems = newAttr "tItems" viewGetTItems viewSetTItems

mkListView :: Labeled x => Window a -> [Prop (ListView x ())] -> IO (ListView x ())
mkListView window props = mkListViewEx window toLabel props
  
mkListViewEx :: Window a -> (x -> String) -> [Prop (ListView x ())] -> IO (ListView x ())
mkListViewEx window present props = mkViewEx singleListBox window present props


-- MultiListView

data CMultiListView a b

type MultiListView a b = MultiListBox (CMultiListView a b)

instance Labeled x => TSelections x (MultiListView x ()) where
  tSelections = newAttr "tSelections" multiListViewGetTSelections multiListViewSetTSelections

instance Labeled x => TItems x (MultiListView x ()) where
  tItems = newAttr "tItems" viewGetTItems viewSetTItems

mkMultiListView :: Labeled x => Window a -> [Prop (MultiListView x ())] -> IO (MultiListView x ())
mkMultiListView window props = mkMultiListViewEx window toLabel props

mkMultiListViewEx :: Window a -> (x -> String) -> [Prop (MultiListView x ())] -> IO (MultiListView x ())
mkMultiListViewEx window present props = mkViewEx multiListBox window present props

multiListViewSetTSelections :: MultiListView x () -> [x] -> IO ()
multiListViewSetTSelections (multiListView :: MultiListView x ()) selectionItems =
 do { Just ((model, present) :: (Var [x], x -> String)) <-
        unsafeObjectGetClientData multiListView
    ; viewItems <- get model value
    ; let labels = map present selectionItems
    ; let indices = catMaybes [ findIndex (\it -> present it == label) viewItems
                              | label <- labels ]
    ; set multiListView [ selections := indices ]
    }

multiListViewGetTSelections :: MultiListView x () -> IO [x]
multiListViewGetTSelections multiListView =
 do { Just ((model, _) :: (Var [x], x -> String)) <-
        unsafeObjectGetClientData multiListView
    ; selectedIndices <- get multiListView selections
    ; viewItems <- get model value
    ; return (map (safeIndex "XTC.multiListViewGetTSelections" viewItems)
                    selectedIndices)
    }


-- ChoiceView

data CChoiceView a b

type ChoiceView a b = Choice (CChoiceView a b)

instance Selecting (ChoiceView x ()) where
  select = newEvent "select" choiceGetOnCommand choiceOnCommand
-- Necessary because wxHaskell declares "instance Selecting (Choice ())" instead of
-- "Selecting (Choice a)". TODO: let/make Daan fix this

instance Selection (ChoiceView x ()) where
  selection = newAttr "selection" choiceGetSelection choiceSetSelection
-- Necessary because wxHaskell declares "instance Selection (Choice ())" instead of
-- "Selection (Choice a)".

instance TSelection x (ChoiceView x ()) where
  tSelection = newAttr "tSelection" viewGetTSelection viewSetTSelection

instance TItems x (ChoiceView x ()) where
  tItems = newAttr "tItems" viewGetTItems viewSetTItems

mkChoiceView :: Labeled x => Window a -> [Prop (ChoiceView x ())] -> IO (ChoiceView x ())
mkChoiceView window (props :: [Prop (ChoiceView x ())]) =
  mkViewEx choice window (toLabel :: x -> String) props

mkChoiceViewEx :: Window a -> (x -> String) -> Style -> [Prop (ChoiceView x ())] -> IO (ChoiceView x ())
mkChoiceViewEx window present stl props =
  mkViewEx (\win -> choiceEx win stl) window present props


-- ComboView

data CComboView a b

type ComboView a b = ComboBox (CComboView a b)


instance TSelection x (ComboView x ()) where
  tSelection = newAttr "tSelection" viewGetTSelection viewSetTSelection

instance TItems x (ComboView x ()) where
  tItems = newAttr "tItems" viewGetTItems viewSetTItems

mkComboView :: Labeled x => Window a -> [Prop (ComboView x ())] -> IO (ComboView x ())
mkComboView window (props :: [Prop (ComboView x ())]) =
  mkViewEx comboBox window (toLabel :: x -> String) props

mkComboViewEx :: Window a -> (x -> String) -> Style -> [Prop (ComboView x ())] -> IO (ComboView x ())
mkComboViewEx window present stl props = 
  mkViewEx (\win -> comboBoxEx win stl) window present props



-- generic mk function that puts a model and a present function in the client data
mkViewEx :: (parent -> [p] -> IO (Object a)) -> parent -> (x -> String) -> [Prop (WxObject b)] ->
            IO (WxObject b)
mkViewEx mkView window present props =
 do { model <- varCreate []
    ; view <- fmap objectCast $ mkView window []
    ; objectSetClientData view (return ()) (model, present)
    ; set view props
    ; return view
    }

-- generic set/getTSelection for RadioView, ListView, and ChoiceView

viewGetTSelection :: TSelection x (WxObject a) => WxObject a -> IO x
viewGetTSelection view =
 do { Just ((model, _) :: (Var [x], x -> String)) <-
        unsafeObjectGetClientData view
    ; selectedIndex <- get view selection
    ; viewItems <- get model value
    ; return (safeIndex "XTC.viewGetTSelection" viewItems selectedIndex)
    }

-- if non unique, set to first viewItem with same label
-- selection is set to 0 if object is not found, maybe -1 is better?
viewSetTSelection :: TSelection x (WxObject a) => WxObject a -> x -> IO ()
viewSetTSelection view selectionItem =
 do { Just ((model, present) :: (Var [x], x -> String)) <-
        unsafeObjectGetClientData view
    ; viewItems <- get model value
    ; let label = present selectionItem
    ; let index = findLabelIndex present label viewItems
    ; set view [ selection := index ]
    }
 where findLabelIndex :: (x -> String) -> String -> [x] -> Int
       findLabelIndex present label theItems =
         case findIndex (\it -> present it == label) theItems of
           Just ix -> ix
           Nothing -> 0

viewGetTItems :: TItems x (WxObject a) => WxObject a -> IO [x]
viewGetTItems view =
 do { Just ((model, _) :: (Var [x], x -> String)) <-
        unsafeObjectGetClientData view
    ; viewItems <- get model value
    ; return viewItems
    }

viewSetTItems :: TItems x (WxObject a) => WxObject a -> [x] -> IO ()
viewSetTItems view viewItems =
 do { Just ((model, present) :: (Var [x], x -> String)) <-
        unsafeObjectGetClientData view
    ; set model [ value := viewItems ]
    ; set view [ items := map present viewItems ]
    }






-- ValueEntry

class Parseable x where
  parse :: String -> Maybe x

instance Parseable String where
  parse = Just

{- When a type is instance of Read, a simple Parseable instance can be declared with readParse
   e.g. for Int:  instance Parseable Int where parse = readParse

TODO: can we make this some kind of default?
-}
readParse :: Read x => String -> Maybe x 
readParse str = case reads str of
                  [(x, "")] -> Just x
                  _         -> Nothing

class TValued  x w | w -> x where
  tValue :: Attr w (Maybe x)

data CValueEntry x b

type ValueEntry x b = TextCtrl (CValueEntry x b)

instance TValued x (ValueEntry x ()) where
  tValue
    = newAttr "tValue" valueEntryGetTValue valueEntrySetTValue

mkValueEntry :: (Show x, Read x) => Window b -> [ Prop (ValueEntry x ()) ] -> IO (ValueEntry x ())
mkValueEntry window props = mkValueEntryEx window show readParse props
                  
mkValueEntryEx :: Window b -> (x -> String) -> (String -> Maybe x) -> [ Prop (ValueEntry x ()) ] -> IO (ValueEntry x ())
mkValueEntryEx window present parse props =
 do { valueEntry <- fmap objectCast $ textEntry window []
    ; objectSetClientData valueEntry (return ()) (present, parse) 
    ; set valueEntry $ props ++ [ on change := validate valueEntry ]
                                          
    ; return valueEntry
    }
 where validate :: ValueEntry x () -> IO ()
       validate valueEntry =
        do { mVal <- get valueEntry tValue
           ; set valueEntry [ bgcolor := case mVal of -- TODO: add property for error color?
                                           Nothing -> lightgrey
                                           _       -> white
                            ]
           ; repaint valueEntry
           } -- drawing a squiggly doesn't work because font metrics are not available

valueEntryGetTValue :: ValueEntry x () -> IO (Maybe x)
valueEntryGetTValue valueEntry =
 do { Just ((_, parse) :: (x -> String, String -> Maybe x)) <- unsafeObjectGetClientData valueEntry
    ; valueStr <- get valueEntry text
    ; return $ parse valueStr
    }

valueEntrySetTValue :: ValueEntry x () -> Maybe x -> IO ()
valueEntrySetTValue valueEntry mValue =
 do { Just ((present, _) :: (x -> String, String -> Maybe x)) <- unsafeObjectGetClientData valueEntry
    ; case mValue of
        Nothing    -> return ()
        Just theValue -> set valueEntry [ text := present theValue ]
    }


class Observable w where
  change :: Event w (IO ())
  
instance Observable (TextCtrl a) where
  change = newEvent "change" (controlGetOnText) (controlOnText)



-- ObservableVar

-- add variable as WxObject
{-
type Observer x = (WxObject (), x -> IO ())

data ObservableVar x = ObservableVar (Var [Observer x]) (Var x)

instance Valued ObservableVar where
  value
    = newAttr "value" observableVarGetValue observableVarSetValue

mkObservableVar :: x -> IO (ObservableVar x)
mkObservableVar x =
 do { observersV <- variable [ value := [] ]
    ; var        <- variable [ value := x ]
    ; return $ ObservableVar observersV var
    }
    
observableVarGetValue :: ObservableVar x -> IO x
observableVarGetValue (ObservableVar _ var) = get var value

observableVarSetValue :: ObservableVar x -> x -> IO ()
observableVarSetValue (ObservableVar observersV var) x =
 do { myObservers <- get observersV value
    ; set var [ value := x ]
    ; sequence_ [ obs x | (_, obs) <- myObservers ]
    }

class Observable  x w | w -> x where
  observers :: Attr w [Observer x]

instance Observable x (ObservableVar x) where
  observers
    = newAttr "observers" observableVarGetObservers observableVarSetObservers

observableVarGetObservers :: ObservableVar x -> IO [Observer x]
observableVarGetObservers (ObservableVar observersV _) = get observersV value 

observableVarSetObservers :: ObservableVar x -> [Observer x] -> IO ()
observableVarSetObservers (ObservableVar observersV var) myObservers = -- return ()
 do { set observersV [ value := myObservers ]
    ; x <- get var value
    ; sequence_ [ obs x | (_, obs) <- myObservers ]
    }


-- all WxObjects get the event 'change'

class Observing w where
  change :: ObservableVar x -> Event w (x -> IO ())
  
instance Observing (WxObject a) where
  change observableVar
    = newEvent "change" (getOnObserve observableVar) (setOnObserve observableVar)

setOnObserve :: ObservableVar x -> Object a -> (x -> IO ()) -> IO ()
setOnObserve (ObservableVar observersV var) obj observer = 
 do { oldObservers <- get observersV value
    ; let otherObservers = filter ((/= objectCast obj) . fst) oldObservers
    ; set observersV [ value := (objectCast obj, observer) : otherObservers ]
    ; x <- get var value
    ; observer x
    }

getOnObserve :: ObservableVar x -> Object a -> IO (x -> IO ())
getOnObserve  (ObservableVar observersV _) obj =
 do { myObservers <- get observersV value
    ; case lookup (objectCast obj) myObservers of
        Just obs -> return obs
        Nothing  -> do { internalError "XTC" "getOnObserve" "object is not an observer" 
                       ; return $ \_ -> return ()
                       }
    }    
-}


-- Utility functions

safeIndex :: String -> [a] -> Int -> a
safeIndex msg xs i
    | i >= 0 && i < length xs = xs !! i
    | otherwise = internalError "XTC" "safeIndex" msg

internalError :: String -> String -> String -> a
internalError moduleName functionName errorString =
    error (moduleName ++ "." ++ functionName ++ ": " ++ errorString)


-- Test function

xtc :: IO ()
xtc = start $
 do { -- counterV <- mkObservableVar 1
    ; f <- frame []
    
    
    ; listV <- mkListView f [ tItems := ["sdfsdf", "fdssd"]
                               , enabled := True
                               ]
    
    ; choiceV <- mkChoiceView f [ tItems := ["sdfsdf", "fdssd"]
                               , enabled := True
                               ]
    ; comboV <- mkComboView f [ tItems := ["sdfsdf", "fdssd"]
                               , enabled := True
                               ]
    ; t <- textEntry f []
    ; ve <- mkValueEntry f [ tValue := Just True ]
  --  ; set t [ on (change counterV) := \i -> set t [ text := show i ] ] 
    
    ; bUp   <- button f [ text := "increase", on command := do { s1 <- get comboV tSelection
                                                               ; s2 <- get listV text
                                                               ; print (s1,s2)
                                                               } ] -- set counterV [ value :~ (+1) ] ]
  --  ; bDown <- button f [ text := "decrease", on command := set counterV [ value :~ (+ (-1::Int)) ] ]
    
 --   ; bChangeHandler <- button f [ text := "change handler"
 --                                , on command := set t [ on (change counterV) := \i -> set t [text := "<<"++show i++">>"] ]]
    ; set f [ layout := column 5 [ row 5 [ Graphics.UI.WX.label "Counter value:", widget t ]
   --                                      , hfloatCenter $ row 5 [ widget bUp, widget bDown ] 
   --                                      , hfloatCenter $ widget bChangeHandler
                                         , widget listV
                                         , widget choiceV
                                         , widget comboV
                                         , widget ve
                                         ]
                                 ]
    
    }
