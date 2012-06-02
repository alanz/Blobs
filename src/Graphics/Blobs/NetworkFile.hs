{-# LANGUAGE UndecidableInstances #-}
module Graphics.Blobs.NetworkFile where

import qualified Graphics.Blobs.Network as N
import Graphics.Blobs.Math
import Graphics.Blobs.Common
--import Graphics.Blobs.Colors
import Graphics.Blobs.Shape
import Graphics.Blobs.InfoKind
import Graphics.Blobs.Palette

import Text.XML.HaXml.Types
import Text.XML.HaXml.Escape
import Text.XML.HaXml.Posn (noPos)
import Text.XML.HaXml.Parse hiding (element)
-- import Text.XML.HaXml.XmlContent as XML
import Text.XML.HaXml.XmlContent.Haskell as XML
import Text.XML.HaXml.Combinators (replaceAttrs)
import Text.XML.HaXml.Verbatim
--import Text.XML.HaXml.TypeMapping (toDTD,toHType)
import Text.PrettyPrint.HughesPJ
import qualified Text.XML.HaXml.Pretty as Pretty
import Char
--import Maybe
import Monad(when)
import List(nub,isPrefixOf)

-- | Print the network data structure to an XML text
toString :: (InfoKind n g, InfoKind e g, XmlContent g) =>
            N.Network g n e -> String
toString network = render . Pretty.document $
    Document (Prolog Nothing [] (Just (toDTD (toHType network))) []) emptyST
             (f (toContents network)) []
  where
    f [CElem e _] = e
    f _ = error "bad"	-- shouldn't happen

-- | Parses a string to the network data structure
--   Returns either an error message (Left) or the network,
--   a list of warnings (Right) and a boolean indicating whether
--   the file was an old Dazzle file
fromString :: (InfoKind n g, InfoKind e g, XmlContent g) =>
              String -> Either String (N.Network g n e, [String], Bool)
fromString xml =
    case xmlParse' "input file" xml of
        Left err -> Left err -- lexical or initial (generic) parse error
        Right (Document _ _ e _) ->
            case runParser parseContents [CElem e noPos] of
                (Left err, _) -> Left err  -- secondary (typeful) parse error
                (Right v, _)  -> Right (v,[],False)

{-
-- non-XML output
toStringShow :: (Show g, Show n, Show e) => Network g n e -> String
toStringShow network =
    show ( getNodeAssocs network
         , getEdgeAssocs network
         , getCanvasSize network
         , getGlobalInfo network
         )

fromStringShow :: (Read g, InfoKind n g, InfoKind e g) =>
                  String -> Either String (Network g n e)
fromStringShow txt =
    case reads txt of
        ((tuple,[]):_) ->
            let (nodeAssocs, edgeAssocs, canvasSize, globalInfo) = tuple
            in Right ( setNodeAssocs nodeAssocs
                     . setEdgeAssocs edgeAssocs
                     . setCanvasSize canvasSize
                     $ Network.empty globalInfo undefined undefined
                     )
        _ -> Left "File is not a Blobs network"
-}

---------------------------------------------------------
-- Internal type isomorphic to (index,value) pairs
-- (but permits instances of classes)
---------------------------------------------------------
data AssocN n = AssocN Int (N.Node n)
deAssocN :: AssocN n -> (Int,N.Node n)
deAssocN (AssocN n v) = (n,v)
data AssocE e = AssocE Int (N.Edge e)
deAssocE :: AssocE e -> (Int,N.Edge e)
deAssocE (AssocE n v) = (n,v)

---------------------------------------------------------
-- Convert our data type to/from an XML tree
---------------------------------------------------------
instance (HTypeable g, HTypeable n, HTypeable e)
         => HTypeable (N.Network g n e) where
    toHType _ = Defined "Network" [] [Constr "Network" [] []]
 -- toHType g = Defined "Network" [] [Constr "Network" []
 --			[ Tagged "Width" [String]
 --			, Tagged "Height" [String]
 --			, toHType (getGlobalInfo g)
 --			, toHType (getPalette g)
 --			, toHType (getNodeAssocs g)
 --			, toHType (getEdgeAssocs g)
 --			]]
instance (InfoKind n g, InfoKind e g, XmlContent g) =>
         XmlContent (N.Network g n e) where
    toContents network =
        [CElem (Elem "Network" []
                   [ simpleString  "Width"     (show width)
                   , simpleString  "Height"    (show height)
                   , makeTag       "Info"      (toContents netInfo)
                   , makeTag       "Palette"   (toContents (N.getPalette network))
                   , makeTag       "Nodes"     (concatMap toContents nodeAssocs)
                   , makeTag       "Edges"     (concatMap toContents edgeAssocs)
                   ]) () ]
      where
        nodeAssocs = map (uncurry AssocN) $ N.getNodeAssocs network
        edgeAssocs = map (uncurry AssocE) $ N.getEdgeAssocs network
        (width, height) = N.getCanvasSize network
        netInfo = N.getGlobalInfo network
    parseContents = do
        { inElement "Network" $ do
              { w  <- inElement "Width"  $ fmap read XML.text
              ; h  <- inElement "Height" $ fmap read XML.text
              ; i  <- inElement "Info"   $ parseContents
              ; p  <- inElement "Palette"$ parseContents
              ; ns <- inElement "Nodes"  $ many1 parseContents
              ; es <- inElement "Edges"  $ many1 parseContents
              ; networkValid ns es
              ; return ( N.setCanvasSize (w,h)
                       . N.setPalette p
                       . N.setNodeAssocs (map deAssocN ns)
                       . N.setEdgeAssocs (map deAssocE es)
                       $ N.empty i undefined undefined)
              }
        }

peekAttributes :: String -> XMLParser [(String,AttValue)]
peekAttributes t =
    do{ (p, e@(Elem _ as _)) <- posnElement [t]
      ; reparse [CElem e p]
      ; return as
      }

instance HTypeable (AssocN n) where
    toHType _ = Defined "Node" [] [Constr "Node" [] []]
instance (InfoKind n g) => XmlContent (AssocN n) where
    toContents (AssocN n node) =
        concatMap (replaceAttrs [("id",'N':show n)]) (toContents node)
    parseContents = do
        { [("id",n)] <- peekAttributes "Node"
        ; n' <- num n
        ; node <- parseContents
        ; return (AssocN n' node)
        }
      where num (AttValue [Left ('N':n)]) = return (read n)
            num (AttValue s) = fail ("Problem reading Node ID: "++verbatim s)

instance HTypeable (AssocE e) where
    toHType _ = Defined "Edge" [] [Constr "Edge" [] []]
instance (InfoKind e g) => XmlContent (AssocE e) where
    toContents (AssocE n edge) =
        concatMap (replaceAttrs [("id",'E':show n)]) (toContents edge)
    parseContents = do
        { [("id",n)] <- peekAttributes "Edge"
        ; n' <- num n
        ; edge <- parseContents
        ; return (AssocE n' edge)
        }
      where num (AttValue [Left ('E':n)]) = return (read n)
            num (AttValue s) = fail ("Problem reading Edge ID: "++verbatim s)

instance HTypeable (N.Node n) where
    toHType _ = Defined "Node" [] [Constr "Node" [] []]
instance (InfoKind n g) => XmlContent (N.Node n) where
    toContents node =
        [ makeTag "Node"
            (toContents (N.getPosition node) ++
            [ escapeString "Name"       (N.getName node)
            , simpleString "LabelAbove" (show (N.getNameAbove node))
            , makeTag      "Shape"      (toContents (N.getShape node))
            , makeTag      "Info"       (toContents (N.getInfo node))
            , makeTag      "Arity"      (toContents (N.getArity node))
            ])
        ]
    parseContents = do
        { inElement "Node" $ do
              { p <- parseContents	-- position
              ; n <- inElement "Name" $ XML.text
              ; a <- inElement "LabelAbove" $ fmap read XML.text
              ; s <- inElement "Shape" $ parseContents
              ; i <- inElement "Info" $ parseContents
              ; r <- (inElement "Arity" $ parseContents)
                       `onFail` (return Nothing)
              ; return (N.constructNode n p a s i r)
              }
        }

instance HTypeable DoublePoint where
    toHType _ = Defined "DoublePoint" [] [Constr "X" [] [], Constr "Y" [] []]
instance XmlContent DoublePoint where
    toContents (DoublePoint x y) =
        [ simpleString "X"          (show x)
        , simpleString "Y"          (show y)
        ]
    parseContents = do
        { x <- inElement "X" $ fmap read XML.text
        ; y <- inElement "Y" $ fmap read XML.text
        ; return (DoublePoint x y)
        }

instance HTypeable (N.Edge e) where
    toHType _ = Defined "Edge" [] [Constr "Edge" [] []]
instance InfoKind e g => XmlContent (N.Edge e) where
    toContents edge =
        [ makeTag "Edge"
            [ simpleString  "From"      (show (N.getEdgeFrom edge))
            , simpleString  "To"        (show (N.getEdgeTo edge))
            , makeTag       "Via"       (concatMap toContents (N.getEdgeVia edge))
            , makeTag       "Info"      (toContents (N.getEdgeInfo edge))
            , makeTag       "FromPort"  (toContents (N.getEdgeFromPort edge))
            , makeTag       "ToPort"    (toContents (N.getEdgeToPort edge))
            ]
        ]
    parseContents = do
        { inElement "Edge" $ do
              { f <- inElement "From" $ fmap read XML.text
              ; t <- inElement "To" $ fmap read XML.text
              ; v <- inElement "Via" $ many parseContents
              ; i <- inElement "Info" $ parseContents
              ; fp <- (inElement "FromPort" $ parseContents)
                          `onFail` (return 0)
              ; tp <- (inElement "ToPort" $ parseContents)
                          `onFail` (return 0)
              ; return (N.constructEdge f fp t tp v i)
              }
        }

{- derived by DrIFT -}
instance HTypeable Colour where
    toHType v = Defined "Colour" []
                   [Constr "RGB" [] [toHType aa,toHType ab,toHType ac]]
      where (RGB aa ab ac) = v
instance XmlContent Colour where
    parseContents = do
        { inElement "RGB" $ do
              { aa <- parseContents
              ; ab <- parseContents
              ; ac <- parseContents
              ; return (RGB aa ab ac)
              }
        }
    toContents v@(RGB aa ab ac) =
        [mkElemC (showConstr 0 (toHType v))
                 (concat [toContents aa, toContents ab, toContents ac])]

{- derived by DrIFT -}
instance HTypeable Shape where
    toHType v = Defined "Shape" []
                    [Constr "Circle" [] [toHType aa,toHType ab]
                    ,Constr "Polygon" [] [toHType ac,toHType ad]
                    ,Constr "Lines" [] [toHType ae,toHType af]
                    ,Constr "Composite" [] [toHType ag]]
      where
        (Circle aa ab) = v
        (Polygon ac ad) = v
        (Lines ae af) = v
        (Composite ag) = v
instance XmlContent Shape where
    parseContents = do
        { e@(Elem t _ _) <- element  ["Circle","Polygon","Lines","Composite"]
        ; case t of
          _ | "Polygon" `isPrefixOf` t -> interior e $
                do { ac <- parseContents
                   ; ad <- parseContents
                   ; return (Polygon ac ad)
                   }
            | "Lines" `isPrefixOf` t -> interior e $
                do { ae <- parseContents
                   ; af <- parseContents
                   ; return (Lines ae af)
                   }
            | "Composite" `isPrefixOf` t -> interior e $
                fmap Composite parseContents
            | "Circle" `isPrefixOf` t -> interior e $
                do { aa <- parseContents
                   ; ab <- parseContents
                   ; return (Circle aa ab)
                   }
        }
    toContents v@(Circle aa ab) =
        [mkElemC (showConstr 0 (toHType v)) (concat [toContents aa,
                                                     toContents ab])]
    toContents v@(Polygon ac ad) =
        [mkElemC (showConstr 1 (toHType v)) (concat [toContents ac,
                                                     toContents ad])]
    toContents v@(Lines ae af) =
        [mkElemC (showConstr 2 (toHType v)) (concat [toContents ae,
                                                     toContents af])]
    toContents v@(Composite ag) =
        [mkElemC (showConstr 3 (toHType v)) (toContents ag)]

{- derived by DrIFT -}
instance HTypeable ShapeStyle where
    toHType v = Defined "ShapeStyle" []
                    [Constr "ShapeStyle" [] [toHType aa,toHType ab,toHType ac]]
      where (ShapeStyle aa ab ac) = v
instance XmlContent ShapeStyle where
    parseContents = do
        { inElement  "ShapeStyle" $ do
              { aa <- parseContents
              ; ab <- parseContents
              ; ac <- parseContents
              ; return (ShapeStyle aa ab ac)
              }
        }
    toContents v@(ShapeStyle aa ab ac) =
        [mkElemC (showConstr 0 (toHType v))
                 (concat [toContents aa, toContents ab, toContents ac])]

{- handwritten -}
instance HTypeable a => HTypeable (Palette a) where
    toHType p = Defined "Palette" [toHType a] [Constr "Palette" [] []]
              where (Palette ((_,(_,Just a)):_)) = p
instance XmlContent a => XmlContent (Palette a) where
    toContents (Palette xs) =
        [ mkElemC "Palette" (concatMap toContents xs) ]
    parseContents = do
        { inElement "Palette" $ fmap Palette (many1 parseContents) }

{-
instance XmlContent a => XmlContent (Either String a) where
  toContents (Left str)    = [ simpleString "ShapeName" (show str) ]
  toContents (Right shape) = (toContents shape)

  parseContents = do
    return () -- Need to implement this
-}
---- UTILITY FUNCTIONS

-- Abbreviations
makeTag :: String -> [Content i] -> Content i
makeTag tagName children = CElem (Elem tagName [] children) undefined

tagWithId :: String -> String -> [Content i] -> Content i
tagWithId tagName identity children =
    CElem (Elem tagName [("id", AttValue [Left identity])] children) undefined

-- | A simple string contains no spaces or unsafe characters
simpleString :: String -> String -> Content i
simpleString tag value =
    CElem (Elem tag [] [ CString False value undefined ]) undefined

-- | The string value may contain spaces and unsafe characters
escapeString :: String -> String -> Content i
escapeString key value =
    CElem ((if isSafe value then id else escape) $
             Elem key [] [ CString (any isSpace value) value undefined ])
          undefined
  where
    isSafe cs = all isSafeChar cs
    isSafeChar c = isAlpha c || isDigit c || c `elem` "- ."

    escape :: Element i -> Element i
    escape = xmlEscape stdXmlEscaper

comment :: String -> Content i
comment s = CMisc (Comment (commentEscape s)) undefined

-- Replace occurences of "-->" with "==>" in a string so that the string
-- becomes safe for an XML comment
commentEscape :: String -> String
commentEscape [] = []
commentEscape ('-':'-':'>':xs) = "==>" ++ commentEscape xs
commentEscape (x:xs) = x : commentEscape xs

---------------------------------------------------------
-- Check whether the network read from file is valid
---------------------------------------------------------

networkValid :: [AssocN n] -> [AssocE e] -> XMLParser ()
networkValid nodeAssocs edgeAssocs
    | containsDuplicates nodeNrs =
        fail "Node numbers should be unique"
    | containsDuplicates edgeNrs =
        fail "Edge numbers should be unique"
    | otherwise =
          do{ mapM_ (checkEdge nodeNrs) edgeAssocs
            ; -- determine whether there are multiple edges between any two nodes
            ; let multipleEdges = duplicatesBy betweenSameNodes edges
            ; when (not (null multipleEdges)) $
                fail $ "There are multiple edges between the following node pairs: " ++
                    commasAnd [ "(" ++ show (N.getEdgeFrom e) ++ ", "
                                    ++ show (N.getEdgeTo e) ++ ")"
                              | e <- multipleEdges
                              ]
            ; return ()
            }
  where
    nodeNrs = map (fst . deAssocN) nodeAssocs
    (edgeNrs, edges) = unzip (map deAssocE edgeAssocs)

-- Check whether edges refer to existing node numbers and whether
-- there are no edges that start and end in the same node
checkEdge :: [N.NodeNr] -> AssocE e -> XMLParser ()
checkEdge nodeNrs (AssocE edgeNr edge)
    | fromNr == toNr =
        fail $ "Edge " ++ show edgeNr ++ ": from-node and to-node are the same"
    | fromNr `notElem` nodeNrs = nonExistingNode fromNr
    | toNr   `notElem` nodeNrs = nonExistingNode toNr
    | otherwise                = return ()
  where
    fromNr = N.getEdgeFrom edge
    toNr   = N.getEdgeTo   edge
    nonExistingNode nodeNr =
        fail $ "Edge " ++ show edgeNr ++ ": refers to non-existing node "
               ++ show nodeNr

containsDuplicates :: Eq a => [a] -> Bool
containsDuplicates xs = length (nub xs) /= length xs

-- Partial equality on edges
betweenSameNodes :: N.Edge e -> N.Edge e -> Bool
betweenSameNodes e1 e2 =
    (N.getEdgeFrom e1 == N.getEdgeFrom e2  &&  N.getEdgeTo e1 == N.getEdgeTo e2)
    ||
    (N.getEdgeFrom e1 == N.getEdgeTo e2    &&  N.getEdgeTo e1 == N.getEdgeFrom e1)

-- Returns elements that appear more than once in a list
duplicates :: Eq a => [a] -> [a]
duplicates [] = []
duplicates (x:xs)
    | x `elem` xs = x : duplicates (filter (/= x) xs)
    | otherwise   = duplicates xs

-- Returns elements that appear more than once in a list, using given Eq op
duplicatesBy :: (a->a->Bool) -> [a] -> [a]
duplicatesBy _  [] = []
duplicatesBy eq (x:xs)
    | any (eq x) xs = x : duplicatesBy eq (filter (not . eq x) xs)
    | otherwise     = duplicatesBy eq xs

