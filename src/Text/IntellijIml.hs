module Text.IntellijIml (removeFacet) where

import qualified Text.XML.HaXml        as X
import qualified Text.XML.HaXml.Parse  as XP
import qualified Text.XML.HaXml.Pretty as XPP

import           Data.Maybe            (mapMaybe)

removeFacet :: String -> String -> Either String String
removeFacet fileName fileContent = outputDoc <$>  XP.xmlParse' fileName fileContent

outputDoc :: (Show a) => X.Document a -> String
outputDoc = X.render . XPP.document . removeFacet'

removeFacet' :: (Show a) => X.Document a -> X.Document a
removeFacet' (X.Document p s (X.Elem q a contents) m) = (X.Document
                                                         p
                                                         s
                                                         (X.Elem
                                                          q
                                                          a
                                                          (mapMaybe unrelatedToGwt contents))
                                                         m)

unrelatedToGwt :: (Show a) => X.Content a -> Maybe (X.Content a)
unrelatedToGwt (X.CElem (X.Elem (X.N name) attrs content) x) = if not (any isGWTAttribute attrs)
                                                                  then Just (X.CElem
                                                                             (X.Elem
                                                                              (X.N name)
                                                                              attrs
                                                                              (mapMaybe unrelatedToGwt content))
                                                                             x)
                                                                  else Nothing
unrelatedToGwt x = Just x

isGWTAttribute :: X.Attribute -> Bool
isGWTAttribute (X.N "name", X.AttValue attrs) = Left "GWT" `elem` attrs
isGWTAttribute _                              = False
