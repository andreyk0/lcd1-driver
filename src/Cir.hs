{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData        #-}

-- | Circular list
module Cir
  ( Cir
  , cirFromList
  , cirElem
  , cirPrev
  , cirNext
  )
where

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import           Import

data Cir a = Focus [a] a [a] deriving (Eq, Show)


-- | Constructs a cirular list from a non-empty list.
cirFromList :: NonEmpty a -> Cir a
cirFromList (a :| as) = Focus [] a as


-- | Current element.
cirElem :: Cir a -> a
cirElem (Focus _ a _) = a


-- | Move to next element.
cirNext :: Cir a -> Cir a

cirNext (Focus pas a []) = Focus [] a' as'
  where (a' :| as') = NEL.reverse $ a :| pas

cirNext (Focus pas a (n : nas)) = Focus (a : pas) n nas


-- | Move to prev element.
cirPrev :: Cir a -> Cir a

cirPrev (Focus [] a nas) = Focus pas' p' []
  where (p' :| pas') = NEL.reverse $ a :| nas

cirPrev (Focus (p : pas) a nas) = Focus pas p (a : nas)
