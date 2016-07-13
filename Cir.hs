
-- | Circular list
module Cir (
  Cir
, cirFromList
, cirElem
, cirPrev
, cirNext
) where


data Cir a = Focus [a] a [a] deriving (Eq, Show)


-- | Constructs a cirular list from a non-empty list.
cirFromList:: [a]
           -> Cir a
cirFromList as = Focus [] (head as) (tail as)


-- | Current element.
cirElem:: Cir a
       -> a
cirElem (Focus _ a _) = a


-- | Move to next element.
cirNext:: Cir a
       -> Cir a

cirNext (Focus pas a []) = Focus [] a' as'
  where (a':as') = reverse $ a:pas

cirNext (Focus pas a (n:nas)) = Focus (a:pas) n nas


-- | Move to prev element.
cirPrev:: Cir a
       -> Cir a

cirPrev (Focus [] a nas) = Focus pas' p' []
  where (p':pas') = reverse $ a:nas

cirPrev (Focus (p:pas) a nas) = Focus pas p (a:nas)
