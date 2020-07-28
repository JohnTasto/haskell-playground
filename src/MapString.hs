-- From Generalizing Generalized Tries by Ralf Hinze
-- http://www.cs.ox.ac.uk/ralf.hinze/publications/index.html#J4

module MapString where

value :: Maybe v -> v
value Nothing  = error "not found"
value (Just v) = v

type MapChar v = [(Char, v)]

lookupChar :: Char -> MapChar v -> v
lookupChar _ []            = error "not found"
lookupChar c ((c', v) : x) = if c == c' then v else lookupChar c x

-- data String = [] | (Char : String)
data MapString v = TrieString (Maybe v) (MapChar (MapString v))
-- or ...tring v = TrieString (Maybe v) (MapString (MapChar v))

lookupString  :: String -> MapString v -> v
lookupString []    (TrieString  tn _) = value tn
lookupString (c:s) (TrieString _ tc) = (lookupString s . lookupChar c) tc
-- or ...ing (c:s) (TrieString _ tc) = (lookupChar c . lookupString s) tc

data BST = Leaf String | Node BST Char BST
data MapBST v = TrieBST (MapString v) (MapBST (MapChar (MapBST v)))

lookupBST :: BST -> MapBST v -> v
lookupBST (Leaf s)     (TrieBST tl _) = lookupString s tl
lookupBST (Node l c r) (TrieBST _ tn) = (lookupBST r . lookupChar c . lookupBST l) tn
