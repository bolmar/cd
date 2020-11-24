{-# LANGUAGE FlexibleContexts #-}
module CD where

import qualified System.Environment as Env
import qualified Control.Arrow as Arr
import qualified Control.Monad as Mnd
import qualified Control.Monad.State.Lazy as State
import qualified Data.List as List
import qualified Data.Maybe as Maybe

data Op = Add | Div | Mul | Sub
  deriving (Enum, Eq, Ord)

instance Show Op where
  show Add = "+"
  show Div = "/"
  show Mul = "*"
  show Sub = "-"

op Add = (+)
op Div = (/)
op Mul = (*)
op Sub = (-)

ops = [Add .. ]

evaluate (L v) = fromIntegral v
evaluate (N o t1 t2) = op o (evaluate t1) (evaluate t2)

data T v = L v | N Op (T v) (T v)
  deriving Eq

instance (Show v) => Show (T v) where
  show (L v) = show v
  show (N o t1 t2) = showTerm o t1 t2

showTerm o t1 t2 = showNode (Left o) t1 ++ " " ++ show o ++ " " ++ showNode (Right o) t2

infixr 9 ·
(·) = (.) . (.)

showNode _ (L v) = show v
showNode po (N o t1 t2) = showNode' po o t1 t2

-- avoid redundant parentheses
showNode' (Left  Add) o = showTerm o
showNode' (Right Add) o = showTerm o
showNode' (Left  Sub) o = showTerm o
showNode' (Right Div) (o@Div) = bracket · showTerm o
showNode' (Right Div) (o@Mul) = bracket · showTerm o
showNode' _ (o@Div) = showTerm o
showNode' _ (o@Mul) = showTerm o
showNode' _ o = bracket · showTerm o

bracket :: String -> String
bracket str = '(' : str ++ ")"

{-
insert * d (a - (b + c)) =
  [ (a - (b + c)) * d
  , a - (b + c) * d
  , a - (b + c*d)
  ]
other possible trees are generated through permutating the input
-}
insert o v (L v0) = [N o (L v0) v]
insert o v (N o0 t1 t2) = N o (N o0 t1 t2) v : map (N o0 t1) (insert o v t2)

data Solution delta tree = S delta tree
instance (Show d, Show tr) => Show (Solution d tr) where
  show (S _ tr) = show tr

main = do
  (epsM, args) <- fmap (takeBy $ List.isPrefixOf "e") Env.getArgs
  let eps = Maybe.maybe 3 (read . tail) epsM :: Double
  let (target:vals) = map read args :: [Int]

  cd ((<=), eps) (fromIntegral target) vals

takeBy :: (a -> Bool) -> [a] -> (Maybe a, [a])
takeBy = Arr.first Maybe.listToMaybe · List.partition

cd d target = pretty . filterSolutions . generateSolutions
  where
    generateSolutions = map (eval target) . candidates
    filterSolutions = flip State.runState (d, []) . Mnd.filterM review

pretty ([], ((_, d), ts)) = p $ Mnd.liftM (flip (++) (" : " ++ show d)) $ Maybe.listToMaybe ts
pretty (s:_, _) = p $ Just $ show s ++ " : 0.0"

p = putStrLn . (++) ",cd " . Maybe.fromMaybe "no solutions"

candidates = concatMap trees . filter (not . null) . concatMap List.permutations . List.subsequences

trees (v:vs) = foldr trees' [L v] vs
trees' v = concatMap (\t -> concatMap (\o -> insert o (L v) t) ops)

eval target tree =
  let r = evaluate tree
      d = r - target
  in S d tree

review (S d tree) = do
  ((cmp, epsilon), trees) <- State.get
  let delta = abs d
  if delta == 0 then
    State.put (((<), delta), show tree : trees) >> return True
  else
    if cmp delta epsilon && notElem (show tree) trees then
      State.put (((<), delta), show tree : trees) >> return False
    else
      return False
