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

showTerm o t1 t2 = showNode o t1 ++ " " ++ show o ++ " " ++ showNode o t2

infixr 9 ·
(·) = (.) . (.)

showNode _ (L v) = show v
showNode po (N o t1 t2) = showNode' po o t1 t2

-- avoid some redundant parentheses
showNode' Add o = showTerm o
showNode' Sub (o@Div) = showTerm o
showNode' Div (o@Mul) = bracket · showTerm o
showNode' _ (o@Mul) = showTerm o
showNode' _ o = bracket · showTerm o

bracket str = "(" ++ str ++ ")"

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

data Solution result delta tree = S result delta tree
instance (Show r, Show d, Show tr) => Show (Solution r d tr) where
  show (S r d tr) = show r ++ "(" ++ show d ++ "): ,cd " ++ show tr

main = do
  (epsM, args) <- return . takeBy (List.isPrefixOf "e") =<< Env.getArgs
  let eps = Maybe.maybe 3 (read . tail) epsM :: Double
  let (target:vals) = map read args :: [Int]

  mapM_ print $ cd eps (fromIntegral target) vals

takeBy :: (a -> Bool) -> [a] -> (Maybe a, [a])
takeBy = Arr.first Maybe.listToMaybe · List.partition

cd d target = flip State.evalState d . Mnd.filterM review . map (eval target) . candidates

candidates = concatMap trees . filter (not . null) . concatMap List.subsequences . List.permutations

trees (v:vs) = foldr trees' [L v] vs
trees' v = concatMap (\t -> concatMap (\o -> insert o (L v) t) ops)

eval target tree =
  let r = evaluate tree
      d = target - r
  in S r d tree

review (S r d tree) = do
  epsilon <- State.get
  let delta = abs d
  if delta <= epsilon then
    State.put delta >> return True
  else
    return False
