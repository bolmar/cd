module CDM where

import qualified System.Random as RND
import qualified Control.Arrow as Arr
import qualified Control.Monad as Mnd
import qualified Data.List as List

main = do
  [t] <- rand 1 target :: IO [Int]
  bs  <- rand 2 big
  ss  <- rand 4 small
  pp $ t : rsort (bs ++ ss)
  where
    rand = gen RND.getStdRandom

-- r :: RandomGen g => -> g -> (a, g)
gen :: Monad m => (r -> m a) -> Int -> r -> m [a]
gen g n = sequence . Mnd.liftM (replicate n) g

target :: (Num a, RND.Random a, RND.RandomGen g) => g -> (a, g)
target = RND.randomR (100, 1000)

big = Arr.first (*25) . RND.randomR (1, 4)

small = RND.randomR (1, 10)

rsort = List.sortBy (flip compare)

pp = putStrLn . unwords . map show
