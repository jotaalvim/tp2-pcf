{-# LANGUAGE FlexibleInstances #-}
module Adventurers where

import DurationMonad
import Data.List
import Control.Monad

-- The list of adventurers
data Adventurer = P1 | P2 | P5 | P10 deriving (Show,Eq)
-- Adventurers + the lantern
type Objects = Either Adventurer ()

-- The time that each adventurer needs to cross the bridge
getTimeAdv :: Adventurer -> Int
getTimeAdv P1 = 1
getTimeAdv P2 = 2
getTimeAdv P5 = 5
getTimeAdv P10 = 10

{-- The state of the game, i.e. the current position of each adventurer
+ the lantern. The function (const False) represents the initial state of the
game, with all adventurers and the lantern on the left side of the bridge.
Similarly, the function (const True) represents the end state of the game, with
all adventurers and the lantern on the right side of the bridge.
--}

type State = Objects -> Bool

instance Show State where
  show s = (show . (fmap show)) [s (Left P1),
                                 s (Left P2),
                                 s (Left P5),
                                 s (Left P10),
                                 s (Right ())]

instance Eq State where
  (==) s1 s2 = and [s1 (Left P1) == s2 (Left P1),
                    s1 (Left P2) == s2 (Left P2),
                    s1 (Left P5) == s2 (Left P5),
                    s1 (Left P10) == s2 (Left P10),
                    s1 (Right ()) == s2 (Right ())]

-- The initial state of the game
gInit :: State
gInit = const False

-- Changes the state of the game for a given object
changeState :: Objects -> State -> State
changeState a s = let v = s a in (\x -> if x == a then not v else s x)

-- Changes the state of the game for a list of objects 
mChangeState :: [Objects] -> State -> State
mChangeState os s = foldr changeState s os
                               
{-- For a given state of the game, the function presents all the
possible moves that the adventurers can make.  --}

allValidPlays :: State -> ListDur State
allValidPlays s = manyChoice $ map ( validPlay s . (right:) . fromPair ) pairs
    where
        pairs = makePairs $ filterL s 

allValidPlays2 :: State -> ListDur State
allValidPlays2 s = manyChoice $ muda1 ++ muda2
    where
    -- muda 1 pessoa
        muda1 = map ( validPlay s . (right:) . return   ) $ filterL s
    -- muda 2 pessoas
        muda2 = map ( validPlay s . (right:) . fromPair ) $ makePairs $ filterL s

validPlay :: State -> [Objects] -> ListDur State
validPlay s l = LD $ return $ wait (maxT l ) $ return $ mChangeState l s 

maxT = foldr ( max . either getTimeAdv zero) 0

-- blackbird combinator
(...) = (.).(.)

-- função que alcula mas não coloca tempo na duração
validPlay2 :: State -> [Objects] -> ListDur State
validPlay2  = return ... flip mChangeState


-- filtra o pessoal que está com a lanterna num estado
filterL :: State -> [Objects]
filterL s =  filter ((== s right) . s) $ Left <$> [P1, P2, P5, P10]


{-- For a given number n and initial state, the function calculates
all possible n-sequences of moves that the adventures can make --}
exec :: Int -> State -> ListDur State
--exec = undefined
exec 0 s = allValidPlays2 s
exec n s = do
    moves <- allValidPlays2 s
    rest <- exec (n - 1) moves
    return rest


allT s = all s [Left P1, Left P2, Left P5, Left P10]
{-- Is it possible for all adventurers to be on the other side
in <=17 min and not exceeding 5 moves ? --}
-- To implement
--leq17 :: Bool
leq17 = filter (\ (Duration (a,b)) -> allT b ) $ remLD $ exec 10 gInit

{-- Is it possible for all adventurers to be on the other side
in < 17 min ? --}
-- To implement
l17 :: Bool
l17 = undefined

-------------------------------------------------------------------------------
{-- Implementation of the monad used for the problem of the adventurers.
Recall the Knight's quest --}

data ListDur a = LD [Duration a] deriving Show

remLD :: ListDur a -> [Duration a]
remLD (LD x) = x

instance Functor ListDur where
   fmap f = let f' = (fmap f) in
     LD . (map f') . remLD

instance Applicative ListDur where
   pure x = LD [Duration (0,x)]
   l1 <*> l2 = LD $ do x <- remLD l1
                       y <- remLD l2
                       return $ do f <- x; a <- y; return (f a)

instance Monad ListDur where
   return = pure
   l >>= k = LD $ do x <- remLD l
                     g x where
                       g(Duration (i,x)) = let u = (remLD (k x))
                          in map (\(Duration (i',x)) -> Duration (i + i', x)) u

manyChoice :: [ListDur a] -> ListDur a
manyChoice = LD . concat . (map remLD)

--------- Utils --------------

zero = const 0

right  = Right ()

--------- List Utils ----------

fromPair (a,b) = [a,b]

makePairs :: (Eq a) => [a] -> [(a,a)]
makePairs as = normalize $ do a1 <- as; a2 <- as; [(a1,a2)]
                                
normalize :: (Eq a) => [(a,a)] -> [(a,a)]
normalize l = removeSw $ filter p1 l where
  p1 (x,y) = if x /= y then True else False

removeSw :: (Eq a) => [(a,a)] -> [(a,a)]
removeSw [] = []
removeSw ((a,b):xs) = if elem (b,a) xs then removeSw xs else (a,b):(removeSw xs)
