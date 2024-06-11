{-# LANGUAGE FlexibleInstances #-}
module Adventurers where

import Control.Monad
import DurationMonad
import Data.List

-- The list of adventurers
data Adventurer = P1 | P2 | P5 | P10 deriving (Show,Eq)
-- Adventurers + the lantern
type Objects = Either Adventurer ()

-- The time that each adventurer needs to cross the bridge
getTimeAdv :: Adventurer -> Int
getTimeAdv P1 = 1
getTimeAdv P2 = 2
getTimeAdv P5 = 5
getTimeAdv _  = 10

{-- The state of the game, i.e. the current position of each adventurer
+ the lantern. The function (const False) represents the initial state
of the game, with all adventurers and the lantern on the left side of
the bridge. Similarly, the function (const True) represents the end
state of the game, with all adventurers and the lantern on the right
side of the bridge.  --}

type State = Objects -> Bool

instance Show State where
  show s = (show . fmap show) [s (Left P1),
                                 s (Left P2),
                                 s (Left P5),
                                 s (Left P10),
                                 s (right)]

instance Eq State where
  (==) s1 s2 = and [s1 (Left P1) == s2 (Left P1),
                    s1 (Left P2) == s2 (Left P2),
                    s1 (Left P5) == s2 (Left P5),
                    s1 (Left P10) == s2 (Left P10),
                    s1 (right) == s2 (right)]



-- The initial state of the game
gInit :: State
gInit = const False

-- Changes the state of the game for a given object
changeState :: Objects -> State -> State
changeState a s = let v = s a in (\x -> if x == a then not v else s x)

-- Changes the state of the game of a list of objects 
mChangeState :: [Objects] -> State -> State
mChangeState os s = foldr changeState s os

-------------------------------------------------------------------------------
right = Right()

-- https://blog.lahteenmaki.net/combinator-birds.html
-- ψ = x y z w = x (y z) (y w)
psi = join . ((flip . ((.) .)) .) . (.)
-- Φ x y z w = x (y w) (z w)
phoenix = (ap .) . (.)


-- gets a list of all adventurers that have the lamp and the lamp

-- ALLVALIDPLAYS
peopleThatHaveTheLamp :: State -> [Objects]
peopleThatHaveTheLamp s = (right :) $ filter ((== s right) . s) $ Left <$> [P1, P2, P5, P10] 
-- peopleThatHaveTheLamp s = filter (\x -> s (Right ()) == s x) [Left P1, Left P2, Left P5, Left P10, Right()]

-- takes the list of all adventures that have the lamp and make a list of all the crossings that can be made (lamp included)
possibleCrossings :: State -> [[Objects]]
possibleCrossings = filter (\p -> (elem right p) && psi (||) (length p ==) 2 3) . subsequences . peopleThatHaveTheLamp
--possibleCrossings = filter (\p -> (elem right p) && ((length p) == 2 || (length p) == 3)) . subsequences . peopleThatHaveTheLamp

-- more complete getTimeAdv function that also includes the lantern
getTimeObj :: Objects -> Int
getTimeObj (Left a) = getTimeAdv a
getTimeObj _ = 0

-- take a crossing and returns the time of this crossing (the max of time that takes the more late person)
getCrossingTime :: [Objects] -> Int
getCrossingTime = foldr (max . getTimeObj) 0

-- take a time and a listDur and applies wait with that time to all durations of the list
waitList :: ListDur a -> Int ->  ListDur a
waitList = c LD map wait remLD
--waitList l t = LD $ map (wait t) (remLD l)

c k f g h l t = k $ f (g t) (h l) 
-- Φ x y z w = x (y w) (z w)

-- for a given state of the game, the function presents all the possible moves that the adventurers can make.
allValidPlays :: State -> ListDur State
allValidPlays s = manyChoice $ map (\c -> waitList (return (mChangeState c s)) (getCrossingTime c)) (possibleCrossings s)
   
-- exec
{-- For a given number n and initial state, the function calculates
all possible n-sequences of moves that the adventurers can make --}
exec :: Int -> State -> ListDur State
exec 0 s = pure s
exec n s = do s1 <- allValidPlays s
              exec (n-1) s1
  
--------  Questoes   ----------------------------------------------------------

{-- Is it possible for all adventurers to be on the other side
in <=17 min and not exceeding 5 moves ? --}
-- yes it can in 5 mooves and this prooves it
leq17 :: Bool
leq17 = any (\(Duration (t,s)) -> t <= 17 && s == const True) (remLD (exec 5 gInit))


{-- Is it possible for all adventurers to be on the other side
in < 17 min ? --}
-- this prooves it because P10 must cross at least one time wich takes 10 minutes, 
-- supposing now each cross then only takes 1 minute (the fastest adventurer)
-- we only have 6 crossing lefts if we dont want to exceed 16 minutes
-- so there cannot be more than 7 crossing and there is no need to calculate more than that
l17 :: Bool
l17 = any (\(Duration (t,s)) -> t < 17 && s == const True) (remLD (exec 7 gInit))

-------- Valorização ----------------------------------------------------------

-- FASTEST

-- like allvalidplays but with historic just use the first one of the list
allValidPlaysHist :: [State] -> ListDur [State]
allValidPlaysHist [] = LD []
allValidPlaysHist (s:ss) = manyChoice $ map (\c -> waitList (pure ([mChangeState c s]++[s]++ss)) (getCrossingTime c)) (possibleCrossings s)

-- like exec but with historic just use the first one of the list
execHist :: Int -> [State] -> ListDur [State]
execHist 0 s = pure s
execHist n s = do s1 <- allValidPlaysHist s
                  execHist (n-1) s1
                 
-- show the solutions of 17 minutes
fastest :: [[State]]
fastest = map (reverse . getValue) fastestAux
fastestAux :: [(Duration [State])]
fastestAux = filter (\(Duration (t,(s:ss))) -> t <= 17 && s == const True) (remLD (execHist 5 [gInit]))


-- SOLUTIONS

-- show the number of solutions that have less than x crossings
solutionsC :: Int -> Int
solutionsC x = length (filter (\(Duration (t,s)) -> s == const True) (remLD (exec x gInit)))

-- show the number of solutions that have less than x crossings and less than y minutes
solutionsCT :: Int -> Int -> Int
solutionsCT x y = length (filter (\(Duration (t,s)) -> t <= y && s == const True) (remLD (exec x gInit)))
   


-------------------------------------------------------------------------------
-- Implementation of the monad used for the problem of the adventurers.
-- Recall the Knight's quest 

data ListDur a = LD [Duration a] deriving Show

remLD :: ListDur a -> [Duration a]
remLD (LD x) = x

instance Functor ListDur where
   fmap f = LD . map (fmap f) . remLD

instance Applicative ListDur where
   pure x = LD [ Duration(0,x) ]
   l1 <*> l2 = LD $ do x <- remLD l1
                       y <- remLD l2
                       return $ x <*> y


instance Monad ListDur where
   return = pure
   l >>= k = LD $ do x <- remLD l
                     g x where
                        g (Duration (d, a)) = let u = remLD (k a) in
                           map (\(Duration (d', a)) -> Duration (d + d', a)) u


manyChoice :: [ListDur a] -> ListDur a
manyChoice = LD . concatMap remLD
