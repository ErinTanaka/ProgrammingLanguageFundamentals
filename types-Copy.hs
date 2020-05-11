--
--Homework 3: Types
--Due: May 15, 2018

module Types where
import Data.Maybe

--Exercise 1: A Rank-based Type System for the Stack Language

type Prog = [Cmd]

data Cmd = LD Int
          | ADD
          | MULT
          | DUP
          | INC
          | SWAP
          | POP Int
--a)
type Rank = Int
type CmdRank = (Int,Int)

--First, define a function rankC that maps each stack operation to its rank.
rankC :: Cmd -> CmdRank

rankC (LD _) = (0,1)
rankC ADD = (2,1)
rankC MULT = (2,1)
rankC DUP = (1,2)
rankC INC = (1,1)
rankC SWAP = (2, 2)
rankC (POP i) = (i, 0)


--define a function rankP that computes the rank of a program. The Maybe data type is used
--to capture rank errors, that is, a program that contains a rank error should be mapped to Nothing
--whereas ranks of other programs are wrapped by the Just constructor.
rankP :: Prog -> Maybe Rank
rankP [] = Just 0
-- rankP (x:xs)= rank (x:xs) ((fst (rankC x) - snd (rankC x)) + fromJust((rankP xs)))
--
-- rank :: Prog -> Rank -> Maybe Rank
-- rank (x:xs) i | i > 0 = Just i
--               | otherwise = Nothing
rankP prog = rank prog 0

rank:: Prog -> Rank -> Maybe Rank
rank (x:xs) r = if r < fst (rankC x) then Nothing
                else rank xs (r - (fst (rankC x)) + (snd (rankC x)))

--b)

type Stack = [Int]

semStatTC :: Prog -> Maybe Stack
semStatTC p = let b = (rankP p) in
              if b==Nothing then Nothing
              else  Just (sem p)


-- the new type of the function sem is Stack. The function definition can be simplified to this type because we don't have to worry about any stack underflow
sem :: Prog -> Stack
sem prog = semCmd prog []
--
semCmd :: Prog -> Stack ->Stack
semCmd ((LD i):xs) stack = (i:stack)
-------------------------------------------------------------------------- uhh done?

--Exercise 2: Shape Language
data Shape= X
          | TD Shape Shape
          | LR Shape Shape
          deriving Show

type BBox = (Int, Int)
--a)
bbox :: Shape -> BBox
bbox X = (1,1)
-- bbox X = x == Int && y == Int
        -- where (x,y) = bbox X
-- bbox TD a b = x == Int && y == Int && w == Int && z == Int = (Int,Int)
--               where (x,y) = bbox a
--                     (w,z) = bbox b
-- bbox LR a b = x == Int && y == Int && w == Int && z == Int = (Int,Int)
--               where (x,y) = bbox a
--                     (w,z) = bbox b
-- -- bbox LR a b = bbox a == (Int, Int) && bbox b == (Int, Int)
bbox _ = Nothing


--b)
--rect :: Shape -> Maybe BBox


--Exercise 3: Parametric Polymorphism
--a)
--  1)
--  2)
--  3)
--  4)

--b)

--d)
