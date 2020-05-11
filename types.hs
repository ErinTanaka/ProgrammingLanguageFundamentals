--Erin Tanaka, Madison Dhanens , Hema Susmita Padala, Mitch Hoesing
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

rankC :: Cmd -> CmdRank

rankC (LD _) = (0,1)
rankC ADD = (2,1)
rankC MULT = (2,1)
rankC DUP = (1,2)
rankC INC = (1,1)
rankC SWAP = (2, 2)
rankC (POP i) = (i,0)

rankP :: Prog -> Maybe Rank
rankP [] = Just 0
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

--semantics for stack language so we can test stuff
sem :: Prog -> Stack
sem prog = semCmd prog []

semCmd :: Prog -> Stack ->Stack
semCmd ((LD i):cmds) stack = semCmd cmds (i:stack)
semCmd (ADD:cmds) (x:y:xs) = semCmd cmds (x+y:xs)
semCmd (MULT:cmds) (x:y:xs) = semCmd cmds (x*y:xs)
semCmd (DUP:cmds) (x:xs) = semCmd cmds (x:x:xs)
semCmd (INC:cmds) (x:xs) = semCmd cmds (x+1:xs)
semCmd (SWAP:cmds) (x:y:xs)= semCmd cmds (y:x:xs)
semCmd ((POP i):cmds) (x:xs) |i==0 = semCmd cmds (x:xs)
                            | otherwise = semCmd ((POP (i-1)):cmds) xs

--Exercise 2: Shape Language
data Shape= X
          | TD Shape Shape
          | LR Shape Shape
          deriving Show

type BBox = (Int, Int)
--width, height

--from Shape.hs on Professor Erwig's class website
-- semB :: Shape -> Image
-- semB X           = [(1,1)]
-- semB (LR s1 s2) = d1 ++ [(x+maxx d1,y) | (x,y) <- semB s2]
--                  where d1 = sem s1
-- semB (TD s1 s2) = d2 ++ [(x,y+maxy d2) | (x,y) <- semB s1]
--                  where d2 = sem s2
--
-- maxx :: [BBox] -> Int
-- maxx = maximum . map fst
--
-- maxy :: [BBox] -> Int
-- maxy = maximum . map snd

--a)
bbox :: Shape -> BBox
bbox X = (1,1)
bbox (TD a b) | x >= w = (x, y+z)
              | x < w = (w, y+z)
              where (x,y) = bbox a
                    (w,z) = bbox b
bbox (LR a b) | y >= z = (x+w, y)
              | y < z = (x+w, z)
              where (x,y) = bbox a
                    (w,z) = bbox b

--b)
rect :: Shape -> Maybe BBox
rect X = Just (1,1)
rect (TD a b) | x == w = Just (x, y+z)
              where (x,y) = bbox a
                    (w,z) = bbox b
rect (LR a b) | y==z = Just (x+w, y)
              where (x,y) = bbox a
                    (w,z) = bbox b
rect _ = Nothing

--Exercise 3: Parametric Polymorphism
--Exercise 3. Parametric Polymorphism

--(a) Consider the functions f and g, which are given by the following two function definitions.
--f x y = if null x then [y] else x
--g x y = if not (null x) then [] else [y]
--g [] y = []
--(1) What are the types of f and g?
--(2) Explain why the functions have these types.
--(3) Which type is more general?
--(4) Why do f and g have different types?

-- Answers:
--(1) f :: [t] -> t -> [t], g :: [a] -> a1 -> [a1]
--(2) f has this type because x and y have not been inferred to be anymore specific of a type.
	--g has this type because the return type will not be of type x but either an empty list
	--or a list of type y.
--(3) Function f can have any type t whereas in function g x is a list and y is an element of a list.
--(4) Because function f will work on any type t where g will work on a list with elements of type "a" and a different type "a1".

f x y = if null x then [y] else x

g x y = if not (null x) then [] else [y]
g [] y = []

--(b) Find a (simple) definition for a function h that has the following type.
--		h :: [b] -> [(a, b)] -> [b]

--Note that the goal of this part of the exercise is not to find a particularly useful function, but a
--function definition that has the given type signature. More precisely, you should give a definition for
--h without a type annotation, for which GHCi will then infer the shown type. A perfectly valid way to
--approach this exercise is to experiment with function definitions, without giving type declarations,
--and have GHCi infer the type using the :t interpreter command.

h [x] [(y,z)] = [x]
h [] [(y,z)] = [z]

--(c) Find a (simple) definition for a function k that has the following type.
--		k :: (a -> b) -> ((a -> b) -> a) -> b

k x y = x (y x)

--(d) Can you define a function of type a -> b? If yes, explain your definition. If not, explain why it is so
	--difficult.

	--I cannot, with static type checking I do not believe this is possible as it will never pass the
	--static type checker.
