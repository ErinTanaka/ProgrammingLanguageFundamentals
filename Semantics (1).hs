module Semantics where

type Prog = [Cmd]

data Cmd = LD Int
           | ADD [Int]
           | MULT [Int]
           | DUP [Int]
           deriving Show

type Stack = [Int]
type D = [Int]

sem :: Prog -> D
sem [] = []

semCmd :: Cmd -> D
semCmd (LD i) = [i]
semCmd (ADD (xs)) = if (length xs) >= 2 then init(init xs) ++ [last xs + last(init xs)]
                    else []
semCmd (MULT (xs)) = init(init xs) ++ [last xs * last(init xs)]
semCmd (DUP (xs)) = xs ++ [last xs]


--exercise 3
data CmdLogo = Pen Mode
          | Moveto Int Int
          | Seq CmdLogo CmdLogo
          deriving Show

data Mode = Up | Down
          deriving (Eq, Show)

type State = (Mode, Int, Int)
type Line = (Int, Int, Int, Int)
type Lines = [Line]

semS :: CmdLogo -> State -> (State, Lines)
semS (Pen mode) (cmode, x, y) = ((mode, x, y), [])
semS (Moveto x y) (cmode, cx, cy) | cmode == Down = ((cmode, x, y), [(x,y,cx,cy)])
                                  | otherwise = ((cmode, x, y), [])
semS (Seq cmd1 cmd2) (cmode, cx, cy)=(fst(semS cmd2 (fst(semS cmd1 (cmode, cx, cy)))), snd(semS cmd1 (cmode, cx, cy))++snd( semS cmd2 (cmode, cx, cy)))

sem' :: CmdLogo -> Lines
sem' (Pen mode) = snd(semS (Pen mode) (Up, 0, 0))
sem' (Moveto x1 y1) = snd(semS (Moveto x1 y1) (Up, 0, 0))
sem' (Seq cmds1 cmds2) = snd(semS (cmds1 (Up, 0, 0))) ++ snd(semS (cmds2 (fst(semS (cmds1 (Up, 0, 0))))))
