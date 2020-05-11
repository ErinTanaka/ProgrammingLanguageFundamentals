--
-- type Prog = [Cmd]
--
-- data Cmd = LD Int
--           | ADD
--           | MULT
--           | DUP
-- type Stack = [Int]
--
-- type D = [Int]
-- sem :: Prog -> D
-- sem [] = []
-- sem (cmd:cmds)= semcmd cmd : sem cmds
--
-- semcmd :: Cmd -> D
-- semcmd (LD i) = [i]


--exercise 3

data Cmd = Pen Mode
          | Moveto Int Int
          | Seq Cmd Cmd
data Mode = Up | Down

type State = (Mode, Int, Int)
type Line = (Int, Int, Int, Int)
type Lines = [Line]

semS :: Cmd -> State -> (State, Lines)
semS (Pen mode) (cmode, x, y) = ((mode, x, y), [])
semS (Moveto x y) (cmode, cx, cy) | cmode == Down = ((cmode, x, y), [(x,y,cx,cy)])
                                  | otherwise = ((cmode, x, y), [])
semS (Seq cmd1 cmd2) (cmode, cx, cy)=(fst(semS cmd2 (fst(semS cmd1 (cmode, cx, cy)))), snd(semS cmd1 (cmode, cx, cy))++snd( semS cmd2 (cmode, cx, cy)))


--sem' :: Cmd -> Lines
