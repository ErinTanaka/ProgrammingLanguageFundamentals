module Semantics where
import System.IO
import Data.Maybe

type Prog = [Cmd]

data Cmd = LD Int
           | ADD
           | MULT
           | DUP
           deriving Show

type Stack = [Int]
type D = Maybe Stack -> Maybe Stack

sem :: Prog -> D
sem [] xs = xs
sem (p:ps) xs = sem ps (semCmd p xs)


semCmd :: Cmd -> D
semCmd (LD i) (Just xs) = Just (i:xs)

semCmd ADD (Just (x:y:xs)) = Just (x+y:xs)

semCmd MULT (Just (x:y:xs)) = Just (x*y:xs)

semCmd DUP (Just (x:xs)) = Just (x:x:xs)

semCmd _ _ = Nothing

--Exercise 2
--2a)
type Prog2 = [Cmd2]

data Cmd2 = LD2 Int
           | ADD2
           | MULT2
           | DUP2
           | DEF String Prog2
           | CALL String
           deriving Show

--2b)
type Macros = [(String, Prog2)]
type Stack2 = [Int]
type State2 = Maybe [(Macros, Stack2)] -> Maybe [(Macros, Stack2)]

--2c)
sem2 :: Prog2 -> State2
sem2 [] st = st
sem2 (p:ps) st = sem2 ps (semCmd2 p st)

semCmd2 :: Cmd2 -> State2
semCmd2 (LD2 i) (Just [(macros, stack)]) = Just [(macros, (i:stack))]
semCmd2 ADD2 (Just [(macros, (x:y:xs))]) = Just [(macros, (x+y:xs))]
semCmd2 MULT2 (Just [(macros, (x:y:xs))]) = Just [(macros, (x*y:xs))]
semCmd2 DUP2 (Just [(macros, (x:xs))]) = Just [(macros, (x:x:xs))]
semCmd2 (DEF name cmdls) (Just [(macros, xs)]) = Just [((name, cmdls):macros, xs)]
--semCmd2 (CALL name) (Just [(macros, xs)]) = Just [(macros, (snd(semCmd2 (lookup name macros) (xs))))]
--semCmd2 (CALL name) (Just [(macros, xs)]) = Just [(macros, maybeToList (lookup name [(macros, xs)]))]
semCmd2 (CALL name) (Just [(macros, xs)]) = sem2 (fromJust(lookup (name) (macros))) (Just [(macros, xs)])

--Exercise 3
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
sem' (Seq cmds1 cmds2) = snd(semS (cmds1) (Up, 0, 0)) ++ snd(semS (cmds2) (fst(semS (cmds1) (Up, 0, 0))))

testsndtwo=(fst(semS (Pen Down) (Up, 0, 0)))

-- Pretty printing of lines:
-- write an svg file
--
ppLines :: Lines -> IO ()
ppLines ls = do h <- openFile "MiniLogo.svg" WriteMode
                hPutStr h (svgHdr++concatMap ppLine ls++svgFtr)
                hClose h

-- fixed size and maginifaction factor
-- (can be generalized easily)
--
factor=100
yMax=1100

svgHdr = "<?xml version=\"1.0\" standalone=\"no\"?>\n \
         \ <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n \
         \    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n \
         \ <svg width=\"12cm\" height=\"11cm\" viewBox=\"0 0 1200 1100\"\n \
         \    xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">\n \
         \ <title>Mini Logo Result Viewer</title>\n \
         \ <desc>A set of line segments</desc>\n \
         \ <rect x=\"10\" y=\"10\" width=\"1180\" height=\"1080\" \
         \       fill=\"none\" stroke=\"red\" /> "
svgFtr = "</svg>\n"

ppLine :: Line -> String
ppLine (x,y,x',y') = "<path d=\"M "++ppPos x y++" L "++ppPos x' y'++"\" "++
                     "stroke=\"blue\" stroke-width=\"5\" />\n"

ppPos :: Int -> Int -> String
ppPos x y = show (50+factor*x)++" "++show (yMax-50-factor*y)

testpp=sem' (Seq (Pen Up) (Moveto 5 10))
