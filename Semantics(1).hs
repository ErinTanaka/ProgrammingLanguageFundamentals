module Semantics where
import System.IO
-- type Prog = [Cmd]
--
-- data Cmd = LD Int
--            | ADD
--            | MULT
--            | DUP
--            deriving Show
--
-- type Stack = [Int]
-- type D = [Int]
--
-- sem :: Prog -> D
-- sem [] = []
--
-- semCmd :: Cmd -> D
-- semCmd (LD i) xs = xs ++ [i]
-- semCmd ADD (x:y:s) = if (length xs) >= 2 then init(init xs) ++ [last xs + last(init xs)]
--                     else []
-- semCmd (MULT (xs)) = init(init xs) ++ [last xs * last(init xs)]
-- semCmd (DUP (xs)) = xs ++ [last xs]


--exercise 3 done-skis
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

--test=sem'(Seq)
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

--sem" (Seq (Pen Down) (Seq (Moveto 5 3) (Moveto 7 10)))

testpp=sem' (Seq (Pen Up) (Moveto 5 10))
--sem' (Seq cmds1 cmds2) = snd(semS (cmds1 State(Up, 0, 0)))-- ++ snd(semS (cmds2 (fst(semS (cmds1 (Up, 0, 0))))))
