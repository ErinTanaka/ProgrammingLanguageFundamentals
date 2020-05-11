--2a
type Circuit = (Gates, Links)
type Gates = [(Int, GateFn)]
data GateFn = And | Or | Xor | Not deriving Show
type Links = [Link]
data Link = L (Int, Int) (Int, Int) deriving Show


--2b
twob=([(1,Xor), (2,And)], [L (1,1) (2,1), L (1,2) (2,2)])

--2c
ppCircuit :: Circuit -> String
ppCircuit (g, l) = ppGates g ++ ppLinks l

ppGates :: Gates -> String
ppGates [] = ""
ppGates ((x,gf):xs) = show x ++ ":" ++ ppGateFn gf ++ ";" ++ "\n" ++ ppGates xs

ppGateFn :: GateFn -> String
ppGateFn And = "and"
ppGateFn Or = "or"
ppGateFn Xor = "xor"
ppGateFn Not = "not"

ppLinks :: Links -> String
ppLinks [] = ""
ppLinks (x:xs) = ppLink x++"\n"++ppLinks xs

ppLink :: Link -> String
ppLink (L (x, y) (z, w))= "from "++show x++"."++show y++" to "++ show z++"."++show w
lols=ppCircuit ([(1,Xor), (2,And)], [L (1,1) (2,1), L (1,2) (2,2)])
