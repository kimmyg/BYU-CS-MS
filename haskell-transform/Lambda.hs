module Lambda where
import Data.Char

data Term = Var Char | Abs Char Term | App Term Term | Empty

--instance Read Term where
--  read = readTerm

instance Show Term where
  show = showTerm

readAbs1 :: String -> (Term,String)
readAbs1 "" = (Empty,"")
readAbs1 (x:xs)
  | isAlpha x && isLower x = readAbs2 x xs
  | otherwise = error "expected a variable in an abstraction"

readAbs2 :: Char -> String -> (Term,String)
readAbs2 _ "" = (Empty,"")
readAbs2 v (x:xs)
  | x == '.' = let (t,r) = (readTerm' xs) in (Abs v t,r)
  | otherwise = error "expected . after variable"

readApp1 :: String -> (Term,String)
readApp1 "" = (Empty,"")
readApp1 xs = let (t,r) = (readTerm' xs) in readApp2 t r

readApp2 :: Term -> String -> (Term,String)
readApp2 _ "" = error "expected space, got empty string"
readApp2 t (x:xs) = if x == ' ' then readApp3 t xs else error ("expected space, got " ++ [x])

readApp3 :: Term -> String -> (Term,String)
readApp3 _ "" = error "expected term, got empty string"
readApp3 t xs = let (t',r) = (readTerm' xs) in readApp4 t t' r

readApp4 :: Term -> Term -> String -> (Term,String)
readApp4 _ _  ""     = error "expected ), got empty string"
readApp4 t t' (x:xs) = if x == ')' then (App t t',xs) else error ("expected ), got " ++ [x])

readTerm' :: String -> (Term,String)
readTerm' "" = (Empty,"")
readTerm' (x:xs)
  | isAlpha x && isLower x = (Var x,xs)
  | x == '\\' = readAbs1 xs
  | x == '('  = readApp1 xs
  | otherwise = error ("looking for start of term, found " ++ [x])

readTerm :: String -> Term
readTerm xs = let (t,r) = (readTerm' xs) in if r == "" then t else error ("read term but left with " ++ r)

showTerm :: Term -> String
showTerm t = case t of
  Var v    -> [v]
  Abs v e  -> ['\\',v,'.'] ++ (showTerm e)
  App t t' -> "(" ++ (showTerm t) ++ " " ++ (showTerm t') ++ ")"
  Empty    -> "<empty>"

main = do
  line <- getLine
  putStrLn $ show $ readTerm line
