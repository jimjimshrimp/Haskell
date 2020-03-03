
import Data.List
import Data.Maybe
import qualified Data.Maybe           as Mb
-------------------------
-------- PART A --------- 
-------------------------

type Var = String

data Term =
    Variable Var
  | Lambda   Var  Term
  | Apply    Term Term
  --deriving Show

instance Show Term where
  show = pretty

example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Variable "a")) (Variable "x")) (Variable "b")))
--example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Variable "c")) (Variable "x")) (Variable "b")))

pretty :: Term -> String
pretty = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m 
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m



------------------------- Assignment 1

numeral :: Int -> Term
numeral i =  Lambda "f" (Lambda "x" (numeral' i))

numeral' :: Int -> Term
numeral' 0 = Variable "x"
numeral' i = Apply (Variable "f")(numeral' (i-1))
-- This function does the recursive applying for numeral.

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x == y    = x : merge xs ys
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


------------------------- Assignment 2

--fun :: String -> [Char]

--map fun ['a'..'z']

variables :: [Var]
--l1 = map ("a" :) ['1'..'3']
--variables' 0 xs  = xs
--variables' n xs = foldl (+) n xs 
--variables' xs 0 = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]
--variables' xs 0 = map (:[]) xs
--variables' xs i = variables' xs (i-1) ++ map (: show i) xs  
--variables = variables' ['a'..'z'] (maxBound::Int)

variables' []=[]
variables' (i: is)= (map (: show i) ['a'..'z']) ++ (variables' is)
variables = (map (:[]) ['a'..'z']) ++ variables' [1..]

filterVariables :: [Var] -> [Var] -> [Var]
filterVariables xs ys = [ x | x<- xs, not (x `elem` ys)] 

fresh :: [Var] -> Var

fresh xs = filterVariables variables xs !! 0

used :: Term -> [Var]
used (Variable a) = [a]
--used used (Lambda x xs) = ([x]) ++ (used xs)
used (Lambda x xs) = merge ([x]) (used xs)
used (Apply n m)  = merge (used n)  (used m)


------------------------- Assignment 3

rename :: Var -> Var -> Term -> Term
rename x y (Variable z) 
   | x == z = (Variable y)
   | otherwise = Variable z
rename x y (Lambda z n) 
   | x == z = Lambda y (rename x y n)
   | otherwise = Lambda z (rename x y n)
rename x y (Apply  n m) = Apply (rename x y n) (rename x y m)



free (Variable y) = [y]
free (Lambda a b) = delete a (free b) 
free (Apply c d) = (free c) `union` (free d)

substitute :: Var -> Term -> Term -> Term

substitute s n (Variable x)
   | s == x = n
   | otherwise = (Variable x)
                    --where k = fresh (used n)
substitute s n (Lambda z m)
   |s == z = Lambda z m 
   --not (z `elem` (used n)) && s /= z = let z' = fresh ([z]++ used n++ used m) in substitute z (Variable z') (Lambda z (substitute s n m))
   --z `elem` (used n)&& s /=z = let z' = fresh (used n ++ used m) in substitute s n (Lambda z' (substitute z (Variable z') m))
   |otherwise = let z'= fresh ((used n)++(used m)++[s]) in Lambda z' (substitute s n (rename z z' m))
 
substitute s n (Apply a b) = Apply (substitute s n a) (substitute s n b)


------------------------- Assignment 4
{-
--beta :: Term -> [Term]
beta (Variable a) = Variable a
beta (Lambda a b) = Lambda a (beta b)
beta (Apply (Lambda a b) x ) = substitute a x b 
beta (Apply x y) = Apply (beta x) (beta y)

beta'(Variable a) = Variable a
beta' (Lambda a b) = Lambda a (beta b)
beta' (Apply x y) = Apply (beta' x) y

beta'' xs = [beta xs] ++ [beta' xs]


betas (Variable _) = []
betas (Lambda a b) = [Lambda a b' | b' <- (betas b)]
betas (Apply (Lambda a b) x ) = [substitute a x b] 
betas (Apply x y) = [Apply x' y | x'<- (betas x) ] ++ [Apply x y'| y' <- (betas y)] 


normalize xs = do print xs
                  let result = betas xs in 
                      if null(result)
                         then return ()
                         else do 
                              normalize (head(result))
-}

beta :: Term -> [Term]

beta (Apply (Lambda x n) m) = [substitute x m n] ++ [Apply (Lambda x bn) m | bn <- (beta n)] ++ [Apply (Lambda x n) bm | bm <- (beta m)]

beta (Apply n m) = [Apply bn m | bn <- (beta n)] ++ [Apply n bm | bm <- (beta m)]

beta (Lambda n m) = [Lambda n bm | bm <- (beta m)]

beta (Variable z) = []

normalize xs = do print xs
                  let result = beta xs in 
                      if null(result)
                         then return ()
                         else do 
                              normalize (head(result))

------------------------- 

a_beta :: Term -> [Term]
a_beta (Variable _) = []
a_beta (Lambda a b) = [Lambda a b' | b' <- (a_beta b)]
--a_beta (Apply (Lambda a b) x ) = [substitute a x b] 
a_beta (Apply x y) = [Apply x' y | x'<- (a_beta x) ] ++ [Apply x y'| y' <- (a_beta y)] 

{-
a_normalize :: Term -> IO ()

a_normalize xs = do print xs
                   let result = a_beta xs in 
                      if null(result)
                         then return ()
                         else do 
                              normalize (head(result))
-}

-------------------------

example1 :: Term

example1 = Apply (numeral 3) (numeral 3)

-- Requires many more steps in normal order than applicative order



example2 :: Term

example2 = Apply (numeral 0) (Apply (numeral 0) (numeral 0))

-- Requires one less step for normal order over applicative order



-------------------------
-------- PART B --------- 
-------------------------


------------------------- Assignment 5 (PAM)

type PState=(Term, [Term])

state1 = (Lambda "x" (Lambda "y" (Variable "x")) , [Variable "Yes", Variable "No"])

term1 = Apply (Apply (Lambda "x" (Lambda "y" (Variable "x"))) (Variable "Yes")) (Variable "No")

term2 = Apply (Apply (Lambda "b" (Apply example (Variable "Yes"))) (Lambda "z" (Variable "z"))) (Variable "No")


p_start :: Term -> PState
p_start xs = (xs, []) 

p_step :: PState -> PState

p_step ((Lambda x n), s) = ((substitute x (head s) n), (tail s)) 

p_step ((Apply n m), s) = (n, [m] ++ s)

-- This performs one transition step.


p_final :: PState -> Bool

p_final ((Lambda x n), s) = if null(s) then True else False

p_final ((Variable x), s) = True

p_final (n, s) = False

-- This checks to see if a state is final.


p_run :: Term -> IO ()
p_run xs = p_run' (p_start xs)

p_run' ys =    do print ys
                  let result = p_step ys in 
                      if (p_final ys)
                         then return()
                         else do 
                              p_run' result

p_readback :: PState -> Term

p_readback (xs, []) = xs

p_readback (xs, termList) = Apply (p_readback ((xs, (tail termList)))) (head termList)


------------------------- Assignment 6 (KAM)
type State = (Closure, [(Term, Env)])
type Env = [(Var, Closure)]
data Closure =  Closure(Term, Env)  --deriving (Show)

state2 = (Closure(Apply (Lambda "x" (Variable "x")) (Variable "y"),[("y",Closure(Lambda "z" (Variable "z"), []))]),[]):: State

instance Show Closure where
  show (Closure(a,b)) = show a ++ "," ++ show b

start :: Term -> State
start xs = (Closure(xs, []),[])

step :: State -> State
step (Closure (Variable a, (b, Closure(c, d)) : e), f)
    | a == b = (Closure (c, d), f)
    | otherwise = (Closure(Variable a, e) , f)
step (Closure(Lambda a b, c), ((d, e): x)) = (Closure(b, (a, Closure(d, e)) : c), x)
step (Closure(Apply a b, c), d) = (Closure(a, c), ((b, c) : d))

final :: State -> Bool
final (Closure (Lambda x n, e), []) = True
final (Closure (Variable x, []), s) = True
final (_ , _) = False

run :: Term -> IO ()
run xs = run' (start xs)
run' ys =      do print ys
                  let result = step ys in 
                      if (final ys)
                         then return()
                         else do 
                              run' result