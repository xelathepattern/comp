import Text.Regex.TDFA ((=~), getAllTextMatches)

--TODO: refactor to have fids that only when evaluated get translated to actual functions via a lookup table
data Props = P { arity :: Int, precedence :: Int, leftAssoc :: Bool} 
data Symbol a = E | C a | L | R | O ([a] -> a) Props -- S means Symbol, C is constant, O is operator, L R are parens, E is end of input

getC :: Symbol a -> a 
getC (C x) = x
getC _ = error "expected constant symbol to get"

type Stack a = [Symbol a]
estacks = ([], [])

updateStacks :: Symbol a -> Stack a -> (Stack a, [Symbol a]) -- read in -> opStack -> (opStack, out)
updateStacks s@(C x) (opStack) = (opStack, [s])

updateStacks L (opStack) = (L:opStack, []) -- push left paren onto opStack
-- when pushing right paren, pop opStack into outStack until you find left paren 
-- when L is found, discard it
updateStacks R (L:opsRest) = (opsRest, [])
-- until L is found, pop off opsStack into outStack
updateStacks R (f:opsRest) = (fst recurseResult, snd recurseResult ++ [f])
  where recurseResult = updateStacks R opsRest

-- when pushing an operator, cases based on precedence and associativity
updateStacks fopO@(O _ f_p) ops@((gopO@(O _ g_p)):opsRest)  
    | (f_prec > g_prec) = (fopO:ops, []) -- if f prec larger, then add to ops
    | (f_prec < g_prec) = (fopO:opsRest, [gopO]) -- if f prec smaller, then pop g from opStack into outStack, and only then push f into opStack
    | (f_prec == g_prec) && (leftAssoc f_p) = (fopO:opsRest, [gopO]) -- if prec equal and f left assoc then do same as previous case
    where f_prec = precedence f_p
          g_prec = precedence g_p 

-- op pushing degenerate case when e.g. the opStack has a left paren, or a constant, or empty
updateStacks f@(O _ _) (opStack) = (f:opStack, [])

rpnEvalUpdateStack :: Symbol a -> Stack a -> Stack a
rpnEvalUpdateStack (C x) (stack) = (C x):stack
rpnEvalUpdateStack (O f f_p) (stack) = (C (f toFeed)):rest
  where n = arity f_p 
        toFeed = map getC (take n stack)
        rest = drop n stack

rpnEvalUpdateStack L _ = error "L in rpnEvalUpdateStack"
rpnEvalUpdateStack R _ = error "R in rpnEvalUpdateStack"
rpnEvalUpdateStack E _ = error "E in rpnEvalUpdateStack"

evalUpdateStacks :: Symbol a -> (Stack a, Stack a) -> (Stack a, Stack a)
-- if end of input symbol, pop all of opStack into rpnStack
evalUpdateStacks E ([], rpnStack) = ([], rpnStack)
evalUpdateStacks E (s:opStack, rpnStack) = evalUpdateStacks E (opStack, rpnEvalUpdateStack s rpnStack)
-- for any other symbol, update the opStack of the shunting yard algorithm, and take any output
-- from the shunting yard algorithm and use it to update the rpnEval stack.
evalUpdateStacks s (opStack, rpnStack) = (updatedOpStack, updatedPushRPN)
  where (updatedOpStack, maybePushRPNs) = updateStacks s opStack
        updatedPushRPN = foldl (flip rpnEvalUpdateStack) rpnStack maybePushRPNs

unwrapOut :: (Stack a, Stack a) -> a
unwrapOut ([], [C x]) = x
unwrapOut _ = error "can't unwrap when either opStack nonempty, rpnStack longer than 1, or rpnStack's element isn't a constant symbol"

evalInputSymbols :: [Symbol a] -> a
evalInputSymbols inputSymbols = unwrapOut (foldl (flip evalUpdateStacks) estacks inputSymbols)



type StringOrToken a = Either String (Symbol a)
convertMinus :: (Num a) => [String] -> [StringOrToken a] --detect binary minus and convert those
convertMinus [] = []
convertMinus (")":"-":xs) = (Left ")"):(Right s):(convertMinus xs) --if prev was right paren
convertMinus (possN:"-":xs) = if (last possN) `elem` "0123456789" --if prev was a number (numbers always end in a digit)
                            then (Left possN):(Right s):(convertMinus xs)
                            else (Left possN):(convertMinus ("-":xs))
convertMinus (x:y:xs) = (Left x):(convertMinus (y:xs))
convertMinus ([x]) = [Left x]

tokenizeRest :: (Floating a, Read a) => [StringOrToken a] -> [Symbol a] --only to be called after the binary minuses have been detected and converted
tokenizeRest [] = []
tokenizeRest ((Left "("):xs) = L:(tokenizeRest xs)
tokenizeRest ((Left ")"):xs) = R:(tokenizeRest xs)
tokenizeRest ((Left "^"):xs) = e:(tokenizeRest xs)
tokenizeRest ((Left "*"):xs) = m:(tokenizeRest xs)
tokenizeRest ((Left "/"):xs) = d:(tokenizeRest xs)
tokenizeRest ((Left "+"):xs) = p:(tokenizeRest xs)
tokenizeRest ((Left "-"):xs) = n:(tokenizeRest xs)
--otherwise, must be constant
tokenizeRest ((Left cStr):xs) = (C (read cStr)):(tokenizeRest xs)
--otherwise, isn't a string and is instead a token 
tokenizeRest ((Right x):xs) = x:(tokenizeRest xs)


regexSplit :: String -> [String]
regexSplit s = getAllTextMatches (s =~ (opGroup ++ "|" ++ numGroup)) :: [String]
  where opGroup = "([\\+\\/\\*\\^\\(\\)\\-])"
        numGroup = "(([0-9]|\\.)+)"


tokenize :: (Floating a, Read a) => String -> [Symbol a]
tokenize s = (tokenizeRest $ convertMinus regexMatches) ++ [E]
  -- the regex is:
  -- op group: ([\+\/\*\^\(\)\-]) -- without the escape chars this is +/*^()-)
  -- OR num group: (([0-9]|\.)+) -- that is, at least one of [0-9] or literal period.
  where regexMatches = regexSplit s

evalStr = evalInputSymbols . tokenize

e :: Floating a => Symbol a
e = O (foldl1 $ flip (**)) (P 2 4 False)
m :: Num a => Symbol a
m = O (foldl1 (*)) (P 2 3 True)
d :: Fractional a => Symbol a
d = O (foldl1 $ flip (/)) (P 2 3 True)
p :: Num a => Symbol a
p = O (foldl1 (+)) (P 2 2 True)
s :: Num a => Symbol a
s = O (foldl1 $ flip (-)) (P 2 2 True)
n :: Num a => Symbol a 
n = O ((0-) . head) (P 1 5 False)

st = "(1+2)*3-1"
out = evalStr st
