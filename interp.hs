module Main where

import Data.Char
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

data Value = VInt Int | VBool Bool deriving Show

data Expr
  = EConst Value
  | EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EDiv Expr Expr
  | ELt Expr Expr
  | EIf Expr Expr Expr
  | EVar String
  | ELet String Expr Expr
  deriving Show

type Env = [(String, Value)]

type EvalM = ExceptT String (ReaderT Env (StateT Int Identity))

assertVInt :: Value -> EvalM Int
assertVInt (VInt i) = return i
assertVInt (VBool _) = throwError "expected integer, but got boolean"

assertVBool :: Value -> EvalM Bool
assertVBool (VInt _) = throwError "expected boolean, but got integer"
assertVBool (VBool b) = return b

assertNonZero :: Value -> EvalM Value
assertNonZero (VInt 0) = throwError "expected non-zero value"
assertNonZero v = return v

assertNonZeroExpr :: Expr -> EvalM Value
assertNonZeroExpr expr = eval expr >>= assertNonZero

evalInt :: (Int -> Int -> Int) -> Expr -> Expr -> EvalM Value
evalInt f x y = do a <- eval x >>= assertVInt
                   b <- eval y >>= assertVInt
                   return $ VInt (f a b)

evalOrd :: (Int -> Int -> Bool) -> Expr -> Expr -> EvalM Value
evalOrd f x y = do a <- eval x >>= assertVInt
                   b <- eval y >>= assertVInt
                   return $ VBool (f a b)

evalIf :: Expr -> Expr -> Expr -> EvalM Value
evalIf x y z = do a <- eval x >>= assertVBool
                  if a then
                    eval y
                  else
                    eval z

evalVar :: String -> EvalM Value
evalVar n = do env <- ask
               case lookup n env of
                 Just v  -> return v
                 Nothing -> throwError ("unbound variable " ++ n)

evalLet :: String -> Expr -> Expr -> EvalM Value
evalLet n x y = do a <- eval x
                   local ((n, a):) $ eval y

countUp :: EvalM ()
countUp = do cnt <- get
             put (cnt + 1)

eval :: Expr -> EvalM Value
eval (EConst v) = return v
eval (EAdd x y) = countUp >> evalInt (+) x y
eval (ESub x y) = evalInt (-) x y
eval (EMul x y) = evalInt (*) x y
eval (EDiv x y) = assertNonZeroExpr y >> evalInt div x y
eval (ELt x y) = evalOrd (<) x y
eval (EIf x y z) = evalIf x y z
eval (EVar n) = evalVar n
eval (ELet n x y) = evalLet n x y

newtype Parser a = Parser { parse :: String -> [(a,String)] }

char :: Char -> Parser Char
char c = Parser go
  where
    go (c':cs) | c == c' = [(c, cs)]
    go _ = []

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser go
  where
    go str = (parse p) str ++ (parse q) str

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return x = Parser (\str -> [(x, str)])
  m >>= f = Parser (\str ->
                     do (x,str1) <- (parse m) str
                        (y,str2) <- (parse (f x)) str1
                        return (y, str2))

runParser :: Parser a -> String -> [a]
runParser p str = [ x | (x, s) <- (parse p) str, null s ]

string :: String -> Parser String
string [] = return []
string (c:cs) = do x  <- char c
                   xs <- string cs
                   return $ x:xs

digit :: Parser Int
digit = digitToInt <$> digit'

digit' :: Parser Char
digit' = foldl1 (+++) $ map char ['0'..'9']

number :: Parser Int
number = do d <- digit
            e <- number' d
            return $ e

number' :: Int -> Parser Int
number' i = (do d <- digit
                ds <- number' (i * 10 + d)
                return $ ds)
            +++
            (return i)

alpha :: Parser Char
alpha = foldl1 (+++) $ map char (['a'..'z'] ++ ['A'..'Z'])

space :: Parser ()
space = do (char ' ') +++ (char '\t') +++ (char '\n')
           return ()

spaces :: Parser ()
spaces = (do space; spaces) +++ (return ())




program :: Parser Expr
program = do spaces
             e <- parseE
             spaces
             return e

identifier :: Parser String
identifier = do hd <- alpha
                tl <- identifier'
                return $ [hd] ++ tl

identifier' :: Parser String
identifier' = (do c  <- alpha +++ (intToDigit <$> digit)
                  cs <- identifier'
                  return $ [c] ++ cs)
              +++
              (return "")

parseE :: Parser Expr
parseE = parseI +++ parseL

parseI :: Parser Expr
parseI = do string "if"
            spaces
            e1 <- parseE
            spaces
            string "then"
            spaces
            e2 <- parseE
            spaces
            string "else"
            spaces
            e3 <- parseE
            return $ EIf e1 e2 e3

parseL :: Parser Expr
parseL = do e <- parseM
            f <- parseL' e
            return $ f

parseL' :: Expr -> Parser Expr
parseL' e = (do spaces
                string "+"
                spaces
                f <- parseM
                g <- parseL' (EAdd e f)
                return $ g)
            +++
            (do spaces
                string "-"
                spaces
                f <- parseM
                g <- parseL' (ESub e f)
                return $ g)
            +++
            (return e)

parseM :: Parser Expr
parseM = do e <- parseK
            f <- parseM' e
            return $ f

parseM' :: Expr -> Parser Expr
parseM' e = (do spaces
                string "*"
                spaces
                f <- parseM
                g <- parseM' (EMul e f)
                return $ g)
            +++
            (do spaces
                string "/"
                spaces
                f <- parseM
                g <- parseM' (EDiv e f)
                return $ g)
            +++
            (return e)

parseK :: Parser Expr
parseK = parseN +++ parseB +++ parseC

parseN :: Parser Expr
parseN = do n <- number
            return $ EConst (VInt n)

parseB :: Parser Expr
parseB = (do string "True"
             return $ EConst (VBool True))
         +++
         (do string "False"
             return $ EConst (VBool False))

parseC :: Parser Expr
parseC = do e1 <- parseE
            spaces
            string "<"
            spaces
            e2 <- parseE
            return $ ELt e1 e2

runParse :: String -> [Expr]
runParse str = runParser program str

runEval :: Expr -> Either String Value
runEval expr = runIdentity (evalStateT (runReaderT (runExceptT (eval expr)) []) 0)

main :: IO ()
main = do s <- getContents
          let e = head $ runParse s
          print e
          case (runEval e) of
            Left  msg -> print msg
            Right val -> print val
          return ()
