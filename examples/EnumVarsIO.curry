------------------------------------------------------------------------------
-- Example for using the combination of a State and IO monad:
--
-- Given: an arithmetic expression containing numbers, variables (strings), etc
-- Task: replace all variables in an expression by unique numbers
------------------------------------------------------------------------------

import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State

-- The representation of arithmetic expressions is parameterized
-- over the representation of variables.
data Exp a = Num Int
           | Var a
           | BinOp String (Exp a) (Exp a)
 deriving Show

-- An example expression with strings as variables.
exp1 :: Exp String
exp1 = BinOp "+" (Var "x") (BinOp "*" (Var "y") (Var "x"))

-- The representation of the state used in the task.
-- The state consists of a current number for enumerating the variables
-- and a mapping from variable names to unique numbers (which will
-- be expanded during the transformation).
data TransInfo = TransInfo { currNr :: Int
                           , varMap :: [(String,Int)]
                           }

-- The initial state.
initState :: TransInfo
initState = TransInfo 0 []

-- The type of our actual state monad contains the transformation state
-- and integrates the I/O monad.
type TransStateIO a = StateT TransInfo IO a

-- Auxiliary operation: get a unique index for a given variable name.
-- Either return the existing index or create a fresh one and update
-- the state.
getVarIndex :: String -> TransStateIO Int
getVarIndex v = do
  ti <- get
  maybe (do let freshnr = currNr ti + 1
            put ti { currNr = freshnr, varMap = (v,freshnr) : varMap ti }
            return freshnr )
        return
        (lookup v (varMap ti))

-- The actual implementation of the transformation task performs
-- a traversal over the given expression in a monadic way.
-- Inside the traversal, I/O actions can be performed via `lift`.
enumExp :: Exp String -> TransStateIO (Exp Int)
enumExp exp = case exp of
  Num n -> return (Num n)
  Var v -> do vi <- getVarIndex v
              return $ Var vi
  BinOp o e1 e2 -> do
    lift $ putStrLn $ "Processing operator '" ++ o ++ "'..."
    te1 <- enumExp e1
    te2 <- enumExp e2
    return (BinOp o te1 te2)

-- We can embed the transformation into the IO monad via `evalStateT`.
transExp :: Exp String -> IO ()
transExp exp = do
  putStrLn $ "Original expression: " ++ show exp
  texp <- evalStateT (enumExp exp) initState
  putStrLn $ "Transformed expression: " ++ show texp

main1 :: IO ()
main1 = transExp exp1

-- If one also wants to return the final state after the transformation,
-- use `runStateT` instead of `evalStateT`.
runTransExp :: Exp String -> IO (Exp Int, TransInfo)
runTransExp exp = runStateT (enumExp exp) initState

main2 :: IO (Exp Int, TransInfo)
main2 = runTransExp exp1
