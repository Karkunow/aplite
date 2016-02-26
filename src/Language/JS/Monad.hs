module Language.JS.Monad where
import Control.Monad.State
import Language.JS.Syntax
import Haste (JSString)
import Control.Monad.Cont

data JSEnv = JSEnv
  { jsLocals     :: [Decl]
  , jsFFI        :: [JSString]
  , jsParams     :: [Param]
  , jsArgs       :: [Typed Exp]
  , jsStmts      :: [Stmt]
  , jsFinalStmts :: [Stmt]
  , jsNextId     :: Integer
  }

emptyEnv :: Integer -> JSEnv
emptyEnv startid = JSEnv
  { jsLocals = []
  , jsFFI = []
  , jsParams = []
  , jsArgs = []
  , jsStmts = []
  , jsFinalStmts = []
  , jsNextId = startid
  }

type JSGen = ContT JSEnv (State JSEnv)

addStm :: Stmt -> JSGen ()
addStm s = modify $ \env -> env {jsStmts = s : jsStmts env}

addLocal :: Type -> Id -> JSGen ()
addLocal t n =
  modify $ \env -> env {jsLocals = Decl t n : jsLocals env}

addImport :: JSString -> JSGen ()
addImport f = modify $ \env -> env {jsFFI = f : jsFFI env}

freshId :: JSGen Id
freshId = do
  env <- get
  case jsNextId env of
    ident -> do
      put env {jsNextId = succ ident}
      return (MkId ident)

genIdFor :: String -> JSGen Id
genIdFor = pure . MkId . read

addFinalStm :: Stmt -> JSGen ()
addFinalStm s = modify $ \env -> env {jsFinalStmts = s : jsFinalStmts env}

-- | Generate code in a new function.
inFunction :: JSGen a -> JSGen (a, [Param], Stmt)
inFunction m = do
  oldparams <- state $ \env -> (jsParams env, env {jsParams = []})
  (x, block) <- inBlock m
  params <- state $ \env -> (jsParams env, env {jsParams = oldparams})
  return (x, reverse params, block)

-- | Generate code in a C block.
inBlock :: JSGen a -> JSGen (a, Stmt)
inBlock m = do
  oldenv <- get
  put oldenv
    { jsStmts      = []
    , jsFinalStmts = []
    }
  x <- m
  env <- get
  put env
    { jsStmts      = jsStmts oldenv
    , jsFinalStmts = jsFinalStmts oldenv
    }
  return (x, Block (reverse (jsStmts env ++ jsFinalStmts env)))

inBlock_ :: JSGen a -> JSGen Stmt
inBlock_ = fmap snd . inBlock

addArg :: Typed Exp -> JSGen ()
addArg arg = modify $ \env -> env {jsArgs = arg : jsArgs env}

addParam :: Param -> JSGen ()
addParam p = modify $ \env -> env {jsParams = p : jsParams env}

runJSGen :: forall a. ReturnValue a => Integer -> JSGen a -> Func
runJSGen startid m =
    case evalState (runContT m' (const get)) (emptyEnv startid) of
      env -> mkFunc env
  where
    m' = do
      x <- m
      case returnStmt x of
        Just stm -> stm >>= addFinalStm
        _        -> return ()

    mkFunc env = Func
      { funParams = reverse $ jsParams env
      , funLocals = reverse $ jsLocals env
      , funBody   = reverse (jsFinalStmts env ++ jsStmts env)
      }

evalJSGen :: Integer -> ContT a (State JSEnv) a -> a
evalJSGen startid m = evalState (runContT m return) (emptyEnv startid)

class ReturnValue a where
  returnStmt :: a -> Maybe (JSGen Stmt)

instance ReturnValue () where
  returnStmt _ = Nothing

-- TODO: inModule, inNewBlock, inNewFunction, wrapMain, collectArgs, collectDefinitions?, liftSharedLocals?
