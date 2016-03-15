{-# LANGUAGE OverloadedStrings #-}
module Language.JS.Monad where
import Control.Monad.State
import Language.JS.Syntax
import Haste (JSString, toJSString)
import qualified Haste.JSString as S
import Control.Monad.Cont

data JSEnv = JSEnv
  { jsLocals     :: [Decl]
  , jsFFI        :: [JSString]
  , jsParams     :: [Param]
  , jsArgs       :: [Typed Exp]
  , jsStmts      :: [Stmt]
  , jsFinalStmts :: [Stmt]
  , jsNextId     :: Int
  }

emptyEnv :: Int -> JSEnv
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

freshIdWith :: JSString -> JSGen Id
freshIdWith prefix = do
  env <- get
  case jsNextId env of
    ident -> do
      put env {jsNextId = succ ident}
      return (MkId (S.append prefix (toJSString ident)))

freshId :: JSGen Id
freshId = freshIdWith "v"

genIdFor :: VarId -> JSGen Id
genIdFor = pure . MkId

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

runJSGen :: forall a. Int -> JSGen () -> Func
runJSGen startid m =
    case evalState (runContT m (const get)) (emptyEnv startid) of
      env -> mkFunc env
  where
    mkFunc env = Func
      { funParams  = reverse $ jsParams env
      , funLocals  = reverse $ jsLocals env
      , funBody    = reverse (jsFinalStmts env ++ jsStmts env)
      }

evalJSGen :: Int -> ContT a (State JSEnv) a -> a
evalJSGen startid m = evalState (runContT m return) (emptyEnv startid)

-- TODO: inModule, inNewBlock, inNewFunction, wrapMain, collectArgs, collectDefinitions?, liftSharedLocals?
