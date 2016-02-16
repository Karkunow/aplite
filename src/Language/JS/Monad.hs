module Language.JS.Monad where
import Control.Monad.State
import Language.JS.Syntax
import Language.C.Monad (MonadInclude (..))
import qualified Language.C.Syntax as C
import qualified Data.Map.Strict as M

data Block = Block
  { blockDecls :: [Decl]
  , blockStmts :: [Stmt]
  }

data JSEnv = JSEnv
  { jsLocals     :: [Decl]
  , jsGlobals    :: [Decl]
  , jsParams     :: [Typed Id]
  , jsArgs       :: [Typed Exp]
  , jsStmts      :: [Stmt]
  , jsFinalStmts :: [Stmt]
  , jsNextId     :: Id
  , jsVarEnv     :: M.Map String Id
  }

type JSGen = State JSEnv

instance MonadInclude JSGen where
  addInclude _ = return ()

addStm :: Stmt -> JSGen ()
addStm s = modify $ \env -> env {jsStmts = s : jsStmts env}

addLocal :: Type -> Id -> Maybe (Typed Exp) -> JSGen ()
addLocal t n i =
  modify $ \env -> env {jsLocals = Decl t n (fmap untyped i) : jsLocals env}

addGlobal :: Type -> Id -> Maybe (Typed Exp) -> JSGen ()
addGlobal t n i =
  modify $ \env -> env {jsLocals = Decl t n (fmap untyped i) : jsGlobals env}

freshId :: JSGen Id
freshId = do
  env <- get
  case jsNextId env of
    ident -> do
      put env {jsNextId = succ ident}
      return ident

genIdFor :: String -> JSGen Id
genIdFor name = do
  env <- jsVarEnv `fmap` get
  case M.lookup name env of
    Just ident -> pure ident
    _          -> do
      ident <- freshId
      state $ \st -> (ident, st {jsVarEnv = M.insert name ident env})

addFinalStm :: Stmt -> JSGen ()
addFinalStm s = modify $ \env -> env {jsFinalStmts = s : jsFinalStmts env}

inFunction :: JSGen a -> JSGen (a, [Param], Block)
inFunction m = do
  oldparams <- state $ \env -> (jsParams env, env {jsParams = []})
  (x, block) <- inBlock m
  params <- state $ \env -> (jsParams env, env {jsParams = oldparams})
  return (x, reverse params, block)

inBlock :: JSGen a -> JSGen (a, Block)
inBlock m = do
  oldenv <- get
  put oldenv
    { jsLocals     = []
    , jsStmts      = []
    , jsFinalStmts = []
    }
  x <- m
  env <- get
  put env
    { jsLocals     = jsLocals oldenv
    , jsStmts      = jsStmts oldenv
    , jsFinalStmts = jsFinalStmts oldenv
    }
  return (x, Block (jsLocals env) (reverse (jsStmts env ++ jsFinalStmts env)))

addArg :: Typed Exp -> JSGen ()
addArg arg = modify $ \env -> env {jsArgs = arg : jsArgs env}

addParam :: Typed Id -> JSGen ()
addParam p = modify $ \env -> env {jsParams = p : jsParams env}

-- TODO: inModule, inNewBlock, inNewFunction, wrapMain, collectArgs, collectDefinitions?, liftSharedLocals?
