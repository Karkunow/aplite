module Language.JS.Monad where
import Control.Monad.State
import Language.JS.Syntax
import Language.C.Monad (MonadInclude (..))
import qualified Language.C.Syntax as C

data JSEnv = JSEnv
  { jsLocals     :: [Decl]
  , jsGlobals    :: [Decl]
  , jsParams     :: [Typed Id]
  , jsArgs       :: [Typed Exp]
  , jsStmts      :: [Stmt]
  , jsFinalStmts :: [Stmt]
  , jsNextId     :: Id
  }

emptyEnv :: JSEnv
emptyEnv = JSEnv
  { jsLocals = []
  , jsGlobals = []
  , jsParams = []
  , jsArgs = []
  , jsStmts = []
  , jsFinalStmts = []
  , jsNextId = MkId 0
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

addParam :: Typed Id -> JSGen ()
addParam p = modify $ \env -> env {jsParams = p : jsParams env}

runJSGen :: JSGen a -> (Func, a)
runJSGen m =
    case runState m emptyEnv of
      (x, env) -> (mkFunc env, x)
  where
    mkFunc env = Func
      { funParams = jsParams env
      , funLocals = jsLocals env
      , funBody   = reverse (jsStmts env) ++ reverse (jsFinalStmts env)
      }

runJSGen_ :: JSGen a -> Func
runJSGen_ = fst . runJSGen

-- TODO: inModule, inNewBlock, inNewFunction, wrapMain, collectArgs, collectDefinitions?, liftSharedLocals?
