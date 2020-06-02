module Context where
import ParseExpressionVar

type Context = ([Int], [Bool], [(String, Int)], [(String, Bool)])
type Bindings = ([(String, Int)], [(String, Bool)])

testContext :: Context
testContext = ([1..2], [True, False], [], [])

emptyBindings :: Bindings
emptyBindings = ([], [])

genBindings :: Expr -> Context -> [Bindings]
genBindings (EInt (EIntVar v)) (is,_,_,_) = [([(v, i)], []) | i <- is]
genBindings (EBool (EBoolVar v)) (_,bs,_,_) = [([], [(v, b)]) | b <- bs]

generateBindings :: [Expr] -> Context -> [Bindings]
generateBindings [] _ = [emptyBindings]
generateBindings (e:es) ctx = addBindings <$> (genBindings e ctx) <*> generateBindings es ctx

getIntVarValue :: Context -> String -> Maybe Int
getIntVarValue (_, _, ivm, _) v = lookup v ivm

intInContext :: Int -> Context -> Bool
intInContext n (ns, _, _ , _) = elem n ns

getBoolVarValue :: Context -> String -> Maybe Bool
getBoolVarValue (_, _, _, bvm) v = lookup v bvm

boolInContext :: Bool -> Context -> Bool
boolInContext b (_, bs, _ , _) = elem b bs

addCtxBindings :: Context -> Bindings -> Context
addCtxBindings (ns, bs, ivm, bvm) (nivm, nbvm) = (ns, bs, ivm ++ nivm, bvm ++ nbvm)

addCtxIntBinding :: (String, Int) -> Context -> Context
addCtxIntBinding (k, v) (ns, bs, ivm, bvm) = (ns, bs, (k, v):ivm, bvm)

addCtxBoolBinding :: (String, Bool) -> Context -> Context
addCtxBoolBinding (k, v) (ns, bs, ivm, bvm) = (ns, bs, ivm, (k, v):bvm)

addIntBindings :: (String, Int) -> Bindings -> Bindings
addIntBindings (k, v) (ivm, bvm) = ((k, v):ivm, bvm)

addBoolBindings :: (String, Bool) -> Bindings -> Bindings
addBoolBindings (k, v) (ivm, bvm) = (ivm, (k, v):bvm)

addBindings :: Bindings -> Bindings -> Bindings
addBindings (ivm, bvm) (nivm, nbvm) = (ivm ++ nivm, bvm ++ nbvm)

extractIntBinding :: String -> Bindings -> Int
extractIntBinding s (ivm, _) = case lookup s ivm of
                               (Just v) -> v
                               _ -> error "Name not found in integer bindings"

extractBoolBinding :: String -> Bindings -> Bool
extractBoolBinding s (_, bvm) = case lookup s bvm of
                                (Just v) -> v
                                _ -> error "Name not found in boolean bindings"