{-# LANGUAGE GADTs,FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- Abstract Syntax Definitions
data KULang where
    Num :: Int -> KULang  
    Boolean :: Bool -> KULang
    Plus :: KULang -> KULang -> KULang 
    Minus :: KULang -> KULang -> KULang
    Mult :: KULang -> KULang -> KULang 
    Div :: KULang -> KULang -> KULang  
    Exp :: KULang -> KULang -> KULang
    If0 :: KULang -> KULang -> KULang -> KULang
    Id :: String -> KULang
    Lambda :: String -> TypeKULang -> KULang -> KULang 
    App :: KULang -> KULang -> KULang 
    And :: KULang -> KULang -> KULang   
    Or :: KULang -> KULang -> KULang  
    Leq :: KULang -> KULang -> KULang  
    IsZero :: KULang -> KULang  
    If :: KULang -> KULang -> KULang -> KULang  
    Bind :: String -> KULang -> KULang -> KULang
    Between :: KULang -> KULang -> KULang -> KULang
    Fix :: KULang -> KULang
    deriving (Show,Eq)

data TypeKULang where
    TNum :: TypeKULang
    TBool :: TypeKULang
    (:->:) :: TypeKULang -> TypeKULang -> TypeKULang
    TUnit :: TypeKULang
    deriving (Show,Eq)

data KULangVal where
    NumV :: Int -> KULangVal
    BooleanV :: Bool -> KULangVal
    ClosureV :: String -> KULang -> EnvVal -> KULangVal
    deriving (Show,Eq)

data KULangExt where
    NumX :: Int -> KULangExt
    PlusX :: KULangExt -> KULangExt -> KULangExt
    MinusX :: KULangExt -> KULangExt -> KULangExt
    MultX :: KULangExt -> KULangExt -> KULangExt
    DivX :: KULangExt -> KULangExt -> KULangExt
    ExpX :: KULangExt -> KULangExt -> KULangExt
    If0X :: KULangExt -> KULangExt -> KULangExt -> KULangExt
    LambdaX :: String -> KULangExt -> KULangExt
    AppX :: KULangExt -> KULangExt -> KULangExt
    BindX :: String -> KULangExt -> KULangExt -> KULangExt
    IdX :: String -> KULangExt
    deriving (Show,Eq)

-- Environment Definitions
type Env = [(String,KULang)]
type EnvVal = [(String,KULangVal)]
type Gamma = [(String, TypeKULang)]

-- Reader Definition
data Reader e a = Reader (e -> a)

-- Monad Definition
instance Monad (Reader e) where
 return x = Reader $ \e -> x 
 g >>= f = Reader $ \e -> runR (f (runR g e)) e 

 -- Applicative Definition
instance Applicative (Reader e) where
 pure x = Reader $ \e -> x
(Reader f) <*> (Reader g) = Reader $ \e -> (f e) (g e)

-- Functor Definition
instance Functor (Reader e) where
 fmap f (Reader g) = Reader $ \e -> (f . g) e

-- Fail Definition
instance MonadFail (Reader e) where
        fail = error "fail"

-- Helper Methods
runR :: Reader e a -> e -> a
runR (Reader f) e = f e 

ask :: Reader a a 
ask = Reader $ \e -> e

local :: (e->t) -> Reader t a -> Reader e a 
local f r = ask >>= \e -> return (runR r (f e))

useClosure :: String -> KULangVal -> EnvVal -> EnvVal -> EnvVal
useClosure i v e _ = (i,v):e 

subst :: String -> KULang -> KULang -> KULang
subst i v (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Mult l r) = (Mult (subst i v l) (subst i v r))
subst i v (Div l r) = (Div (subst i v l) (subst i v r))
subst i v (Boolean b) = (Boolean b)
subst i v (And l r) = (And (subst i v l) (subst i v r))
subst i v (Or l r) = (Or (subst i v l) (subst i v r))
subst i v (Leq l r) = (Leq (subst i v l) (subst i v r))
subst i v (IsZero l) = (IsZero (subst i v l))
subst i v (If p t e) = (If (subst i v p) (subst i v t) (subst i v e))
subst i v (Exp b e) = (Exp (subst i v b) (subst i v e))
subst i v (If0 c t e) = (If0 (subst i v c) (subst i v t) (subst i v e))
subst i v (Between l r e) = (Between (subst i v l) (subst i v r) (subst i v e))
subst i v (Id i') = if i==i' then v else (Id i')
subst i v (Bind i' v' b') = if i==i' then (Bind i' (subst i v v') b') else (Bind i' (subst i v v') (subst i v b'))
subst i v (Lambda i' t b) = if i==i' then (Lambda i' t b) else (Lambda i' t (subst i v b))
subst i v (App f a) = (App (subst i v f) (subst i v a))
subst i v (Fix f) = Fix (subst i v f)


-- Part 1 - Type Inference - Matt Latendresse
typeofM :: Gamma -> KULang -> (Maybe TypeKULang)
typeofM c (Num n) = if n>=0 then return TNum else Nothing
typeofM c (Boolean b) = return TBool
typeofM c (Plus l r) = do {TNum <- typeofM c l;
                        TNum <- typeofM c r; 
                        return TNum;} 
typeofM c (Minus l r) = do {TNum <- typeofM c l;
                        TNum <- typeofM c r; 
                        return TNum;} 
typeofM c (Mult l r) = do {TNum <- typeofM c l;
                        TNum <- typeofM c r; 
                        return TNum;} 
typeofM c (Div l r) = do {TNum <- typeofM c l;
                         TNum <- typeofM c r; 
                        return TNum;} 
typeofM c (Exp l r) = do{l' <- typeofM c l;
                        r' <- typeofM c r;
                        if (l' == TNum) && (r' == TNum) then Just TNum else Nothing}
typeofM c (And l r) = do {TBool <- typeofM c l;
                        TBool <- typeofM c r; 
                        return TBool;}
typeofM c (Or l r) = do {TBool <- typeofM c l;
                        TBool <- typeofM c r; 
                        return TBool;}
typeofM c (Leq l r) = do {TNum <- typeofM c l;
                        TNum <- typeofM c r; 
                        return TBool;}
typeofM c (IsZero l) = do {TNum <- typeofM c l; 
                        return TBool;}
typeofM cont (If0 c t e) = do {TNum <- typeofM cont c;
                           t' <- typeofM cont t;
                           e' <- typeofM cont e;
                           if (t' == e') then Just t' else Nothing}
typeofM cont (Between l c h) = do {TNum <- typeofM cont l;
                               TNum <- typeofM cont c;
                               TNum <- typeofM cont h;
                               return TBool;}  
typeofM cont (If c t e) = do {TBool <- typeofM cont c;
                        t' <- typeofM cont t;
                        e' <- typeofM cont e;
                        if (t' == e') then 
                            Just t';
                        else
                            Nothing} 
typeofM c (Bind i v b) = do { v' <- (typeofM c v);
                            typeofM ((i,v'):c) b;
                            }
typeofM c (Id i) = (lookup i c)
typeofM c (Lambda x t b) = do { tyB <- typeofM ((x,t):c) b ;
                                  return (t :->: tyB) }
typeofM cont (App x y) = do { tyXd :->: tyXr <- typeofM cont x ;
                             tyY <- typeofM cont y ;
                             if tyXd==tyY
                             then return tyXr
                             else Nothing }
typeofM c (Fix t) = do { (d :->: r) <- typeofM c t;
                         return r }
                         
                         
                         
                         
--Part2 - Junyi Zhao & Sam Jerguson

evalStat :: EnvVal -> KULang -> (Maybe KULangVal)
evalStat _ (Num n) = return (NumV n)
evalStat _ (Boolean b) = return (BooleanV b)
evalStat e (Plus l r) = do{(NumV l') <- (evalStat e l);
                              (NumV r') <- (evalStat e r);
                              return (NumV (l' + r'))}
evalStat e (Minus l r) = do{(NumV l') <- (evalStat e l);
                               (NumV r') <- (evalStat e r);
                               if(r' < l') then return (NumV (l' - r')) else Nothing}
evalStat e (Mult l r) = do{(NumV l') <- (evalStat e l);
                              (NumV r') <- (evalStat e r);
                              return (NumV (l' * r'))}
evalStat e (Div l r) = do{(NumV l') <- (evalStat e l);
                               (NumV r') <- (evalStat e r);
                               if(r' == 0) then Nothing else return (NumV (l' `div` r'))}
evalStat e (Exp l r) = do{(NumV l') <- (evalStat e l);
                               (NumV r') <- (evalStat e r);
                               return (NumV (l' ^ r'))}
--evalStat e (Lambda i b) = return (ClosureV i b e)
evalStat e (Lambda s t l) = return (ClosureV s l e) -- changed
evalStat e (App f a) = do {(ClosureV i b e) <- (evalStat e f);
                               a' <- (evalStat e a);
                               evalStat ((i,a'):e) b }
evalStat e (And l r) = do { (BooleanV l') <- evalStat e l;
                            (BooleanV r') <- evalStat e r;
                            return (BooleanV (l' && r')) }
evalStat e (Or l r) = do { (BooleanV l') <- evalStat e l;
                           (BooleanV r') <- evalStat e r;
                           return (BooleanV (l' || r')) }
evalStat e (Leq l r) = do { (NumV l') <- evalStat e l;
                            (NumV r') <- evalStat e r;
                            return (BooleanV (l' <= r')) }
evalStat e (IsZero l) = do { (NumV l') <- evalStat e l;
                             return (BooleanV (l' == 0)) }
evalStat e (If c t o) = do { (BooleanV c') <- evalStat e c;
                             if c' then (evalStat e t) else (evalStat e o) }
evalStat e (Bind i v b) = do { v' <- evalStat e v;
                               evalStat ((i, v'):e) b }
evalStat e (Between l c h) = do { (NumV l') <- evalStat e l;
                                  (NumV c') <- evalStat e c;
                                  (NumV h') <- evalStat e h;
                                  return (BooleanV (l' <= c' && c' <= h')) }
evalStat e (Id i) = lookup i e
evalStat e (If0 c t o) = do {(NumV c') <- (evalStat e c);
                                if c'==0 then (evalStat e t) else (evalStat e o) }
--end of part 2 by Junyi Zhao

--Part 3 - Sam Jerguson -- 
evalStat e (Fix f) = 
  do {
   (ClosureV i b j) <- evalStat e f;
   evalStat j (subst i (Fix (Lambda i (TNum :->: TNum) b)) b)
   }

-- --part 4 - Jarrod Grothusen
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


 -- Part 5 - Joe Murray
--Interpreter
interp :: Gamma -> EnvVal -> KULang -> Maybe KULangVal
interp gamma env expr = do
  ty <- typeofM gamma expr
  evalStat env expr
