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


-- Part 1 - Type Inference
typeofM :: Env -> KULang -> (Maybe TypeKULang)
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
