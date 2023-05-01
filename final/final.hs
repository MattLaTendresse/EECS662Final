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
    Lambda :: String -> KULang -> KULang 
    App :: KULang -> KULang -> KULang 
    And :: KULang -> KULang -> KULang   
    Or :: KULang -> KULang -> KULang  
    Leq :: KULang -> KULang -> KULang  
    IsZero :: KULang -> KULang  
    If :: KULang -> KULang -> KULang -> KULang  
    Bind :: String -> KULang -> KULang -> KULang
    Between :: KULang -> KULang -> KULang -> KULang
    deriving (Show,Eq)

data KULangVal where
    NumV :: Int -> KULangVal
    BooleanV :: Bool -> KULangVal
    ClosureV :: String -> KULang -> EnvVal -> KULangVal
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
