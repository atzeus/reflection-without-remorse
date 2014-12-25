{-# LANGUAGE ExistentialQuantification,GADTs,Rank2Types #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Operational.Reflectable
-- Copyright   :  (c) Atze van der Ploeg 2014
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
-- An operational monad that supports alternating between building and observing.
-- It supports all operations ('>>=', 'return', 'fromView' and 'toView') in worst case constant time.
--
-- See the paper Reflection without Remorse: Revealing a hidden sequence to speed up Monadic Reflection, Atze van der Ploeg and Oleg Kiselyov, Haskell Symposium 2014
-- for more details.
--
-- Paper: <http://homepages.cwi.nl/~ploeg/zseq.pdf>
-- Talk : <http://www.youtube.com/watch?v=_XoI65Rxmss>
-----------------------------------------------------------------------------

module Control.Monad.Operational.Reflectable(Program,ProgramView(..), fromView, toView) where

import Data.TASequence.FastCatQueue
import Control.Monad
import Control.Applicative

newtype TermMCont r a b = TC (a -> Program r b)
type TermCExp r a b = FastTCQueue (TermMCont r) a b

data ProgramView r a where
  Bind   :: r w -> (w -> Program r a) -> ProgramView r a
  Return :: a -> ProgramView r a

data Program r a = forall x. Program (ProgramView r x) (TermCExp r x a)

fromView :: ProgramView r a -> Program r a
fromView r = Program r tempty

toView :: Program r a -> ProgramView r a
toView (Program x s) = case x of
  Return a -> case tviewl s of 
             TAEmptyL -> Return a
             TC h :< t  -> toView $ (h a) <.|| t
  Bind t f -> Bind t (\x -> f x <.|| s) 
  where (<.||) :: Program r a -> TermCExp r a b -> Program r b
        (Program x l) <.|| r = Program x (l >< r)

instance Monad (Program r) where
  return = fromView . Return
  (Program t s) >>= f = Program t (s |> TC f)

instr :: r x -> Program r x
instr r = fromView $ Bind r return

interpretWithMonad :: Monad m => (forall a. r a -> m a) -> Program r b -> m b
interpretWithMonad f = loop where 
  loop m = case toView m of
       Return x -> return x
       Bind i c -> f i >>= loop . c
 


instance Functor (Program r) where
  fmap = liftM

instance Applicative (Program r) where
  pure = return
  (<*>) = ap
