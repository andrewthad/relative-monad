{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Monad.Relative
  ( RelativeMonad(..)
  , WrappedRelativeMonad(..)
  , (>>=:)
  ) where

class RelativeMonad m v | m -> v where
  rreturn :: v x -> m x
  rbind :: m x -> (v x -> m y) -> m y

(>>=:) :: RelativeMonad m v => m a -> (v a -> m b) -> m b
(>>=:) = rbind

data WrappedRelativeMonad m v a where
  WrappedRelativeMonadPure :: a -> WrappedRelativeMonad m v a
  WrappedRelativeMonadUnpure :: m a -> (v a -> WrappedRelativeMonad m v b) -> WrappedRelativeMonad m v b

bindWrappedRelativeMonad :: RelativeMonad m v => WrappedRelativeMonad m v a -> (a -> WrappedRelativeMonad m v b) -> WrappedRelativeMonad m v b
bindWrappedRelativeMonad (WrappedRelativeMonadPure x) g = g x
bindWrappedRelativeMonad (WrappedRelativeMonadUnpure m f) g = WrappedRelativeMonadUnpure m (\x -> f x >>= g)

instance Functor (WrappedRelativeMonad m v) where
  fmap f (WrappedRelativeMonadPure a) = WrappedRelativeMonadPure (f a)
  fmap f (WrappedRelativeMonadUnpure x y) = WrappedRelativeMonadUnpure x (\p -> fmap f (y p))

instance RelativeMonad m v => Applicative (WrappedRelativeMonad m v) where
  pure = WrappedRelativeMonadPure
  m1 <*> m2 = bindWrappedRelativeMonad m1 (\x -> bindWrappedRelativeMonad m2 (\y -> WrappedRelativeMonadPure (x y)))

instance RelativeMonad m v => Monad (WrappedRelativeMonad m v) where
  return = WrappedRelativeMonadPure 
  (>>=) = bindWrappedRelativeMonad

-- data RelativeFree v f a
--   = RelativePure (v a)
--   | RelativeLayer (f (RelativeFree v f a))
-- 
-- instance Functor f => RelativeMonad (RelativeFree v f) v where
--   rreturn = RelativePure
--   rbind (RelativePure a) f = f a
--   rbind (RelativeLayer m) f = RelativeLayer (fmap (>>=: f) m)

-- data Instruction v d
--   = InstructionCreate (Maybe Char) (v Char -> f d)
--   | Instruction
-- 
-- data Instruction s a where
--   InstructionCreate :: Create a -> Instruction s a
--   InstructionCase :: Vector (Generator s a) -> Cage s Int -> Instruction s a

-- data Free f a = Pure a | Free (f (Free f a))
-- 
-- instance Functor f => RelativeMonad (Free f) v where
--   rreturn = RelativePure
--   rbind (RelativePure a) f = f a
--   rbind (RelativeLayer m) f = RelativeLayer (fmap (>>=: f) m)
