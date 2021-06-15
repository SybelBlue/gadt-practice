{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module List where

import GHC.Types (Nat)

data List a (n :: Nat) where
    Cons :: a -> List a x -> List a ()
    Nil  ::                List a 0
