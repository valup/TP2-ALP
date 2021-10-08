module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

----------------------------------------------
-- Seccón 2  
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------

conversion :: LamTerm -> Term
conversion = conv []
             where
                 conv a (LVar s)  = case elemIndex s a of
                                        Just n  -> Bound n
                                        Nothing -> Free (Global s)
                 conv a (App x y) = (conv a x) :@: (conv a y)
                 conv a (Abs s x) = let abs = s:a
                                    in Lam (conv abs x)

-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp (VLam f) v     = f v
vapp (VNeutral n) v = VNeutral (NApp n v)

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii
eval' (Free n)   (nEnv, _) = case lookup n nEnv of
                                Just v  -> v
                                Nothing -> VNeutral (NFree n)
eval' (t1 :@: t2) env      = let v1 = eval' t1 env
                                 v2 = eval' t2 env
                             in vapp v1 v2
eval' (Lam t) (nEnv, lEnv) = VLam (\v -> eval' t (nEnv, v:lEnv))
                            


--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote = undefined






