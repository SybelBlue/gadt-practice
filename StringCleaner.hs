{-# LANGUAGE GADTs #-}
module StringCleaner where

-- can be used to do the same thing as a regular ADT
data InputString where
    Raw   :: String -> InputString
    Escpd :: String -> InputString

-- manage values so that escaping only happens once
escape :: InputString -> InputString
escape (Escpd x) = Escpd x
escape (Raw x) = computeEscaped x


computeEscaped = Escpd . map id -- a long computing function here