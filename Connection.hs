{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds, GADTs #-}
module Connection where

type Socket = Int
type AuthToken = String

data ConnState = Closed | Open | Auth

data Connection (s :: ConnState) where
    ConnClosed ::                        Connection 'Closed
    ConnOpen   :: Socket              -> Connection 'Open
    ConnAuth   :: Socket -> AuthToken -> Connection 'Auth

-- expressing refined type signatures
auth :: Connection 'Open -> Maybe (Connection 'Auth)
auth (ConnOpen s) = if s > 0 then Just (ConnAuth s "yeahhhh") else Nothing

-- expressiong classes of refinement
class OpenOrAuth (s :: ConnState)

instance OpenOrAuth 'Open
instance OpenOrAuth 'Auth

-- expressing across a class of connections
close :: OpenOrAuth s => Connection s -> Connection 'Closed
close _ = ConnClosed
