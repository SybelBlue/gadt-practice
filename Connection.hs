{-# LANGUAGE TypeFamilies #-}
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

-- A function over types
type family Join (a :: ConnState) (b :: ConnState) 
                :: ConnState where
    Join 'Closed _ = 'Closed
    Join _ 'Closed = 'Closed
    Join 'Auth 'Auth = 'Auth 
    Join _ _ = 'Open

-- notice, this resolves
--   Connection Open -> Connection Auth -> Connection (Join Open Auth)
-- to
--   Connection Open -> Connection Auth -> Connection Open
join :: Connection a -> Connection b -> Connection (Join a b)
join ConnClosed _ = ConnClosed
join (ConnAuth s a) (ConnAuth _ _) = ConnAuth s a
join _ _ = undefined  -- etc
