{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Client where

-- import Data.Aeson
-- import Data.Proxy
-- import GHC.Generics
-- import Network.HTTP.Client (newManager, defaultManagerSettings)
-- import Servant.API
-- import Servant.Client
import Api.File as F

getFile :: String -> IO ()
getFile path = do
    F.query (getFile' path)
