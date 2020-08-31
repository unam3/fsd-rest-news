{-# LANGUAGE QuasiQuotes        #-}

module HasqlStatements (
    createUser
    ) where

import Data.Text (Text)
import qualified Hasql.TH as TH
import Hasql.Statement (Statement(..))

createUser :: Statement (Text, Text, Bool) ()
createUser = 
    [TH.singletonStatement|
        insert into users (name, surname, is_admin) values
            (
                $1 :: text,
                $2 :: text,
                $3 :: bool
                )
        |]
