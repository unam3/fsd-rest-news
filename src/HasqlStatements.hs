{-# LANGUAGE QuasiQuotes        #-}

module HasqlStatements (
    createUser,
    deleteUser,
    getUser
    ) where

import Data.Int (Int16)
import Data.Text (Text)
import qualified Hasql.TH as TH
import Hasql.Statement (Statement(..))

createUser :: Statement (Text, Text, Bool) ()
createUser = 
    [TH.singletonStatement|
        insert
        into users (name, surname, is_admin)
            values (
                $1 :: text,
                $2 :: text,
                $3 :: bool
                )
        |]

deleteUser :: Statement (Int16) ()
deleteUser = 
    [TH.singletonStatement|
        delete
        from users
        where user_id = $1 :: int2
        |]

getUser :: Statement (Int16) (Text, Text, Bool)
getUser = 
    [TH.singletonStatement|
        select name :: text, surname :: text, is_admin :: bool
        from users
        where user_id = $1 :: int2
        |]
