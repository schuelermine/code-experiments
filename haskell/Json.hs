{-# LANGUAGE BlockArguments #-}

import Test.QuickCheck
import qualified Data.Map.Strict as Map

data Json = JsonNull | JsonString String | JsonNumber Double | JsonBool Bool | JsonArray [Json] | JsonObject (Map.Map String Json)

instance Arbitrary Json where
    arbitrary = sized \n -> resize (div n 3) gen
        where gen = sized \size ->
                let smaller = resize (size - 1)
                in oneof $ [ return JsonNull,
                            JsonString <$> arbitrary,
                            JsonNumber <$> arbitrary,
                            JsonBool <$> arbitrary ] ++ if size > 1 then [
                            JsonArray <$> smaller arbitrary,
                            JsonObject <$> smaller arbitrary ] else []
