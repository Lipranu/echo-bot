{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Infrastructure.RequesterSpec ( spec ) where

-- IMPORTS ---------------------------------------------------------------------

import Internal
import Infrastructure.Requester

import Control.Monad.Reader         ( Reader, asks, runReader )
import Control.Monad.IO.Class       ( liftIO )
import Data.Text                    ( Text )
import Data.Text.Encoding           ( decodeUtf8 )
import GHC.Generics                 ( Generic )
import Network.HTTP.Client.Extended ( HttpException (..), createCookieJar )
import Network.HTTP.Client.Internal ( ResponseClose (..), Response (..) )
import Network.HTTP.Types           ( mkStatus, http11 )
import Test.Hspec                   ( Spec, describe, it, shouldBe )

import qualified Data.Aeson.Extended          as Aeson
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.Text                    as Text
import qualified Network.HTTP.Client.Extended as HTTP

-- TYPES AND INSTANCES ---------------------------------------------------------

type App = Reader Env

data Env = Env
  { envRequester :: Requester App
  , envResult    :: Either HttpException (Response BSL.ByteString)
  }

instance MonadRequester App where
  requester _ _ = asks envResult

instance Has (Requester App) Env where
  getter = envRequester

data TestBody = TestBody
  { tbFieldText :: Text
  , tbFieldNum  :: Integer
  } deriving (Generic, Show, Eq)

instance Aeson.FromJSON TestBody where
  parseJSON = Aeson.parseJsonDrop

instance Aeson.ToJSON TestBody where
  toJSON = Aeson.toJsonDrop

instance ToRequest TestBody where
  toRequest b = HTTP.defaultRequest
    { HTTP.requestBody = HTTP.RequestBodyLBS $ Aeson.encode b }

-- FUNCTIONS -------------------------------------------------------------------

testBody :: TestBody
testBody = TestBody "text" 1

testBodyRaw, testBodyFail :: BSL.ByteString
testBodyRaw = "{\"field_text\":\"text\",\"field_num\":1}"
testBodyFail = "{\"field_num\":1}"

failedResponse :: HttpException
failedResponse = HTTP.InvalidUrlException "test" "test"

succeededResponse :: Response BSL.ByteString
succeededResponse = Response
  { responseStatus = mkStatus 200 "success"
  , responseVersion = http11
  , responseHeaders = []
  , responseBody = testBodyRaw
  , responseCookieJar = createCookieJar []
  , responseClose' = ResponseClose (return () :: IO ())
  }

requestError, decodeError :: Result a
requestError = RequestError $ Text.pack $ show $ failedResponse
decodeError  = DecodeError err bs
  where bs   = decodeUtf8 $ BSL.toStrict testBodyFail
        err  = "Error in $: parsing Infrastructure.RequesterSpec.\
               \TestBody(TestBody) failed, key \"field_text\" not found"

-- TESTS -----------------------------------------------------------------------

requestSpec :: Spec
requestSpec = describe "request" $ do
  it "should return RequestError if request fails" $ do
    manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
    let req = mkRequester manager
        env = Env req $ Left failedResponse
        test = runReader (request testBody) env
    test `shouldBe` requestError

  it "should return bytestring body on successful request" $ do
    manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
    let req = mkRequester manager
        env = Env req $ Right succeededResponse
        test = runReader (request testBody) env
    test `shouldBe` Result testBodyRaw

decodeSpec :: Spec
decodeSpec = describe "decode" $ do
  it "should return DecodeError if decode fails" $
    let test = decode @TestBody $ Result testBodyFail
     in test `shouldBe` decodeError

  it "should return annotated type on successful decoding" $
    let test = decode @TestBody $ Result testBodyRaw
     in test `shouldBe` Result testBody

  it "should rethrow RequestError" $
    let test = decode @TestBody requestError
     in test `shouldBe` requestError

  it "should rethrow decodeError" $
    let test = decode @TestBody decodeError
     in test `shouldBe` decodeError

spec :: Spec
spec = do
  requestSpec
  decodeSpec
