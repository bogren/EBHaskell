{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.ByteString.Lazy (toChunks)
import Data.List
import Data.Maybe
import Data.Typeable (Typeable)
import Data.Text.Encoding
import qualified Data.Map as M

import Network.HTTP.Types (renderSimpleQuery, parseSimpleQuery)
import qualified Network.HTTP.Types as HT
import Network.HTTP.Conduit

import Control.Exception (Exception, throwIO)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

--
-- Static Strings
--

myclientsecret = "<your id>"
myclientid = "<your id>"
myOAuthEP = "https://instagram.com/oauth/authorize"
myOauthAccessTokenEP = "https://api.instagram.com/oauth/access_token"
myapiurl = "https://api.instagram.com/v1/"

--
-- Custom Data Types
--
data OAuth2 = OAuth2 { oauthClientId :: BS.ByteString
                     , oauthClientSecret :: BS.ByteString
                     , oauthOAuthorizeEndpoint :: BS.ByteString
                     , oauthAccessTokenEndpoint :: BS.ByteString
                     , oauthCallback :: Maybe BS.ByteString
                     , oauthAccessToken :: Maybe BS.ByteString
                     } deriving (Show, Eq)

data OAuthException = OAuthException String
                      deriving (Show, Eq, Typeable)
instance Exception OAuthException

data AccessToken = AccessToken { accessToken :: BS.ByteString } deriving (Show)
instance FromJSON AccessToken where
    parseJSON (Object v) = AccessToken <$> fmap encodeUtf8 (v .: "access_token")
    parseJSON _          = mzero

newtype RecentPosts = RecentPosts {recentPosts :: [RecentPost]} deriving (Show)
instance FromJSON RecentPosts where
  parseJSON (Object o) = RecentPosts <$> o .: "data"
  parseJSON _          = mzero

data RecentPost = RecentPost { media_id :: BS.ByteString } deriving (Show)
instance FromJSON RecentPost where
      parseJSON (Object o) = RecentPost <$> fmap encodeUtf8 ( o .: "id")
      parseJSON _          = mzero

data IGUser = IGUser { username :: BS.ByteString
                     , media_count :: Int
                     } deriving (Show)
instance FromJSON IGUser where
  parseJSON (Object o) = IGUser <$>
                         fmap encodeUtf8 ((o .: "data") >>= (.: "username")) <*>
                        (((o .: "data") >>= (.: "counts")) >>= (.: "media"))
  parseJSON _          = mzero

newOAuth2 :: OAuth2
newOAuth2 = OAuth2 { oauthClientId = error " "
                   , oauthClientSecret = error " "
                   , oauthOAuthorizeEndpoint = error " "
                   , oauthAccessTokenEndpoint = error " "
                   , oauthCallback = Nothing
                   , oauthAccessToken = Nothing
                   }

request req = (withManager . httpLbs) (req { checkStatus = \_ _ _ -> Nothing } )

(?:) :: (a, Maybe b) -> [(a, b)] -> [(a, b)]
infixr 5 ?:
(a, Just b)  ?: xs = (a, b):xs
(_, Nothing) ?: xs = xs


--
-- API Calls
--
authorizationUrl :: OAuth2 -> BS.ByteString
authorizationUrl oa = (oauthOAuthorizeEndpoint oa) `BS.append` queryString
              where queryString = renderSimpleQuery True query
                    query = foldr step [] [ ("client_id", Just $ oauthClientId oa)
                                          , ("response_type", Just "code")
                                          , ("redirect_uri", oauthCallback oa)]

getAccessToken' :: OAuth2 -> BS.ByteString -> Maybe BS.ByteString -> IO BSL.ByteString
getAccessToken' oa code grant_type = do
    rsp <- request req
    if (HT.statusCode . responseStatus) rsp == 200
        then return $ responseBody rsp
        else throwIO . OAuthException $ "Gaining access_token failed: " ++ BSL.unpack (responseBody rsp)
  where
    req = fromJust $ parseUrl url
    url = BS.unpack $ oauthOAuthorizeEndpoint oa `BS.append` queryString
    queryString = renderSimpleQuery True query
    query = foldr step [] [ ("client_id", Just $ oauthClientId oa)
                          , ("client_secret", Just $ oauthClientSecret oa)
                          , ("code", Just code)
                          , ("redirect_uri", oauthCallback oa)
                          , ("grant_type", grant_type) ]


postAccessToken' :: OAuth2 -> BS.ByteString -> Maybe BS.ByteString -> IO BSL.ByteString
postAccessToken' oa code grant_type = do
    rsp <- request req
    if (HT.statusCode . responseStatus) rsp == 200
        then return $ responseBody rsp
        else throwIO . OAuthException $ "Gaining access_token failed: " ++ BSL.unpack (responseBody rsp)
  where
    Just req = urlEncodedBody query <$> parseUrl url
    url = BS.unpack $ oauthAccessTokenEndpoint oa
    query =
      ("redirect_uri", oauthCallback oa) ?:
      ("grant_type", grant_type) ?:
      [ ("client_id", oauthClientId oa)
      , ("client_secret", oauthClientSecret oa)
      , ("code", code)
      ]

getRecentPost' :: OAuth2 -> IO BSL.ByteString
getRecentPost' oa = do
    rsp <- request (signRequest oa req)
    if (HT.statusCode . responseStatus) rsp == 200
        then return $ responseBody rsp
        else throwIO . OAuthException $ "Fetch recent media failed with code : " ++ (show rsp)
    where
        Just req =  parseUrl url --urlEncodedBody query <$> parseUrl url
        url = "https://api.instagram.com/v1/users/self/media/recent?count=3"
        --query = [("count", (BS.pack "3"))] this should work!!11!

getUser' :: OAuth2 -> String -> IO BSL.ByteString
getUser' oa user = do
    rsp <- request (signRequest oa req)
    if (HT.statusCode . responseStatus) rsp == 200
        then return $ responseBody rsp
        else throwIO . OAuthException $ "Fetch user failed with code : " ++ (show rsp)
    where
        Just req = parseUrl url
        url = myapiurl ++ "users/" ++ user

--
-- Helper Method - Part of a Haskell Guide 
--
step :: (a, Maybe b) -> [(a, b)] -> [(a, b)]
step (a, Just b) xs = (a, b):xs
step _           xs = xs

--
-- Calls
--
getAccessToken :: OAuth2 -> BS.ByteString -> Maybe BS.ByteString -> IO (Maybe AccessToken)
getAccessToken oa code grant_type = decode <$> getAccessToken' oa code grant_type

postAccessToken :: OAuth2 -> BS.ByteString -> Maybe BS.ByteString -> IO (Maybe AccessToken)
postAccessToken oa code grant_type = decode <$> postAccessToken' oa code grant_type

getRecentPost :: OAuth2 -> IO (Maybe RecentPosts)
getRecentPost oa = decode <$> getRecentPost' oa

getUser :: OAuth2 -> String -> IO (Maybe IGUser)
getUser oa user = decode <$> getUser' oa user

signRequest :: OAuth2 -> Request -> Request
signRequest oa req = req { queryString = (renderSimpleQuery False newQuery) }
   where
        newQuery = case oauthAccessToken oa of
            Just at -> insert ("access_token", at) oldQuery
            _       -> insert ("client_id", oauthClientId oa) . insert ("client_secret", oauthClientSecret oa) $ oldQuery
        oldQuery = parseSimpleQuery (queryString req)