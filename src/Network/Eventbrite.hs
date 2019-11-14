{-# LANGUAGE OverloadedStrings #-}
module Network.Eventbrite
  ( cliMain
  , getAttendees
  , Pagination(..)
  , Attendee(..)
  , AttendeeCosts(..)
  , Currency(..)
  , Profile(..)
  )
where

import           System.Environment             ( lookupEnv )
import qualified Network.Wreq                  as W
import           Control.Lens                   ( (^.)
                                                , (.~)
                                                , (&)
                                                )
import           Data.Aeson                     ( FromJSON(..)
                                                , eitherDecode
                                                , withObject
                                                , (.:)
                                                , (.:?)
                                                )

import           Data.List                      ( sortOn )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.ByteString               as BS
                                         hiding ( pack, unpack )
import qualified Data.ByteString.Char8         as BS
{-
 - Very Useful Eventbrite attendees URL:
 - https://www.eventbriteapi.com/v3/events/event_id/attendees/
 -}


-- These types are incomplete, I'll add things when I need them
data Pagination = Pagination {
  paginationContinuation :: Maybe String,
  paginationHasMoreItems :: Bool
}
  deriving (Show)

instance FromJSON Pagination where
  parseJSON = withObject "Pagination"
    $ \v -> Pagination <$> v .:? "continuation" <*> v .: "has_more_items"

data Profile = Profile {
  profileName :: String,
  profileEmail :: String,
  profileLastName :: String
}
  deriving (Show)

instance FromJSON Profile where
  parseJSON = withObject "Profile"
    $ \v -> Profile <$> v .: "name" <*> v .: "email" <*> v .: "last_name"

data Currency = Currency {
  currencyCurrency :: Text,
  currencyValue :: Integer
}
  deriving (Show)

instance FromJSON Currency where
  parseJSON =
    withObject "Currency" $ \v -> Currency <$> v .: "currency" <*> v .: "value"

data AttendeeCosts = AttendeeCosts {
  costsBasePrice :: Currency,
  costsGross :: Currency
}
  deriving (Show)

instance FromJSON AttendeeCosts where
  parseJSON = withObject "AttendeeCosts"
    $ \v -> AttendeeCosts <$> v .: "base_price" <*> v .: "gross"

data Attendee = Attendee {
  attendeeId :: String,
  attendeeProfile :: Profile,
  attendeeCosts :: AttendeeCosts
}
  deriving (Show)


instance FromJSON Attendee where
  parseJSON = withObject "Attendee"
    $ \v -> Attendee <$> v .: "id" <*> v .: "profile" <*> v .: "costs"

data ListAttendeesResponse = ListAttendeesResponse {
  responsePagination :: Pagination,
  responseAttendees :: [Attendee]
}
  deriving (Show)

instance FromJSON ListAttendeesResponse where
  parseJSON = withObject "ListAttendeesResponse"
    $ \v -> ListAttendeesResponse <$> v .: "pagination" <*> v .: "attendees"


-- | Gets a list of attendees given a token and an event id
getAttendees :: String -> String -> Maybe String -> IO (Either String [Attendee])
getAttendees authToken event continuation = do
  response <- W.getWith
    options
    ("https://www.eventbriteapi.com/v3/events/" ++ event ++ "/attendees/")
  let body = response ^. W.responseBody
  case eitherDecode body of
    Left  err           -> return . Left $ show err
    Right attendeesList -> case paginationContinuation $ responsePagination attendeesList of
        Just token -> do
          putStrLn token
          otherAttendeesResponse <- getAttendees authToken event (Just token)
          case otherAttendeesResponse of
            Left err -> return . Left $ err
            Right otherAttendees -> return . Right $ responseAttendees attendeesList ++ otherAttendees 
        Nothing -> return . Right $ responseAttendees attendeesList
 where
  options = case continuation of
    Nothing ->  W.defaults
      &  W.header "Authorization"
      .~ [BS.intercalate " " ["Bearer", BS.pack authToken]]
    Just conttoken -> 
      W.defaults
      & W.param "continuation" .~ [T.pack conttoken]
      &  W.header "Authorization"
      .~ [BS.intercalate " " ["Bearer", BS.pack authToken]]


-- | My own boat party main method
cliMain :: IO ()
cliMain = do
  putStrLn "Getting Attendees for boat party"
  eventbriteToken <- lookupEnv "CSIT_EVENTBRITE_TOKEN"
  boatPartyId     <- lookupEnv "BOAT_PARTY_EVENT"
  case (eventbriteToken, boatPartyId) of
    (Just token, Just eventId) -> do
      response <- getAttendees token eventId Nothing
      case response of
        Left err -> putStrLn $ "Could not decode response " ++ show err
        Right attendeesList -> printAttendees attendeesList
    _ ->
      putStrLn
        "Missing environment variables CSIT_EVENTBRITE_TOKEN and BOAT_PARTY_EVENT"

printAttendees :: [Attendee] -> IO ()
printAttendees = mapM_ (\attendee -> do
    putStrLn . profileName . attendeeProfile $ attendee
    print . currencyValue . costsGross . attendeeCosts $ attendee)
  . sortOn (profileLastName . attendeeProfile)
