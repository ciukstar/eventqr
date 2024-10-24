{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoEn (fillDemoEn) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString as BS

import Database.Persist (PersistStoreWrite (insert, insert_))
import Database.Persist.SqlBackend (SqlBackend)

import Model
    ( User (User, userEmail, userPassword, userAdmin, userName)
    , UserPhoto
      ( UserPhoto, userPhotoUser, userPhotoMime, userPhotoAttribution
      , userPhotoPhoto
      )
    , Card (Card, cardUser, cardIssued, cardQr)
    , Event (Event, eventTime, eventName, eventDescr)
    , Subscriber (Subscriber, subscriberEvent, subscriberCard, subscriberRegDate)
    )

import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)
import Data.Time.Calendar (addDays)
import Data.Time.Clock (getCurrentTime, addUTCTime)


fillDemoEn :: MonadIO m => ReaderT SqlBackend m ()
fillDemoEn = do

    now <- liftIO getCurrentTime

    let dayLong = 24 * 60 * 60
    
    let freepik = [shamlet|
                          Designed by #
                          <a href="https://www.freepik.com/" target=_blank>
                            Freepik
                          |]

    pass1 <- liftIO $ saltPass "marylopez"
    uid1 <- insert $ User { userEmail = "marylopez@xmail.edu"
                          , userPassword = Just pass1
                          , userName = Just "Mary Lopez"
                          , userAdmin = True
                          }

    liftIO (BS.readFile "demo/user_1.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid1
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass2 <- liftIO $ saltPass "jjohnson"
    uid2 <- insert $ User { userEmail = "jjohnson@xmail.edu"
                          , userPassword = Just pass2
                          , userName = Just "John Johnson"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_2.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid2
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass3 <- liftIO $ saltPass "jmaulsby"
    uid3 <- insert $ User { userEmail = "jmaulsby@xmail.edu"
                          , userPassword = Just pass3
                          , userName = Just "Julian Maulsby"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_3.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid3
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass4 <- liftIO $ saltPass "vschoen"
    uid4 <- insert $ User { userEmail = "vschoen@xmail.edu"
                          , userPassword = Just pass4
                          , userName = Just "Valentina Schoen"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_4.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid4
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    cid1 <- insert $ Card { cardUser = uid1
                          , cardIssued = addUTCTime ((-30) * dayLong) now
                          , cardQr = ""
                          }

    cid2 <- insert $ Card { cardUser = uid2
                          , cardIssued = addUTCTime ((-31) * dayLong) now
                          , cardQr = ""
                          }

    cid3 <- insert $ Card { cardUser = uid3
                          , cardIssued = addUTCTime ((-32) * dayLong) now
                          , cardQr = ""
                          }

    cid4 <- insert $ Card { cardUser = uid4
                          , cardIssued = addUTCTime ((-33) * dayLong) now
                          , cardQr = ""
                          }

    eid1 <- insert $ Event { eventTime = addUTCTime (1 * dayLong) now
                           , eventName = "Private party"
                           , eventDescr = "Only Disco"
                           }

    eid2 <- insert $ Event { eventTime = addUTCTime (2 * dayLong) now
                           , eventName = "Team-building event"
                           , eventDescr = "Team-building event, then Disco"
                           }

    eid3 <- insert $ Event { eventTime = addUTCTime (3 * dayLong) now
                           , eventName = "Shareholder meeting"
                           , eventDescr = "Shareholder meeting, then Disco"
                           }

    eid4 <- insert $ Event { eventTime = addUTCTime (4 * dayLong) now
                           , eventName = "Board meeting"
                           , eventDescr = "Board meeting, then Disco"
                           }

    insert_ $ Subscriber { subscriberEvent = eid1
                         , subscriberCard = cid1
                         , subscriberRegDate = now
                         }

    insert_ $ Subscriber { subscriberEvent = eid1
                         , subscriberCard = cid2
                         , subscriberRegDate = now
                         }

    insert_ $ Subscriber { subscriberEvent = eid2
                         , subscriberCard = cid3
                         , subscriberRegDate = now
                         }

    insert_ $ Subscriber { subscriberEvent = eid2
                         , subscriberCard = cid4
                         , subscriberRegDate = now
                         }

    insert_ $ Subscriber { subscriberEvent = eid3
                         , subscriberCard = cid1
                         , subscriberRegDate = now
                         }

    insert_ $ Subscriber { subscriberEvent = eid3
                         , subscriberCard = cid2
                         , subscriberRegDate = now
                         }

    insert_ $ Subscriber { subscriberEvent = eid4
                         , subscriberCard = cid1
                         , subscriberRegDate = now
                         }

    insert_ $ Subscriber { subscriberEvent = eid4
                         , subscriberCard = cid2
                         , subscriberRegDate = now
                         }

    insert_ $ Subscriber { subscriberEvent = eid4
                         , subscriberCard = cid3
                         , subscriberRegDate = now
                         }
    
    return ()
