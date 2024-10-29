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
    , Attendee (Attendee, attendeeEvent, attendeeCard, attendeeRegDate), Info (Info, infoCard, infoName, infoValue)
    )

import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)
import Data.Time.Clock (getCurrentTime, addUTCTime)


fillDemoEn :: MonadIO m => ReaderT SqlBackend m ()
fillDemoEn = do

    now <- liftIO getCurrentTime

    let hour = 60 * 60
    let day = 24 * hour
    
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
                          , cardIssued = addUTCTime ((-30) * day) now
                          , cardQr = ""
                          }
    insert_ $ Info { infoCard = cid1
                   , infoName = "Birthday"
                   , infoValue = "11/22/1996"
                   }
    insert_ $ Info { infoCard = cid1
                   , infoName = "Phone"
                   , infoValue = "098755432"
                   }
    insert_ $ Info { infoCard = cid1
                   , infoName = "Facebook"
                   , infoValue = "https://facebook.com/1234123"
                   }
    insert_ $ Info { infoCard = cid1
                   , infoName = "Instagram"
                   , infoValue = "https://instagram.com/234234234"
                   }

    cid2 <- insert $ Card { cardUser = uid2
                          , cardIssued = addUTCTime ((-31) * day) now
                          , cardQr = ""
                          }
    insert_ $ Info { infoCard = cid2
                   , infoName = "Birthday"
                   , infoValue = "10/21/1995"
                   }
    insert_ $ Info { infoCard = cid2
                   , infoName = "Phone"
                   , infoValue = "+9098755432"
                   }
    insert_ $ Info { infoCard = cid2
                   , infoName = "Facebook"
                   , infoValue = "https://facebook.com/000120"
                   }
    insert_ $ Info { infoCard = cid2
                   , infoName = "Instagram"
                   , infoValue = "https://instagram.com/234234"
                   }

    cid3 <- insert $ Card { cardUser = uid3
                          , cardIssued = addUTCTime ((-32) * day) now
                          , cardQr = ""
                          }
    insert_ $ Info { infoCard = cid3
                   , infoName = "Birthday"
                   , infoValue = "09/20/1994"
                   }
    insert_ $ Info { infoCard = cid3
                   , infoName = "Phone"
                   , infoValue = "+14098755432"
                   }
    insert_ $ Info { infoCard = cid3
                   , infoName = "Facebook"
                   , infoValue = "https://facebook.com/985287782"
                   }
    insert_ $ Info { infoCard = cid3
                   , infoName = "Instagram"
                   , infoValue = "https://instagram.com/09876666"
                   }

    cid4 <- insert $ Card { cardUser = uid4
                          , cardIssued = addUTCTime ((-33) * day) now
                          , cardQr = ""
                          }
    insert_ $ Info { infoCard = cid4
                   , infoName = "Birthday"
                   , infoValue = "08/19/1993"
                   }
    insert_ $ Info { infoCard = cid4
                   , infoName = "Phone"
                   , infoValue = "+454655657"
                   }
    insert_ $ Info { infoCard = cid4
                   , infoName = "Facebook"
                   , infoValue = "https://facebook.com/23898"
                   }
    insert_ $ Info { infoCard = cid4
                   , infoName = "Instagram"
                   , infoValue = "https://instagram.com/12431265"
                   }

    eid11 <- insert $ Event { eventTime = addUTCTime hour now
                            , eventName = "Private party"
                            , eventDescr = "Only Disco"
                            }

    eid12 <- insert $ Event { eventTime = addUTCTime (2 * hour) now
                            , eventName = "Wellness party"
                            , eventDescr = "Wellness party, then Disco"
                            }

    eid2 <- insert $ Event { eventTime = addUTCTime (2 * day) now
                           , eventName = "Team-building event"
                           , eventDescr = "Team-building event, then Disco"
                           }

    eid3 <- insert $ Event { eventTime = addUTCTime (3 * day) now
                           , eventName = "Shareholder meeting"
                           , eventDescr = "Shareholder meeting, then Disco"
                           }

    eid4 <- insert $ Event { eventTime = addUTCTime (4 * day) now
                           , eventName = "Board meeting"
                           , eventDescr = "Board meeting, then Disco"
                           }

    insert_ $ Attendee { attendeeEvent = eid11
                       , attendeeCard = cid1
                       , attendeeRegDate = now
                       }

    insert_ $ Attendee { attendeeEvent = eid11
                       , attendeeCard = cid2
                       , attendeeRegDate = now
                       }

    insert_ $ Attendee { attendeeEvent = eid12
                       , attendeeCard = cid3
                       , attendeeRegDate = now
                       }

    insert_ $ Attendee { attendeeEvent = eid12
                       , attendeeCard = cid4
                       , attendeeRegDate = now
                       }

    insert_ $ Attendee { attendeeEvent = eid2
                         , attendeeCard = cid3
                         , attendeeRegDate = now
                         }

    insert_ $ Attendee { attendeeEvent = eid2
                         , attendeeCard = cid4
                         , attendeeRegDate = now
                         }

    insert_ $ Attendee { attendeeEvent = eid3
                         , attendeeCard = cid1
                         , attendeeRegDate = now
                         }

    insert_ $ Attendee { attendeeEvent = eid3
                         , attendeeCard = cid2
                         , attendeeRegDate = now
                         }

    insert_ $ Attendee { attendeeEvent = eid4
                         , attendeeCard = cid1
                         , attendeeRegDate = now
                         }

    insert_ $ Attendee { attendeeEvent = eid4
                         , attendeeCard = cid2
                         , attendeeRegDate = now
                         }

    insert_ $ Attendee { attendeeEvent = eid4
                         , attendeeCard = cid3
                         , attendeeRegDate = now
                         }
    
    return ()
