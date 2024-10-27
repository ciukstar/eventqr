{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoRu (fillDemoRu) where

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
    , Attendee (Attendee, attendeeEvent, attendeeCard, attendeeRegDate)
    )
    
import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)
import Data.Time.Clock (getCurrentTime, addUTCTime)


fillDemoRu :: MonadIO m => ReaderT SqlBackend m ()
fillDemoRu = do

    now <- liftIO getCurrentTime

    let hour = 60 * 60
    let day = 24 * hour

    let freepik = [shamlet|
                          Designed by #
                          <a href="https://www.freepik.com/" target=_blank>
                            Freepik
                          |]
    

    pass1 <- liftIO $ saltPass "bulanovalm"
    uid1 <- insert $ User { userEmail = "bulanovalm@mail.ru"
                          , userPassword = Just pass1
                          , userName = Just "Буланова Любовь Михайловна"
                          , userAdmin = True
                          }

    liftIO (BS.readFile "demo/user_1.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid1
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass2 <- liftIO $ saltPass "petrovia"
    uid2 <- insert $ User { userEmail = "petrovia@mail.ru"
                          , userPassword = Just pass2
                          , userName = Just "Петров Иван Александрович"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_2.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid2
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass3 <- liftIO $ saltPass "smirnovav"
    uid3 <- insert $ User { userEmail = "smirnovav@mail.ru"
                          , userPassword = Just pass3
                          , userName = Just "Смирнов Андрей Васильевич"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_3.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid3
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass4 <- liftIO $ saltPass "sergeevaav"
    uid4 <- insert $ User { userEmail = "sergeevaav@mail.ru"
                          , userPassword = Just pass4
                          , userName = Just "Сергеева Александра Владимировна"
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

    cid2 <- insert $ Card { cardUser = uid2
                          , cardIssued = addUTCTime ((-31) * day) now
                          , cardQr = ""
                          }

    cid3 <- insert $ Card { cardUser = uid3
                          , cardIssued = addUTCTime ((-32) * day) now
                          , cardQr = ""
                          }

    cid4 <- insert $ Card { cardUser = uid4
                          , cardIssued = addUTCTime ((-33) * day) now
                          , cardQr = ""
                          }

    eid11 <- insert $ Event { eventTime = addUTCTime hour now
                            , eventName = "Частная вечеринка"
                            , eventDescr = "Только Дискотека"
                            }

    eid12 <- insert $ Event { eventTime = addUTCTime (2 * hour) now
                            , eventName = "Оздоровительная вечеринка"
                            , eventDescr = "Оздоровительная вечеринка, затем дискотека"
                            }

    eid2 <- insert $ Event { eventTime = addUTCTime (2 * day) now
                           , eventName = "Мероприятие по сплочению коллектива"
                           , eventDescr = "Командообразование, затем дискотека"
                           }

    eid3 <- insert $ Event { eventTime = addUTCTime (3 * day) now
                           , eventName = "Собрание акционеров"
                           , eventDescr = "Собрание акционеров, затем дискотека"
                           }

    eid4 <- insert $ Event { eventTime = addUTCTime (4 * day) now
                           , eventName = "Заседание правления"
                           , eventDescr = "Заседание совета директоров, затем дискотека"
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
