{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoEn (fillDemoEn) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString as BS
import Data.Text (Text)

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
    , Info (Info, infoCard, infoName, infoValue), Poster (Poster, posterEvent, posterMime, posterPhoto, posterAttribution)
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
    let user1 = User { userEmail = "marylopez@xmail.edu"
                     , userPassword = Just pass1
                     , userName = Just "Mary Lopez"
                     , userAdmin = True
                     }
    uid1 <- insert user1

    liftIO (BS.readFile "demo/user_1.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid1
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass2 <- liftIO $ saltPass "jjohnson"
    let user2 = User { userEmail = "jjohnson@xmail.edu"
                     , userPassword = Just pass2
                     , userName = Just "John Johnson"
                     , userAdmin = False
                     }
    uid2 <- insert user2

    liftIO (BS.readFile "demo/user_2.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid2
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass3 <- liftIO $ saltPass "jmaulsby"
    let user3 = User { userEmail = "jmaulsby@xmail.edu"
                     , userPassword = Just pass3
                     , userName = Just "Julian Maulsby"
                     , userAdmin = False
                     }
    uid3 <- insert user3

    liftIO (BS.readFile "demo/user_3.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid3
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass4 <- liftIO $ saltPass "vschoen"
    let user4 = User { userEmail = "vschoen@xmail.edu"
                     , userPassword = Just pass4
                     , userName = Just "Valentina Schoen"
                     , userAdmin = False
                     }
    uid4 <- insert user4

    liftIO (BS.readFile "demo/user_4.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid4
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    let logoFacebook :: Text
        logoFacebook = "https://upload.wikimedia.org/wikipedia/commons/5/51/Facebook_f_logo_%282019%29.svg"
    let logoInstagram :: Text
        logoInstagram = "https://upload.wikimedia.org/wikipedia/commons/9/96/Instagram.svg"

    cid1 <- insert $ Card { cardUser = uid1
                          , cardIssued = addUTCTime ((-30) * day) now
                          , cardQr = ""
                          }
    insert_ $ Info { infoCard = cid1
                   , infoName = "Birthday"
                   , infoValue = [shamlet|<time class="day" datetime="1996-11-22">11/22/1996|]
                   }
    insert_ $ Info { infoCard = cid1
                   , infoName = "Email"
                   , infoValue = [shamlet|
                                         <a class="link" href="mailto:#{userEmail user1}">
                                           <i.small.tiny-margin>mail
                                           #{userEmail user1}
                                         |]
                   }
    insert_ $ Info { infoCard = cid1
                   , infoName = "Phone"
                   , infoValue = [shamlet|
                                         <a class="link" href="tel:+098755432">
                                           <i.small.tiny-margin>call
                                           +098755432
                                         |]
                   }
    insert_ $ Info { infoCard = cid1
                   , infoName = "Facebook"
                   , infoValue = [shamlet|
                                         <a class="link" href="https://facebook.com/1234123">
                                           <img.tiny.tiny-margin src=#{logoFacebook} alt=facebook loading=lazy>
                                           facebook
                                         |]
                   }
    insert_ $ Info { infoCard = cid1
                   , infoName = "Instagram"
                   , infoValue = [shamlet|
                                         <a class="link" href="https://instagram.com/234234234">
                                           <img.tiny.tiny-margin src=#{logoInstagram} alt=instagram loading=lazy>
                                           instagram
                                         |]
                   }

    cid2 <- insert $ Card { cardUser = uid2
                          , cardIssued = addUTCTime ((-31) * day) now
                          , cardQr = ""
                          }
    insert_ $ Info { infoCard = cid2
                   , infoName = "Birthday"
                   , infoValue = [shamlet|<time class="day" datetime="1995-10-21">10/21/1995|]
                   }
    insert_ $ Info { infoCard = cid2
                   , infoName = "Email"
                   , infoValue = [shamlet|
                                         <a class="link" href="mailto:#{userEmail user2}">
                                           <i.small.tiny-margin>mail
                                           #{userEmail user2}
                                         |]
                   }
    insert_ $ Info { infoCard = cid2
                   , infoName = "Phone"
                   , infoValue = [shamlet|
                                         <a class="link" href="tel:+9098755432">
                                           <i.small.tiny-margin>call
                                           +9098755432
                                         |]
                   }
    insert_ $ Info { infoCard = cid2
                   , infoName = "Facebook"
                   , infoValue = [shamlet|
                                         <a class="link" href="https://facebook.com/000120">
                                           <img.tiny.tiny-margin src=#{logoFacebook} alt=facebook loading=lazy>
                                           facebook
                                         |]
                   }
    insert_ $ Info { infoCard = cid2
                   , infoName = "Instagram"
                   , infoValue = [shamlet|
                                         <a class="link" href="https://instagram.com/234234">
                                           <img.tiny.tiny-margin src=#{logoInstagram} alt=instagram loading=lazy>
                                           instagram
                                         |]
                   }

    cid3 <- insert $ Card { cardUser = uid3
                          , cardIssued = addUTCTime ((-32) * day) now
                          , cardQr = ""
                          }
    insert_ $ Info { infoCard = cid3
                   , infoName = "Birthday"
                   , infoValue = [shamlet|<time class="day" datetime="1994-09-20">09/20/1994|]
                   }
    insert_ $ Info { infoCard = cid3
                   , infoName = "Email"
                   , infoValue = [shamlet|
                                         <a class="link" href="mailto:#{userEmail user3}">
                                           <i.small.tiny-margin>mail
                                           #{userEmail user3}
                                         |]
                   }
    insert_ $ Info { infoCard = cid3
                   , infoName = "Phone"
                   , infoValue = [shamlet|
                                         <a class="link" href="tel:+14098755432">
                                           <i.small.tiny-margin>call
                                           +14098755432
                                         |]
                   }
    insert_ $ Info { infoCard = cid3
                   , infoName = "Facebook"
                   , infoValue = [shamlet|
                                         <a class="link" href="https://facebook.com/985287782">
                                           <img.tiny.tiny-margin src=#{logoFacebook} alt=facebook loading=lazy>
                                           facebook
                                         |]
                   }
    insert_ $ Info { infoCard = cid3
                   , infoName = "Instagram"
                   , infoValue = [shamlet|
                                         <a class="link" href="https://instagram.com/09876666">
                                           <img.tiny.tiny-margin src=#{logoInstagram} alt=instagram loading=lazy>
                                           instagram
                                         |]
                   }

    cid4 <- insert $ Card { cardUser = uid4
                          , cardIssued = addUTCTime ((-33) * day) now
                          , cardQr = ""
                          }
    insert_ $ Info { infoCard = cid4
                   , infoName = "Birthday"
                   , infoValue = [shamlet|<time class="day" datetime="1993-08-19">08/19/1993|]
                   }
    insert_ $ Info { infoCard = cid4
                   , infoName = "Email"
                   , infoValue = [shamlet|
                                         <a class="link" href="mailto:#{userEmail user4}">
                                           <i.small.tiny-margin>mail
                                           #{userEmail user4}
                                         |]
                   }
    insert_ $ Info { infoCard = cid4
                   , infoName = "Phone"
                   , infoValue = [shamlet|
                                         <a class="link" href="tel:+454655657">
                                           <i.small.tiny-margin>call
                                           +454655657
                                         |]
                   }
    insert_ $ Info { infoCard = cid4
                   , infoName = "Facebook"
                   , infoValue = [shamlet|
                                         <a class="link" href="https://facebook.com/23898">
                                           <img.tiny.tiny-margin src=#{logoFacebook} alt=facebook loading=lazy>
                                           facebook
                                         |]
                   }
    insert_ $ Info { infoCard = cid4
                   , infoName = "Instagram"
                   , infoValue = [shamlet|
                                         <a class="link" href="https://instagram.com/12431265">
                                           <img.tiny.tiny-margin src=#{logoInstagram} alt=instagram loading=lazy>
                                           instagram
                                         |]
                   }

    eid11 <- insert $ Event { eventTime = addUTCTime hour now
                            , eventName = "Private party"
                            , eventDescr = "Only Disco"
                            }
    liftIO (BS.readFile "demo/private_party_1.avif") >>= \bs ->
      insert_ Poster { posterEvent = eid11
                     , posterMime = "image/avif"
                     , posterPhoto = bs
                     , posterAttribution = Just freepik
                     }

    eid12 <- insert $ Event { eventTime = addUTCTime (2 * hour) now
                            , eventName = "Wellness party"
                            , eventDescr = "Wellness party, then Disco"
                            }
    liftIO (BS.readFile "demo/wellness_party_1.avif") >>= \bs ->
      insert_ Poster { posterEvent = eid12
                     , posterMime = "image/avif"
                     , posterPhoto = bs
                     , posterAttribution = Just freepik
                     }

    eid2 <- insert $ Event { eventTime = addUTCTime (2 * day) now
                           , eventName = "Team-building event"
                           , eventDescr = "Team-building event, then Disco"
                           }
    liftIO (BS.readFile "demo/team_building_event.avif") >>= \bs ->
      insert_ Poster { posterEvent = eid2
                     , posterMime = "image/avif"
                     , posterPhoto = bs
                     , posterAttribution = Just freepik
                     }

    eid3 <- insert $ Event { eventTime = addUTCTime (3 * day) now
                           , eventName = "Shareholder meeting"
                           , eventDescr = "Shareholder meeting, then Disco"
                           }
    liftIO (BS.readFile "demo/shareholder_meeting_1.avif") >>= \bs ->
      insert_ Poster { posterEvent = eid3
                     , posterMime = "image/avif"
                     , posterPhoto = bs
                     , posterAttribution = Just freepik
                     }

    eid4 <- insert $ Event { eventTime = addUTCTime (4 * day) now
                           , eventName = "Board meeting"
                           , eventDescr = "Board meeting, then Disco"
                           }
    liftIO (BS.readFile "demo/board_meeting_1.avif") >>= \bs ->
      insert_ Poster { posterEvent = eid4
                     , posterMime = "image/avif"
                     , posterPhoto = bs
                     , posterAttribution = Just freepik
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
