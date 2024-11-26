{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoEn (fillDemoEn) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime, addUTCTime)

import Database.Persist (PersistStoreWrite (insert, insert_))
import Database.Persist.SqlBackend (SqlBackend)

import Model
    ( keyApiVapid, keyApiGmail
    , User
      ( User, userEmail, userPassword, userSuper, userAdmin, userName
      , userManager, userAuthType, userVerkey, userVerified
      )
    , UserPhoto
      ( UserPhoto, userPhotoUser, userPhotoMime, userPhotoAttribution
      , userPhotoPhoto
      )
    , Card (Card, cardUser, cardUpdated, cardQr, cardStatus, cardOrdered, cardModerator)
    , Event (Event, eventTime, eventName, eventDescr, eventManager, eventDuration)
    , Attendee (Attendee, attendeeEvent, attendeeCard, attendeeRegDate)
    , Info (Info, infoCard, infoName, infoValue)
    , Poster (Poster, posterEvent, posterMime, posterPhoto, posterAttribution)
    , Token (Token, tokenApi, tokenStore)
    , Store (Store, storeToken, storeKey, storeVal)
    , StoreType (StoreTypeDatabase, StoreTypeGoogleSecretManager), secretVapid
    , AuthenticationType (UserAuthTypePassword)
    , CardStatus (CardStatusApproved, CardStatusAwaiting)
    , Photo (photoCard, photoMime, photoPhoto, photoAttribution, Photo)
    )
    
import Settings (AppSettings (appDevelopment))

import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)


fillDemoEn :: MonadIO m => AppSettings -> ReaderT SqlBackend m ()
fillDemoEn appSettings = do

    now <- liftIO getCurrentTime

    
    let minute = 60
    let hour = 60 * minute
    let day = 24 * hour
    
    let freepik = [shamlet|
                          Designed by #
                          <a href="https://www.freepik.com/" target=_blank>
                            Freepik
                          |]

    if appDevelopment appSettings
        then do
        tid <- insert Token { tokenApi = keyApiVapid
                            , tokenStore = StoreTypeDatabase
                            }
        insert_ Store { storeToken = tid
                      , storeKey = secretVapid
                      , storeVal = "(77365822285703042512872615574182905356295150688934735928983377057495846568016,89758693107958609666387142100268936888371385180946146470089126836526923460219,12181163207068819145591782690996401924629474419901482659430518799427544674224)"
                      }
        else do
        insert_ Token { tokenApi = keyApiGmail
                      , tokenStore = StoreTypeGoogleSecretManager
                      }

        insert_ Token { tokenApi = keyApiVapid
                      , tokenStore = StoreTypeGoogleSecretManager
                      }

    pass1 <- liftIO $ saltPass "marylopez"
    let user1 = User { userEmail = "marylopez@xmail.edu"
                     , userPassword = Just pass1
                     , userName = Just "Mary Lopez"
                     , userSuper = False
                     , userAdmin = True
                     , userManager = True
                     , userAuthType = UserAuthTypePassword
                     , userVerkey = Nothing
                     , userVerified = False
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
                     , userSuper = False
                     , userAdmin = False
                     , userManager = True
                     , userAuthType = UserAuthTypePassword
                     , userVerkey = Nothing
                     , userVerified = False
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
                     , userSuper = False
                     , userAdmin = False
                     , userManager = False
                     , userAuthType = UserAuthTypePassword
                     , userVerkey = Nothing
                     , userVerified = False
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
                     , userSuper = False
                     , userAdmin = False
                     , userManager = False
                     , userAuthType = UserAuthTypePassword
                     , userVerkey = Nothing
                     , userVerified = False
                     }
    uid4 <- insert user4

    liftIO (BS.readFile "demo/user_4.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid4
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass5 <- liftIO $ saltPass "jaturnbow"
    let user5 = User { userEmail = "jaturnbow@xmail.edu"
                     , userPassword = Just pass5
                     , userName = Just "Jill A. Turnbow"
                     , userSuper = False
                     , userAdmin = False
                     , userManager = False
                     , userAuthType = UserAuthTypePassword
                     , userVerkey = Nothing
                     , userVerified = False
                     }
    uid5 <- insert user5

    liftIO (BS.readFile "demo/user_5.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid5
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass6 <- liftIO $ saltPass "jaturnbow"
    let user6 = User { userEmail = "cswatkins@xmail.edu"
                     , userPassword = Just pass6
                     , userName = Just "Charles S. Watkins"
                     , userSuper = False
                     , userAdmin = False
                     , userManager = False
                     , userAuthType = UserAuthTypePassword
                     , userVerkey = Nothing
                     , userVerified = False
                     }
    uid6 <- insert user6

    liftIO (BS.readFile "demo/user_6.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid6
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass7 <- liftIO $ saltPass "dlmeyer"
    let user7 = User { userEmail = "dlmeyer@xmail.edu"
                     , userPassword = Just pass7
                     , userName = Just "Donald L. Meyer"
                     , userSuper = False
                     , userAdmin = False
                     , userManager = False
                     , userAuthType = UserAuthTypePassword
                     , userVerkey = Nothing
                     , userVerified = False
                     }
    uid7 <- insert user7

    liftIO (BS.readFile "demo/user_7.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid7
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass8 <- liftIO $ saltPass "jjbullock"
    let user8 = User { userEmail = "jjbullock@xmail.edu"
                     , userPassword = Just pass8
                     , userName = Just "Jean J. Bullock"
                     , userSuper = False
                     , userAdmin = False
                     , userManager = False
                     , userAuthType = UserAuthTypePassword
                     , userVerkey = Nothing
                     , userVerified = False
                     }
    uid8 <- insert user8

    liftIO (BS.readFile "demo/user_8.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid8
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    let logoFacebook :: Text
        logoFacebook = "https://upload.wikimedia.org/wikipedia/commons/5/51/Facebook_f_logo_%282019%29.svg"
    let logoInstagram :: Text
        logoInstagram = "https://upload.wikimedia.org/wikipedia/commons/9/96/Instagram.svg"

    cid1 <- insert $ Card { cardUser = uid1
                          , cardQr = ""
                          , cardOrdered = addUTCTime ((-31) * day) now
                          , cardStatus = CardStatusApproved
                          , cardUpdated = Just ( addUTCTime ((-30) * day) now )
                          , cardModerator = Just uid1
                          }
    liftIO (BS.readFile "demo/user_1.avif") >>= \bs ->
      insert_ Photo { photoCard = cid1
                    , photoMime = "image/avif"
                    , photoPhoto = bs
                    , photoAttribution = Just freepik
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
                          , cardQr = ""
                          , cardOrdered = addUTCTime ((-32) * day) now
                          , cardStatus = CardStatusApproved
                          , cardUpdated = Just ( addUTCTime ((-31) * day) now )
                          , cardModerator = Just uid1
                          }
    liftIO (BS.readFile "demo/user_2.avif") >>= \bs ->
      insert_ Photo { photoCard = cid2
                    , photoMime = "image/avif"
                    , photoPhoto = bs
                    , photoAttribution = Just freepik
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
                          , cardQr = ""
                          , cardOrdered = addUTCTime ((-33) * day) now
                          , cardStatus = CardStatusApproved
                          , cardUpdated = Just ( addUTCTime ((-32) * day) now )
                          , cardModerator = Just uid1
                          }
    liftIO (BS.readFile "demo/user_3.avif") >>= \bs ->
      insert_ Photo { photoCard = cid3
                    , photoMime = "image/avif"
                    , photoPhoto = bs
                    , photoAttribution = Just freepik
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
                          , cardQr = ""
                          , cardOrdered = addUTCTime ((-34) * day) now
                          , cardStatus = CardStatusApproved
                          , cardUpdated = Just ( addUTCTime ((-33) * day) now )
                          , cardModerator = Just uid1
                          }
    liftIO (BS.readFile "demo/user_4.avif") >>= \bs ->
      insert_ Photo { photoCard = cid4
                    , photoMime = "image/avif"
                    , photoPhoto = bs
                    , photoAttribution = Just freepik
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

    cid5 <- insert $ Card { cardUser = uid5
                          , cardQr = ""
                          , cardOrdered = addUTCTime ((-1) * day) now
                          , cardStatus = CardStatusAwaiting
                          , cardUpdated = Nothing
                          , cardModerator = Nothing
                          }
    liftIO (BS.readFile "demo/user_5.avif") >>= \bs ->
      insert_ Photo { photoCard = cid5
                    , photoMime = "image/avif"
                    , photoPhoto = bs
                    , photoAttribution = Just freepik
                    }

    cid6 <- insert $ Card { cardUser = uid6
                          , cardQr = ""
                          , cardOrdered = addUTCTime ((-1) * day + (1 * hour)) now
                          , cardStatus = CardStatusAwaiting
                          , cardUpdated = Nothing
                          , cardModerator = Nothing
                          }
    liftIO (BS.readFile "demo/user_6.avif") >>= \bs ->
      insert_ Photo { photoCard = cid6
                    , photoMime = "image/avif"
                    , photoPhoto = bs
                    , photoAttribution = Just freepik
                    }

    eid11 <- insert $ Event { eventManager = uid1
                            , eventTime = addUTCTime hour now
                            , eventName = "Private party"
                            , eventDescr = "Only Disco"
                            , eventDuration = 1 * hour + (5 * minute)
                            }
    liftIO (BS.readFile "demo/private_party_1.avif") >>= \bs ->
      insert_ Poster { posterEvent = eid11
                     , posterMime = "image/avif"
                     , posterPhoto = bs
                     , posterAttribution = Just freepik
                     }

    eid12 <- insert $ Event { eventManager = uid2
                            , eventTime = addUTCTime (2 * hour) now
                            , eventName = "Wellness party"
                            , eventDescr = "Wellness party, then Disco"
                            , eventDuration = 1 * hour + (10 * minute)
                            }
    liftIO (BS.readFile "demo/wellness_party_1.avif") >>= \bs ->
      insert_ Poster { posterEvent = eid12
                     , posterMime = "image/avif"
                     , posterPhoto = bs
                     , posterAttribution = Just freepik
                     }

    eid2 <- insert $ Event { eventManager = uid3
                           , eventTime = addUTCTime (2 * day) now
                           , eventName = "Team-building event"
                           , eventDescr = "Team-building event, then Disco"
                            , eventDuration = 1 * hour + (15 * minute)
                           }
    liftIO (BS.readFile "demo/team_building_event.avif") >>= \bs ->
      insert_ Poster { posterEvent = eid2
                     , posterMime = "image/avif"
                     , posterPhoto = bs
                     , posterAttribution = Just freepik
                     }

    eid3 <- insert $ Event { eventManager = uid4
                           , eventTime = addUTCTime (3 * day) now
                           , eventName = "Shareholder meeting"
                           , eventDescr = "Shareholder meeting, then Disco"
                           , eventDuration = 1 * hour + (20 * minute)
                           }
    liftIO (BS.readFile "demo/shareholder_meeting_1.avif") >>= \bs ->
      insert_ Poster { posterEvent = eid3
                     , posterMime = "image/avif"
                     , posterPhoto = bs
                     , posterAttribution = Just freepik
                     }

    eid4 <- insert $ Event { eventManager = uid1
                           , eventTime = addUTCTime ((-2) * day + (-2) * hour) now
                           , eventName = "Board meeting"
                           , eventDescr = "Board meeting, then Disco"
                           , eventDuration = 1 * hour + (25 * minute)
                           }
    liftIO (BS.readFile "demo/board_meeting_1.avif") >>= \bs ->
      insert_ Poster { posterEvent = eid4
                     , posterMime = "image/avif"
                     , posterPhoto = bs
                     , posterAttribution = Just freepik
                     }

    eid5 <- insert $ Event { eventManager = uid1
                           , eventTime = addUTCTime (1 * day + 1 * hour) now
                           , eventName = "Milestone celebration"
                           , eventDescr = "Milestone celebration, then Disco"
                           , eventDuration = 1 * hour + (30 * minute)
                           }
    liftIO (BS.readFile "demo/milestone_celebration.avif") >>= \bs ->
      insert_ Poster { posterEvent = eid5
                     , posterMime = "image/avif"
                     , posterPhoto = bs
                     , posterAttribution = Just freepik
                     }

    eid6 <- insert $ Event { eventManager = uid1
                           , eventTime = addUTCTime (2 * day + 2 * hour) now
                           , eventName = "Business seminar"
                           , eventDescr = "Business seminar, then Disco"
                           , eventDuration = 1 * hour + (35 * minute)
                           }
    liftIO (BS.readFile "demo/business_seminar_1.avif") >>= \bs ->
      insert_ Poster { posterEvent = eid6
                     , posterMime = "image/avif"
                     , posterPhoto = bs
                     , posterAttribution = Just freepik
                     }

    eid7 <- insert $ Event { eventManager = uid1
                           , eventTime = addUTCTime (3 * day + 3 * hour) now
                           , eventName = "Fundraising event"
                           , eventDescr = "Fundraising event, then Disco"
                           , eventDuration = 1 * hour + (13 * minute)
                           }
    liftIO (BS.readFile "demo/fundraising_1.avif") >>= \bs ->
      insert_ Poster { posterEvent = eid7
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
