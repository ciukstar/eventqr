{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoRu (fillDemoRu) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString as BS
import Data.Text(Text)
import Data.Time.Clock (getCurrentTime, addUTCTime)

import Database.Persist (PersistStoreWrite (insert, insert_))
import Database.Persist.SqlBackend (SqlBackend)

import Model
    ( keyApiVapid, keyApiGmail
    , User
      ( User, userEmail, userPassword, userSuper, userAdmin, userName, userManager
      , userAuthType, userVerkey, userVerified
      )
    , UserPhoto
      ( UserPhoto, userPhotoUser, userPhotoMime, userPhotoAttribution
      , userPhotoPhoto
      )
    , Card (Card, cardUser, cardIssued, cardQr)
    , Event (Event, eventTime, eventName, eventDescr, eventManager)
    , Attendee (Attendee, attendeeEvent, attendeeCard, attendeeRegDate)
    , Info (Info, infoCard, infoName, infoValue)
    , Poster (Poster, posterEvent, posterMime, posterPhoto, posterAttribution)
    , Token (Token, tokenApi, tokenStore)
    , Store (Store, storeToken, storeKey, storeVal)
    , StoreType (StoreTypeDatabase, StoreTypeGoogleSecretManager), secretVapid
    , AuthenticationType (UserAuthTypePassword)
    )
    
import Settings (AppSettings (appDevelopment))
    
import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)


fillDemoRu :: MonadIO m => AppSettings -> ReaderT SqlBackend m ()
fillDemoRu appSettings = do

    now <- liftIO getCurrentTime

    let hour = 60 * 60
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
    

    pass1 <- liftIO $ saltPass "bulanovalm"
    let user1 = User { userEmail = "bulanovalm@mail.ru"
                     , userPassword = Just pass1
                     , userName = Just "Буланова Любовь Михайловна"
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

    pass2 <- liftIO $ saltPass "petrovia"
    let user2 = User { userEmail = "petrovia@mail.ru"
                     , userPassword = Just pass2
                     , userName = Just "Петров Иван Александрович"
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

    pass3 <- liftIO $ saltPass "smirnovav"
    let user3 = User { userEmail = "smirnovav@mail.ru"
                     , userPassword = Just pass3
                     , userName = Just "Смирнов Андрей Васильевич"
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

    pass4 <- liftIO $ saltPass "sergeevaav"
    let user4 = User { userEmail = "sergeevaav@mail.ru"
                     , userPassword = Just pass4
                     , userName = Just "Сергеева Александра Владимировна"
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

    pass5 <- liftIO $ saltPass "oalekseeva"
    let user5 = User { userEmail = "oalekseeva@mail.ru"
                     , userPassword = Just pass5
                     , userName = Just "Ольга Алексеева"
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

    pass6 <- liftIO $ saltPass "lglazkov"
    let user6 = User { userEmail = "lglazkov@mail.ru"
                     , userPassword = Just pass6
                     , userName = Just "Леонтий Глазков"
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

    pass7 <- liftIO $ saltPass "obezrukov"
    let user7 = User { userEmail = "obezrukov@mail.ru"
                     , userPassword = Just pass7
                     , userName = Just "Остап Безруков"
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

    pass8 <- liftIO $ saltPass "vyashina"
    let user8 = User { userEmail = "vyashina@mail.ru"
                     , userPassword = Just pass8
                     , userName = Just "Вера Яшина"
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

    let logoVK :: Text
        logoVK = "https://upload.wikimedia.org/wikipedia/commons/2/21/VK.com-logo.svg"
    let logoOk :: Text
        logoOk = "https://upload.wikimedia.org/wikipedia/commons/0/0c/Odnoklassniki.svg"

    cid1 <- insert $ Card { cardUser = uid1
                          , cardIssued = addUTCTime ((-30) * day) now
                          , cardQr = ""
                          }
    insert_ $ Info { infoCard = cid1
                   , infoName = "Дата рождения"
                   , infoValue = [shamlet|<time class="day" datetime="1996-11-22">22.11.1996|]
                   }
    insert_ $ Info { infoCard = cid1
                   , infoName = "Электронная почта"
                   , infoValue = [shamlet|
                                         <a class="link" href="mailto:#{userEmail user1}">
                                           <i.small.tiny-margin>mail
                                           #{userEmail user1}
                                         |]
                   }
    insert_ $ Info { infoCard = cid1
                   , infoName = "Телефон"
                   , infoValue = [shamlet|
                                         <a class="link" href="tel:+098755432">
                                           <i.small.tiny-margin>call
                                           +098755432
                                         |]
                   }
    insert_ $ Info { infoCard = cid1
                   , infoName = "ВКонтакте"
                   , infoValue = [shamlet|
                                         <a class="link" href="https://vk.ru/1234123">
                                           <img.tiny.tiny-margin src=#{logoVK} alt=vk loading=lazy>
                                           VK
                                         |]
                   }
    insert_ $ Info { infoCard = cid1
                   , infoName = "Одноклассники"
                   , infoValue = [shamlet|
                                         <a class="link" href="https://ok.ru/234234234">
                                           <img.tiny.tiny-margin src=#{logoOk} alt=ok loading=lazy>
                                           Ok
                                         |]
                   }

    cid2 <- insert $ Card { cardUser = uid2
                          , cardIssued = addUTCTime ((-31) * day) now
                          , cardQr = ""
                          }
    insert_ $ Info { infoCard = cid2
                   , infoName = "Дата рождения"
                   , infoValue = [shamlet|<time class="day" datetime="1995-10-21">21.10.1995|]
                   }
    insert_ $ Info { infoCard = cid2
                   , infoName = "Электронная почта"
                   , infoValue = [shamlet|
                                         <a class="link" href="mailto:#{userEmail user2}">
                                           <i.small.tiny-margin>mail
                                           #{userEmail user2}
                                         |]
                   }
    insert_ $ Info { infoCard = cid2
                   , infoName = "Телефон"
                   , infoValue = [shamlet|
                                         <a class="link" href="tel:+9098755432">
                                           <i.small.tiny-margin>call
                                           +9098755432
                                         |]
                   }
    insert_ $ Info { infoCard = cid2
                   , infoName = "ВКонтакте"
                   , infoValue = [shamlet|
                                         <a class="link" href="https://vk.ru/000120">
                                           <img.tiny.tiny-margin src=#{logoVK} alt=vk loading=lazy>
                                           VK
                                         |]
                   }
    insert_ $ Info { infoCard = cid2
                   , infoName = "Одноклассники"
                   , infoValue = [shamlet|
                                         <a class="link" href="https://ok.ru/234234">
                                           <img.tiny.tiny-margin src=#{logoOk} alt=ok loading=lazy>
                                           Ok
                                         |]
                   }

    cid3 <- insert $ Card { cardUser = uid3
                          , cardIssued = addUTCTime ((-32) * day) now
                          , cardQr = ""
                          }
    insert_ $ Info { infoCard = cid3
                   , infoName = "Дата рождения"
                   , infoValue = [shamlet|<time class="day" datetime="1994-09-20">20.09.1994|]
                   }
    insert_ $ Info { infoCard = cid3
                   , infoName = "Электронная почта"
                   , infoValue = [shamlet|
                                         <a class="link" href="mailto:#{userEmail user3}">
                                           <i.small.tiny-margin>mail
                                           #{userEmail user3}
                                         |]
                   }
    insert_ $ Info { infoCard = cid3
                   , infoName = "Телефон"
                   , infoValue = [shamlet|
                                         <a class="link" href="tel:+14098755432">
                                           <i.small.tiny-margin>call
                                           +14098755432
                                         |]
                   }
    insert_ $ Info { infoCard = cid3
                   , infoName = "ВКонтакте"
                   , infoValue = [shamlet|
                                         <a class="link" href="https://vk.ru/985287782">
                                           <img.tiny.tiny-margin src=#{logoVK} alt=vk loading=lazy>
                                           VK
                                         |]
                   }
    insert_ $ Info { infoCard = cid3
                   , infoName = "Одноклассники"
                   , infoValue = [shamlet|
                                         <a class="link" href="https://ok.ru/09876666">
                                           <img.tiny.tiny-margin src=#{logoOk} alt=ok loading=lazy>
                                           Ok
                                         |]
                   }

    cid4 <- insert $ Card { cardUser = uid4
                          , cardIssued = addUTCTime ((-33) * day) now
                          , cardQr = ""
                          }
    insert_ $ Info { infoCard = cid4
                   , infoName = "Дата рождения"
                   , infoValue = [shamlet|<time class="day" datetime="1993-08-19">19.08.1993|]
                   }
    insert_ $ Info { infoCard = cid4
                   , infoName = "Электронная почта"
                   , infoValue = [shamlet|
                                         <a class="link" href="mailto:#{userEmail user4}">
                                           <i.small.tiny-margin>mail
                                           #{userEmail user4}
                                         |]
                   }
    insert_ $ Info { infoCard = cid4
                   , infoName = "Телефон"
                   , infoValue = [shamlet|
                                         <a class="link" href="tel:+454655657">
                                           <i.small.tiny-margin>call
                                           +454655657
                                         |]
                   }
    insert_ $ Info { infoCard = cid4
                   , infoName = "ВКонтакте"
                   , infoValue = [shamlet|
                                         <a class="link" href="https://vk.ru/23898">
                                           <img.tiny.tiny-margin src=#{logoVK} alt=vk loading=lazy>
                                           VK
                                         |]
                   }
    insert_ $ Info { infoCard = cid4
                   , infoName = "Одноклассники"
                   , infoValue = [shamlet|
                                         <a class="link" href="https://ok.ru/12431265">
                                           <img.tiny.tiny-margin src=#{logoOk} alt=ok loading=lazy>
                                           Ok
                                         |]
                   }

    eid11 <- insert $ Event { eventManager = uid1
                            , eventTime = addUTCTime hour now
                            , eventName = "Частная вечеринка"
                            , eventDescr = "Только Дискотека"
                            }
    liftIO (BS.readFile "demo/private_party_1.avif") >>= \bs ->
      insert_ Poster { posterEvent = eid11
                     , posterMime = "image/avif"
                     , posterPhoto = bs
                     , posterAttribution = Just freepik
                     }

    eid12 <- insert $ Event { eventManager = uid2
                            , eventTime = addUTCTime (2 * hour) now
                            , eventName = "Оздоровительная вечеринка"
                            , eventDescr = "Оздоровительная вечеринка, затем дискотека"
                            }
    liftIO (BS.readFile "demo/wellness_party_1.avif") >>= \bs ->
      insert_ Poster { posterEvent = eid12
                     , posterMime = "image/avif"
                     , posterPhoto = bs
                     , posterAttribution = Just freepik
                     }

    eid2 <- insert $ Event { eventManager = uid3
                           , eventTime = addUTCTime (2 * day) now
                           , eventName = "Мероприятие по сплочению коллектива"
                           , eventDescr = "Командообразование, затем дискотека"
                           }
    liftIO (BS.readFile "demo/team_building_event.avif") >>= \bs ->
      insert_ Poster { posterEvent = eid2
                     , posterMime = "image/avif"
                     , posterPhoto = bs
                     , posterAttribution = Just freepik
                     }

    eid3 <- insert $ Event { eventManager = uid4
                           , eventTime = addUTCTime (3 * day) now
                           , eventName = "Собрание акционеров"
                           , eventDescr = "Собрание акционеров, затем дискотека"
                           }
    liftIO (BS.readFile "demo/shareholder_meeting_1.avif") >>= \bs ->
      insert_ Poster { posterEvent = eid3
                     , posterMime = "image/avif"
                     , posterPhoto = bs
                     , posterAttribution = Just freepik
                     }

    eid4 <- insert $ Event { eventManager = uid1
                           , eventTime = addUTCTime ((-2) * day + (-2) * hour) now
                           , eventName = "Заседание правления"
                           , eventDescr = "Заседание совета директоров, затем дискотека"
                           }
    liftIO (BS.readFile "demo/board_meeting_1.avif") >>= \bs ->
      insert_ Poster { posterEvent = eid4
                     , posterMime = "image/avif"
                     , posterPhoto = bs
                     , posterAttribution = Just freepik
                     }

    eid5 <- insert $ Event { eventManager = uid1
                           , eventTime = addUTCTime (1 * day + 1 * hour) now
                           , eventName = "Празднование знаменательной даты"
                           , eventDescr = "Празднование знаменательной даты, затем дискотека"
                           }
    liftIO (BS.readFile "demo/milestone_celebration.avif") >>= \bs ->
      insert_ Poster { posterEvent = eid5
                     , posterMime = "image/avif"
                     , posterPhoto = bs
                     , posterAttribution = Just freepik
                     }

    eid6 <- insert $ Event { eventManager = uid1
                           , eventTime = addUTCTime (2 * day + 2 * hour) now
                           , eventName = "Деловой семинар"
                           , eventDescr = "Деловой семинар, затем дискотека"
                           }
    liftIO (BS.readFile "demo/business_seminar_1.avif") >>= \bs ->
      insert_ Poster { posterEvent = eid6
                     , posterMime = "image/avif"
                     , posterPhoto = bs
                     , posterAttribution = Just freepik
                     }

    eid7 <- insert $ Event { eventManager = uid1
                           , eventTime = addUTCTime (3 * day + 3 * hour) now
                           , eventName = "Мероприятие по сбору средств"
                           , eventDescr = "Мероприятие по сбору средств, затем дискотека"
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
