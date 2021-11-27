module Docker where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import Data.Aeson ( (.:) )
import qualified Network.HTTP.Simple as HTTP
import qualified Socket
import RIO

newtype Image = Image { imageToText :: Text }
    deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode { exitCodeToInt :: Int }
    deriving (Eq, Show)

newtype ContainerId = ContainerId { containerIdToText :: Text }
    deriving (Eq, Show)

data CreateContainerOptions
    = CreateContainerOptions
        { image :: Image
        }

data Service 
    = Service
        { createContainer :: CreateContainerOptions -> IO ContainerId
        , startContainer :: ContainerId -> IO ()
        }

createService :: IO Service
createService = do
    pure Service 
        { createContainer = createContainer_ 
        , startContainer = startContainer_
        }

createContainer_ :: CreateContainerOptions -> IO ContainerId
createContainer_ options = do
    manager <- Socket.newManager "/var/run/docker.sock"

    let image = imageToText options.image
    let body = Aeson.object
                [ ("Image", Aeson.toJSON image)
                , ("Tty", Aeson.toJSON True)
                , ("Labels", Aeson.object [("quad", "")])
                , ("Cmd", "uname -a")
                , ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"])
                ]

    let req = HTTP.defaultRequest
            & HTTP.setRequestManager manager
            & HTTP.setRequestPath "/v1.40/containers/create"
            & HTTP.setRequestMethod "POST"
            & HTTP.setRequestBodyJSON body
    
    let parser = Aeson.withObject "create-container" $ \o -> do
            cId <- o .: "Id"
            pure $ ContainerId cId

    res <- HTTP.httpBS req
    parseResponse res parser

parseResponse :: HTTP.Response ByteString -> (Aeson.Value -> Aeson.Types.Parser a) -> IO a
parseResponse res parser = do
    let result = do
            value <- Aeson.eitherDecodeStrict (HTTP.getResponseBody res)
            Aeson.Types.parseEither parser value
    case result of
        Left e -> throwString e
        Right status -> pure status


startContainer_ :: ContainerId -> IO ()
startContainer_ container = do
    manager <- Socket.newManager "/var/run/docker.sock"

    let path = "/v1.40/containers/" <> containerIdToText container <> "/start"
    
    let req = HTTP.defaultRequest
            & HTTP.setRequestManager manager
            & HTTP.setRequestPath (encodeUtf8 path)
            & HTTP.setRequestMethod "POST"
    void $ HTTP.httpBS req