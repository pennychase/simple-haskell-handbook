module Server where

import RIO
import qualified Codec.Serialise as Serialise
import qualified Web.Scotty as Scotty

import Core
import qualified JobHandler


data Config =
    Config 
        { port :: Int 
        }


run :: Config -> JobHandler.Service -> IO ()
run config handler =
    Scotty.scotty config.port do
        Scotty.post "/agent/pull" do
            cmd <- Scotty.liftAndCatchIO do
                handler.dispatchCmd
            Scotty.raw $ Serialise.serialise cmd
        Scotty.post "/agent/send" do
            msg <- Serialise.deserialise <$> Scotty.body
            Scotty.liftAndCatchIO do
                handler.processMsg msg
            Scotty.json ("message processed" :: Text)