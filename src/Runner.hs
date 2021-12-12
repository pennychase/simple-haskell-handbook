module Runner where

import RIO

import Core
import qualified Docker

data Service 
    = Service 
        { runBuild :: Build -> IO Build
        , prepareBuild :: Pipeline -> IO Build
        }

createService :: Docker.Service -> IO Service
createService docker = do
    pure Service
        { runBuild = runBuild_ docker 
        , prepareBuild = prepareBuild_ docker
        }

runBuild_ :: Docker.Service -> Build -> IO Build 
runBuild_ docker build = do
    newBuild <- Core.progress docker build
    case newBuild.state of
        BuildFinished _ ->
            pure newBuild
        _ -> do
            threadDelay (1 * 1000 * 1000)
            runBuild_ docker newBuild

prepareBuild_ :: Docker.Service -> Pipeline -> IO Build 
prepareBuild_ docker pipeline = do
    pure Build
        { pipeline = pipeline
        , state = BuildReady 
        , completedSteps = mempty
        }