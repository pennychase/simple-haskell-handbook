module Core where

import RIO
import qualified RIO.List as List
import qualified RIO.Map as Map

data Pipeline
    = Pipeline
        { steps :: NonEmpty Step
        }
    deriving (Eq, Show)

data Step
    = Step
        { name :: StepName
        , commands :: NonEmpty Text
        , image :: Image
        }
    deriving (Eq, Show)

data Build
    = Build
        { pipeline :: Pipeline
        , state :: BuildState
        , completedSteps :: Map StepName StepResult
        }
    deriving (Eq, Show)

data BuildState
    = BuildReady
    | BuildRunning BuildRunningState
    | BuildFinished BuildResult
    deriving (Eq, Show)

data BuildRunningState 
    = BuildRunningState
        { step :: StepName
        }
    deriving (Eq, Show)

data BuildResult
    = BuildSucceeded
    | BuildFailed
    deriving (Eq, Show)

data StepResult 
    = StepFailed ContainerExitCode
    | StepSucceeded
    deriving (Eq, Show)

newtype StepName = StepName Text
    deriving (Eq, Ord, Show)

newtype Image = Image Text
    deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int
    deriving (Eq, Show)

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

imageToText :: Image -> Text
imageToText (Image image) = image

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

exitCodeToStepResult :: ContainerExitCode -> StepResult
exitCodeToStepResult exit =
    if exitCodeToInt exit == 0
        then StepSucceeded
        else StepFailed exit

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build = 
    if allSucceeded
        then case nextStep of
            Just step -> Right step
            Nothing -> Left BuildSucceeded
        else Left BuildFailed
    where
        allSucceeded = List.all ((==) StepSucceeded) build.completedSteps
        nextStep = List.find f build.pipeline.steps
        f step = not $ Map.member step.name build.completedSteps


progress :: Build -> IO Build
progress build =
    case build.state of
        BuildReady -> 
            case buildHasNextStep build of
                Left result ->
                    pure $ build { state = BuildFinished result }
                Right step -> do
                    let s = BuildRunningState { step = step.name }
                    pure $ build { state = BuildRunning s }
        BuildRunning state -> do
            let exit = ContainerExitCode 0 -- for now always succeed
                result = exitCodeToStepResult exit
            -- Return to BuildReady so we can run again
            pure build
                { state = BuildReady
                , completedSteps = Map.insert state.step result build.completedSteps
                }
        BuildFinished _ -> pure build
