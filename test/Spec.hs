module Main where

import RIO
import qualified RIO.ByteString as ByteString
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified Control.Concurrent.Async as Async
import qualified Data.Yaml as Yaml
import qualified System.Process.Typed as Process
import Test.Hspec


import Core
import qualified Agent
import qualified Docker
import qualified JobHandler
import qualified JobHandler.Memory
import qualified Runner
import qualified Server


main :: IO ()
main = hspec do
    docker <- runIO Docker.createService
    runner <- runIO $ Runner.createService docker

    beforeAll cleanupDocker $ describe "Quad CI" do
        it "should run server and agent" do
            testServerAndAgent runner
        it "should decode pipelines" do
            testYamlDecoding runner
        it "should run a build (success)" do
            testRunSuccess runner
        it "should run a build (failure)" do
            testRunFailure runner
        it "should share a workspace between steps" do
            testSharedWorkSpace docker runner
        it "should collect logs" do
            testLogCollection runner
        it "should pull images" do
            testImagePull runner

cleanupDocker :: IO ()
cleanupDocker = void do
    Process.readProcessStdout "docker rm -f `docker ps -aq --filter \"label=quad\"`"
    Process.readProcessStdout "docker volume rm -f `docker volume ls -q --filter \"label=quad\"`" 


-- Helper functions
makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands
    = Step
        { name = StepName name
        , image = Docker.Image { name = image, tag = "latest" }
        , commands = NonEmpty.Partial.fromList commands
        }

makePipeline :: [Step] -> Pipeline
makePipeline steps  =
    Pipeline { steps = NonEmpty.Partial.fromList steps }

emptyHooks :: Runner.Hooks
emptyHooks =
    Runner.Hooks 
        { logCollected = \_ -> pure () 
        , buildUpdated = \_ -> pure ()
        }

checkBuild :: JobHandler.Service -> BuildNumber -> IO ()
checkBuild handler number = loop
    where
        loop = do
            Just job <- handler.findJob number
            case job.state of
                JobHandler.JobScheduled build -> do
                    case build.state of
                        BuildFinished s -> s `shouldBe` BuildSucceeded 
                        _ -> loop 
                _ -> loop

-- Test Values

testPipeline :: Pipeline
testPipeline = makePipeline
    [ makeStep "First step" "ubuntu" ["date"]
    , makeStep "Second step" "ubuntu" ["uname -r"]
    ]

testBuild :: Build
testBuild = Build
    { pipeline = testPipeline
    , state = BuildReady
    , completedSteps = mempty
    , volume = Docker.Volume ""
    }

-- Tests

testServerAndAgent :: Runner.Service -> IO ()
testServerAndAgent runner = do
    handler <- JobHandler.Memory.createService 

    serverThread <- Async.async do
        Server.run (Server.Config 9000) handler

    Async.link serverThread

    agentThread <- Async.async do
        Agent.run (Agent.Config "http://localhost:9000") runner

    Async.link agentThread

    let pipeline1 = makePipeline
            [ makeStep "agent-test" "busybox" ["echo hello", "echo from agent"]
            ]

    let pipeline2 = makePipeline
            [ makeStep "agent-test" "ubuntu" ["date", "echo from agent"]
            ]

    number1 <- handler.queueJob pipeline1
    checkBuild handler number1

    number2 <- handler.queueJob pipeline2
    checkBuild handler number2

    Async.cancel serverThread
    Async.cancel agentThread

    pure ()


testYamlDecoding :: Runner.Service -> IO ()
testYamlDecoding runner = do
    -- Test complex pipeline for parsing
    pipeline <- Yaml.decodeFileThrow "test/pipeline.sample.yaml"
    build <- runner.prepareBuild pipeline
    -- Test simpler pipeline end-to-end
    pipeline2  <- Yaml.decodeFileThrow "test/pipeline2.sample.yaml"
    build2 <- runner.prepareBuild pipeline2
    result2 <- runner.runBuild emptyHooks build2
    -- The test results
    build.state `shouldBe` BuildReady
    length build.pipeline.steps `shouldBe` 2
    result2.state `shouldBe` BuildFinished BuildSucceeded

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
    build <- runner.prepareBuild $ makePipeline
                [ makeStep "First step" "ubuntu" ["date"]
                , makeStep "Second step" "ubuntu" ["uname -r"]
                ]                    
    result <- runner.runBuild emptyHooks build

    result.state `shouldBe` BuildFinished BuildSucceeded
    Map.elems result.completedSteps `shouldBe` [ StepSucceeded, StepSucceeded ]

testRunFailure :: Runner.Service -> IO ()
testRunFailure runner = do
    build <- runner.prepareBuild $ makePipeline
                [ makeStep "Should fail" "ubuntu" ["exit 1"]
                ]                    
    result <- runner.runBuild emptyHooks build

    result.state `shouldBe` BuildFinished BuildFailed
    Map.elems result.completedSteps `shouldBe` [ StepFailed (Docker.ContainerExitCode 1) ]

testSharedWorkSpace :: Docker.Service -> Runner.Service -> IO ()
testSharedWorkSpace docker runner = do
    build <- runner.prepareBuild $ makePipeline
                [ makeStep "Create file" "ubuntu" ["echo hello > test"]
                , makeStep "Read file" "ubuntu" ["cat test"]
                ]
    result <- runner.runBuild emptyHooks build
    result.state `shouldBe` BuildFinished BuildSucceeded
    Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded ]


testLogCollection :: Runner.Service -> IO ()
testLogCollection runner = do

    expected <- newMVar $ Set.fromList ["hello", "world", "Linux"]
    
    let onLog :: Log -> IO ()
        onLog log = do
            remaining <- readMVar expected
            forM_ remaining $ \word -> do
                case ByteString.breakSubstring word log.output of
                    (_, "") -> pure ()
                    _ -> modifyMVar_ expected (pure . Set.delete word)

    let hooks = Runner.Hooks { logCollected = onLog 
                             ,  buildUpdated = \_ -> pure ()
                             }

    build <- runner.prepareBuild $ makePipeline
                [ makeStep "Long step" "ubuntu" ["echo hello", "sleep 2", "echo world"]
                , makeStep "Echo Linux" "ubuntu" ["uname -s"]
                ]
    result <- runner.runBuild hooks build
    result.state `shouldBe` BuildFinished BuildSucceeded 
    Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded ]
    readMVar expected >>= \logs -> logs `shouldBe` Set.empty

testImagePull :: Runner.Service -> IO ()
testImagePull runner = do
    Process.readProcessStdout "docker rmi -f busybox"
    Process.readProcessStdout "docker pull busybox"

    build <- runner.prepareBuild $ makePipeline
                [ makeStep "First step" "busybox" ["date"]
                ]

    result <- runner.runBuild emptyHooks build 

    result.state `shouldBe` BuildFinished BuildSucceeded 
    Map.elems result.completedSteps `shouldBe` [StepSucceeded]

