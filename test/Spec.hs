module Main where

import RIO
import qualified RIO.ByteString as ByteString
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified System.Process.Typed as Process
import Test.Hspec
import qualified Data.Yaml as Yaml

import Core
import qualified Docker
import qualified Runner
import System.IO
import Core (Build(pipeline))


main :: IO ()
main = hspec do
    docker <- runIO Docker.createService
    runner <- runIO $ Runner.createService docker

    beforeAll cleanupDocker $ describe "Quad CI" do
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
        }

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

testYamlDecoding :: Runner.Service -> IO ()
testYamlDecoding runner = do
    pipeline <- Yaml.decodeFileThrow "test/pipeline.sample.yaml"
    build <- runner.prepareBuild pipeline
    --pipeline2  <- Yaml.decodeFileThrow "test/pipeline2.sample.yaml"
    --build2 <- runner.prepareBuild pipeline2
    --result2 <- runner.runBuild emptyHooks build
    build.state `shouldBe` BuildReady
    length build.pipeline.steps `shouldBe` 2
    --result2.state `shouldBe` BuildFinished BuildSucceeded 

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

    let hooks = Runner.Hooks { logCollected = onLog }

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
    -- Process.readProcessStdout "docker pull busybox"

    build <- runner.prepareBuild $ makePipeline
                [ makeStep "First step" "busybox" ["date"]
                ]

    result <- runner.runBuild emptyHooks build 

    result.state `shouldBe` BuildFinished BuildSucceeded 
    Map.elems result.completedSteps `shouldBe` [StepSucceeded]

