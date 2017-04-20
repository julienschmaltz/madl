{-# LANGUAGE OverloadedStrings, CPP #-}

module TestMain (tests) where

import Distribution.TestSuite

import Utils.File
import Madl.Network
import Invariants.Invariants (getInvariants)
import qualified Deadlock.Runner as Runner

#ifdef EIGHT_ROUTER
import qualified Data.HashMap as Hash
import Utils.Text
import qualified Examples
#endif

tests :: IO [Test]
tests = return $
        map Test (concatMap testInstances testCases)
#ifdef EIGHT_ROUTER
    ++ testEightRouter
#endif

data DeadlockType = NoDeadlock | UnreachableDeadlock | Deadlock
data TestMode = TestMode {
    runSmt :: Bool,
    runNuxmv :: Bool,
    runAbc :: Bool
}

off, smtOnly, _nuxmvOnly, _abcOnly, _noSmt, _noNuxmv, noAbc, allModes :: TestMode 
off        = TestMode False False False
smtOnly    = TestMode True  False False
_nuxmvOnly = TestMode False True  True
_abcOnly   = TestMode False False True
_noSmt     = TestMode False True  True
_noNuxmv    = TestMode True  False True
noAbc     = TestMode True  True  False
allModes   = TestMode True  True  True

data TestCase = TestCase {
    testName :: String,
    testFilePath :: FilePath,
    testExpectedResult :: DeadlockType,
    testMode :: TestMode
}

pathToTests :: FilePath
pathToTests = "examples/"

testCases :: [TestCase]
testCases = [
      TestCase "prodcon_v00"     "prodcon_v00.madl"     Deadlock   off      -- "\"(\"\n\"(\"\n\"unsat\"\n\"sat\\n\"\n"
    , TestCase "prodcon_v01"     "prodcon_v01.madl"     NoDeadlock smtOnly
    , TestCase "twoagents2_2"    "twoagents2_2.madl"    NoDeadlock allModes
    , TestCase "vcwbn"           "vcwbn.madl"           NoDeadlock allModes
    ]
    ++ map (\tc -> tc{testName = "generatedExamples-" ++ testName tc, testFilePath = "generatedExamples/" ++ testFilePath tc}) [
      TestCase "allprim"     "allprim.madl"     NoDeadlock          allModes
    , TestCase "allprimDL"   "allprimDL.madl"   Deadlock            allModes
    , TestCase "itn"         "itn.madl"         NoDeadlock          allModes
    , TestCase "itn2"        "itn2.madl"        NoDeadlock          allModes
    , TestCase "mergeblock"  "mergeblock.madl"  NoDeadlock          allModes
    , TestCase "msn"         "msn.madl"         Deadlock            allModes
    , TestCase "redblue"     "redblue.madl"     UnreachableDeadlock allModes
    , TestCase "redblueDL"   "redblueDL.madl"   Deadlock            allModes
    , TestCase "rtn-22F"     "rtn-22F.madl"     UnreachableDeadlock allModes
    , TestCase "rtn-22T"     "rtn-22T.madl"     Deadlock            allModes
    , TestCase "rtn-23F"     "rtn-23F.madl"     NoDeadlock          allModes
    , TestCase "rtn-23T"     "rtn-23T.madl"     NoDeadlock          allModes
    , TestCase "rtn-32F"     "rtn-32F.madl"     UnreachableDeadlock allModes
    , TestCase "rtn-32T"     "rtn-32T.madl"     Deadlock            allModes
    , TestCase "rtn-34F"     "rtn-34F.madl"     NoDeadlock          allModes
    , TestCase "rtn-34T"     "rtn-34T.madl"     NoDeadlock          allModes
    , TestCase "rtn-45F"     "rtn-45F.madl"     NoDeadlock          noAbc    -- ABC TO
    , TestCase "rtn-45T"     "rtn-45T.madl"     NoDeadlock          noAbc    -- ABC TO
    , TestCase "rtn2-2F"     "rtn2-2F.madl"     NoDeadlock          off      -- prints "\"(\"\n\"(\"\n\"unsat\"\n\"sat\\n\"\n"
    , TestCase "rtn2-2T"     "rtn2-2T.madl"     NoDeadlock          off      -- prints "\"(\"\n\"(\"\n\"unsat\"\n\"sat\\n\"\n" 
    , TestCase "rtn2-3F"     "rtn2-3F.madl"     Deadlock            off -- Merge with 3input needed. (issue #78)
    , TestCase "rtn2-3T"     "rtn2-3T.madl"     Deadlock            off -- Merge with 3input needed. (issue #78)
    , TestCase "rtn3-22F"    "rtn3-22F.madl"    NoDeadlock          allModes
    , TestCase "rtn3-22T"    "rtn3-22T.madl"    NoDeadlock          allModes
    , TestCase "rtn3-23F"    "rtn3-23F.madl"    NoDeadlock          allModes
    , TestCase "rtn3-23T"    "rtn3-23T.madl"    NoDeadlock          allModes
    , TestCase "rtn3-32F"    "rtn3-32F.madl"    UnreachableDeadlock allModes
    , TestCase "rtn3-32T"    "rtn3-32T.madl"    Deadlock            allModes
    , TestCase "sfn"         "sfn.madl"         NoDeadlock          allModes
    , TestCase "smn"         "smn.madl"         Deadlock            allModes
    , TestCase "smt"         "smt.madl"         NoDeadlock          allModes
    , TestCase "smt2"        "smt2.madl"        NoDeadlock          allModes
    , TestCase "smt3"        "smt3.madl"        NoDeadlock          allModes
    , TestCase "ssn"         "ssn.madl"         NoDeadlock          allModes
    , TestCase "stn"         "stn.madl"         NoDeadlock          allModes
    , TestCase "twoagents10" "twoagents10.madl" NoDeadlock          allModes
    , TestCase "twoagents9"  "twoagents9.madl"  UnreachableDeadlock allModes 
    , TestCase "twoagents8"  "twoagents8.madl"  Deadlock            allModes 
    ]
    ++ map (\tc -> tc{testName = "simpleTests-" ++ testName tc, testFilePath = "simpleTests/" ++ testFilePath tc}) [
      TestCase "allprimWithVars"   "allprimWithVars.madl"   NoDeadlock allModes
    , TestCase "allprimWithVarsDL" "allprimWithVarsDL.madl" Deadlock   allModes
    , TestCase "constswidletn"     "constswidletn.madl"     NoDeadlock allModes
    , TestCase "constswidletnDL"   "constswidletnDL.madl"   Deadlock   allModes
    , TestCase "constswtn"         "constswtn.madl"         NoDeadlock allModes
    , TestCase "constswtn2"        "constswtn2.madl"        NoDeadlock allModes
    , TestCase "constswtnDL"       "constswtnDL.madl"       Deadlock   allModes
    , TestCase "dstn"              "dstn.madl"              Deadlock   allModes
    , TestCase "fcjtn01"           "fcjtn01.madl"           NoDeadlock allModes
    , TestCase "fcjtn01DL"         "fcjtn01DL.madl"         Deadlock   allModes
    , TestCase "int_switch"        "int_switch.madl"        NoDeadlock allModes
    , TestCase "lbtn"              "lbtn.madl"              NoDeadlock allModes
    , TestCase "lbtnDL"            "lbtnDL.madl"            Deadlock   allModes
    , TestCase "mtn01"             "mtn01.madl"             NoDeadlock allModes
    , TestCase "mtn01DL"           "mtn01DL.madl"           Deadlock   allModes
    , TestCase "mtn02DL"           "mtn02DL.madl"           Deadlock   allModes
    , TestCase "processExample"    "processExample.madl"    NoDeadlock allModes
    , TestCase "simpleMacro"       "simpleMacro.madl"       NoDeadlock allModes
    , TestCase "testInclude"       "testInclude.madl"       NoDeadlock allModes
--    , TestCase "tn_0005"           "tn_0005.madl"           NoDeadlock allModes
    , TestCase "deadsource"        "deadSource.madl"        Deadlock   allModes
    , TestCase "tn_0000"           "tn_0000.madl"           NoDeadlock allModes
    , TestCase "tn_0001"           "tn_0001.madl"           NoDeadlock allModes
    , TestCase "tn_0002"           "tn_0002.madl"           NoDeadlock allModes
    , TestCase "tn_0003"           "tn_0003.madl"           NoDeadlock allModes
    , TestCase "tn_0004"           "tn_0004.madl"           NoDeadlock allModes
    , TestCase "tn_0005"           "tn_0005.madl"           Deadlock   allModes
    , TestCase "tn_0006"           "tn_0006.madl"           NoDeadlock allModes
    , TestCase "tn_0007"           "tn_0007.madl"           NoDeadlock allModes
    , TestCase "tn_0008"           "tn_0008.madl"           NoDeadlock allModes
    , TestCase "tn_0009"           "tn_0009.madl"           Deadlock   allModes
    , TestCase "tn_0010"           "tn_0011.madl"           Deadlock   allModes
    , TestCase "tn_0011"           "tn_0011.madl"           Deadlock   allModes
    ]
    ++ map (\tc -> tc{testName = "solvedBugs-" ++ testName tc, testFilePath = "solvedBugs/" ++ testFilePath tc}) [
      TestCase "issue97"    "issue97.madl"    NoDeadlock allModes 
    , TestCase "issue98"    "issue98.madl"    NoDeadlock allModes
    , TestCase "issue106"   "issue106.madl"   NoDeadlock allModes
    , TestCase "issue110"   "issue110.madl"   NoDeadlock allModes
    , TestCase "issue119"   "issue119.madl"   Deadlock   allModes
    , TestCase "issue120"   "issue120.madl"   NoDeadlock allModes
    , TestCase "issue121"   "issue121.madl"   NoDeadlock allModes    
    , TestCase "issue125"   "issue125.madl"   Deadlock   allModes
    , TestCase "issue127"   "issue127.madl"   Deadlock   allModes
    , TestCase "issue129"   "issue129.madl"   NoDeadlock allModes
    , TestCase "issue132"   "issue132.madl"   Deadlock   allModes
    , TestCase "issue133"   "issue133.madl"   NoDeadlock allModes
    , TestCase "issue149-0" "issue149-0.madl" NoDeadlock allModes
    , TestCase "issue149-1" "issue149-1.madl" NoDeadlock allModes
    , TestCase "issue149-2" "issue149-2.madl" Deadlock   allModes 
    , TestCase "issue167"   "issue167.madl"   NoDeadlock allModes
    , TestCase "issue174"   "issue174.madl"   NoDeadlock allModes               
    , TestCase "issue183"   "issue183.madl"   NoDeadlock allModes
    , TestCase "issue184"   "issue184.madl"   Deadlock   allModes
    ]
    ++ map (\tc -> tc{testName = "spidergon-" ++ testName tc, testFilePath = "spidergon/" ++ testFilePath tc}) [
      TestCase "spSpidergon4"            "spSpidergon4.madl"                           NoDeadlock smtOnly
    , TestCase "spSpidergon_dist"        "spSpidergon_distributed_routing.madl"        Deadlock   smtOnly
    -- , TestCase "spSpidergon_ms"          "spSpidergon_master_slave.madl"               Deadlock   smtOnly  -- takes too much time with ABC
    , TestCase "spSpidergon_ms_4"        "spSpidergon_master_slave_4.madl"             Deadlock   smtOnly  -- reachable deadlock but proven unreachable (issue #177)
    ]
    ++ map (\tc -> tc{testName = "meshXY-" ++ testName tc, testFilePath = "mesh_xy/" ++ testFilePath tc}) [
        TestCase "meshXY22"          "mesh_xy_22.madl"                      NoDeadlock allModes
    ,   TestCase "meshXY22MS"        "mesh_xy_22_ms.madl"                   Deadlock   smtOnly -- takes too long for reachability
    ,   TestCase "meshXYwc"          "mesh_xy_wc.madl"                      NoDeadlock off
    ]
    ++ map (\tc -> tc{testName = "cross-invs-" ++ testName tc, testFilePath = "simpleTests/" ++ testFilePath tc}) [
        TestCase "cross-invs-04"     "testing_cross_invs_04.madl"           NoDeadlock            allModes
    ,   TestCase "cross-invs-04-DL"  "testing_cross_invs_04_DL.madl"        UnreachableDeadlock   allModes
    ,   TestCase "cross-invs-02"     "testing_cross_invs_02.madl"           UnreachableDeadlock   allModes
    ]
testInstances :: TestCase -> [TestInstance]
testInstances testcase = smtTest ++ nuxmvTest ++ abcTest where
    smtTest   = if not (runSmt $ testMode testcase)   then [] else [smtTestInstance]
    nuxmvTest = if not (runNuxmv $ testMode testcase) then [] else [
#ifdef NUXMV
            nuxmvTestInstance
#endif
        ]
    abcTest = if not (runAbc $ testMode testcase) then [] else [
#ifdef TEST_ABC
            abcTestInstance
#endif
        ]

    smtTestInstance = TestInstance {
        run = createTest smtExpected smtRunner,
        name = testName testcase ++ "-smt",
        tags = [],
        options = [],
        setOption = \_ _ -> Right smtTestInstance
    }
    smtExpected = case testExpectedResult testcase of NoDeadlock -> False; _ -> True;
    smtRunner = Runner.defaultOptions{ Runner.argRunMode=Runner.SmtOnly }

#ifdef NUXMV
    nuxmvTestInstance = TestInstance {
        run = createTest nuxmvExpected nuxmvRunner,
        name = testName testcase ++ "-nuxmv",
        tags = [],
        options = [],
        setOption = \_ _ -> Right nuxmvTestInstance
    }
    nuxmvExpected = case testExpectedResult testcase of Deadlock -> True; _ -> False;
    nuxmvRunner = Runner.defaultOptions{ Runner.argRunMode=Runner.ReachabilityOnly }
#endif

#ifdef TEST_ABC
    abcTestInstance = TestInstance {
        run = createTest abcExpected abcRunner,
        name = testName testcase ++ "-abc",
        tags = [],
        options = [],
        setOption = \_ _ -> Right abcTestInstance
    }
    abcExpected = case testExpectedResult testcase of Deadlock -> True; _ -> False;
    abcRunner = Runner.defaultOptions{ Runner.argRunMode=Runner.ReachabilityOnly,
        Runner.argNuxmvOptions= (Runner.argNuxmvOptions Runner.defaultOptions){
            Runner.reachabilityEngine = Runner.ABC Runner.PDR
        }}
#endif

    createTest :: Bool -> Runner.CommandLineOptions -> IO Progress
    createTest expected opts = do
        network <- readColoredNetworkFromFile defaultReadOptions (pathToTests ++ testFilePath testcase)
        case network of
            Left err -> return . Finished . Error $ err
            Right net -> runTest net opts expected

runTest :: ColoredNetwork -> Runner.CommandLineOptions -> Bool -> IO Progress
runTest net opts expected = do
    let invs = getInvariants net
        nfqs = Runner.notFullQueues net (Runner.argSMTSolver opts) invs
    nfqs' <- nfqs
    r <- Runner.runDeadlockDetection net opts invs nfqs'
    case (expected, r) of
        (_, Left err) -> return . Finished $ Error err
        (True, Right (False,_)) -> return . Finished $ Fail "A reachable deadlock was not found."
        (False, Right (True,_)) -> return . Finished $ Fail "Identified an unexpected deadlock."
        _ -> return $ Finished Pass

#ifdef EIGHT_ROUTER
testEightRouter :: [Test]
testEightRouter = map (Test . eightRouterInstance) ts where
    ts = [("8router", False), ("8routerDL", True)]

eightRouterInstance :: (Text, Bool) -> TestInstance
eightRouterInstance (netName, expected) = testInstance where
    testInstance = TestInstance {
        run = run8routerTest,
        name = utxt netName,
        tags = [],
        options = [],
        setOption = \_ _ -> Right testInstance
    }
    run8routerTest :: IO Progress
    run8routerTest = case Hash.lookup netName Examples.networkMapTyped of
        Nothing -> return $ Finished $ Error ("Couldn't find network "++utxt netName++".")
        Just net -> do
            let invs = getInvariants net
                nfqs = Runner.notFullQueues net (Runner.argSMTSolver opts) invs
            r <- Runner.runDeadlockDetection net (Runner.defaultOptions{ Runner.argRunMode=Runner.SmtOnly }) invs nfqs
                (_, Left err) -> return . Finished $ Error err
                (True, Right False) -> return . Finished $ Fail "A reachable deadlock was not found."
                (False, Right True) -> return . Finished $ Fail "Identified an unexpected deadlock."
                _ -> return $ Finished Pass
#endif
