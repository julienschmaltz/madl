{-# LANGUAGE OverloadedStrings #-}

module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.HUnit.Base (Test(TestLabel))

import qualified Data.HashMap as Hash
import Data.Maybe (fromMaybe)

import Utils.File
import Utils.Text

import Madl.Network as Madl
import Madl.Cycles
import Madl.Base (isConsistent)
import qualified Examples

main :: IO()
main = defaultMain =<< tests

tests :: IO [Test.Framework.Test]
tests = (return . hUnitTestToTests . TestList) =<< fmap concat (sequence alltests) where
    alltests :: [IO [Test.HUnit.Base.Test]]
    alltests = map getTests testcases

pathToTests :: FilePath
pathToTests = "examples/"

testcases :: [(FilePath, Bool)]
testcases =
    [ ("internship/gen0_no_reorder.madl", True)
    , ("internship/gen0_reorder.madl", True)
    , ("internship/gen0_reorder_cc.madl", True)
    , ("bugs/issue68.madl", True)
    , ("solvedBugs/issue98.madl", True)
    , ("solvedBugs/issue133.madl", True)
    , ("solvedBugs/issue129.madl", True)
    , ("solvedBugs/issue97.madl", True)
    , ("solvedBugs/issue106.madl", True)
    , ("solvedBugs/issue110.madl", True)
    , ("solvedBugs/issue119.madl", True)
    , ("solvedBugs/issue120.madl", True)
    , ("solvedBugs/issue121.madl", True)
    , ("solvedBugs/issue125.madl", True)
    , ("solvedBugs/issue127.madl", True)
    , ("solvedBugs/issue132.madl", True)
    , ("solvedBugs/issue149-0.madl", True)
    , ("solvedBugs/issue149-1.madl", True)
    , ("solvedBugs/issue149-2.madl", True)
    , ("solvedBugs/issue167.madl", True)
    , ("solvedBugs/issue174.madl", True)
    , ("solvedBugs/issue183.madl", False) -- TODO(snnw) Cycles Automaton (js) 
    , ("solvedBugs/issue184.madl", False) -- TODO(snnw) Cycles Automaton (js) 
    , ("spidergon/spSpidergon4.madl", True) 
    , ("spidergon/spSpidergon_distributed_routing.madl", True)
    -- , ("spidergon/spSpidergon_master_slave.madl", True)
    , ("spidergon/spSpidergon_master_slave_4.madl", True)
    , ("spidergon/spSpidergon_src_routing.madl", True)
    , ("ligero/model00.madl", True)
    , ("mesh_xy/mesh_xy.madl", True)
    , ("mesh_xy/mesh_xy_22.madl", True)
    , ("mesh_xy/mesh_xy_22_ms.madl", True)
    , ("mesh_xy/mesh_xy_wc.madl", True)
    , ("mesh_xy/mesh_xy_wc_ms.madl", True)
    -- , ("mesh_xy/mesh_xy_ms.madl", True)
    , ("generatedExamples/allprim.madl", True)
    , ("generatedExamples/allprimDL.madl", True)
    , ("generatedExamples/itn.madl", True)
    , ("generatedExamples/itn2.madl", True)
    , ("generatedExamples/mergeblock.madl", True)
    , ("generatedExamples/msn.madl", True)
    , ("generatedExamples/redblue.madl", True)
    , ("generatedExamples/redblueDL.madl", True)
    , ("generatedExamples/rtn-22F.madl", True)
    , ("generatedExamples/rtn-22T.madl", True)
    , ("generatedExamples/rtn-23F.madl", True)
    , ("generatedExamples/rtn-23T.madl", True)
    , ("generatedExamples/rtn-32F.madl", True)
    , ("generatedExamples/rtn-32T.madl", True)
    , ("generatedExamples/rtn-34F.madl", True)
    , ("generatedExamples/rtn-34T.madl", True)
    , ("generatedExamples/rtn-45F.madl", True)
    , ("generatedExamples/rtn-45T.madl", True)
    -- , ("generatedExamples/rtn2-2F.madl", True) -- TODO(snnw) the network has a cycle!
    -- , ("generatedExamples/rtn2-2T.madl", True) -- TODO(snnw) the network has a cycle!
    -- , ("generatedExamples/rtn2-3F.madl", True) --excluded because merge with 3 inputs is not supported (#78)
    -- , ("generatedExamples/rtn2-3T.madl", True)
    , ("generatedExamples/rtn3-22F.madl", True)
    , ("generatedExamples/rtn3-22T.madl", True)
    , ("generatedExamples/rtn3-23F.madl", True)
    , ("generatedExamples/rtn3-23T.madl", True)
    , ("generatedExamples/rtn3-32F.madl", True)
    , ("generatedExamples/rtn3-32T.madl", True)
    , ("generatedExamples/sfn.madl", True)
    , ("generatedExamples/smn.madl", True)
    , ("generatedExamples/smt.madl", True)
    , ("generatedExamples/smt2.madl", True)
    , ("generatedExamples/smt3.madl", True)
    , ("generatedExamples/ssn.madl", True)
    , ("generatedExamples/stn.madl", True)
    , ("generatedExamples/twoagents10.madl", True)
    , ("generatedExamples/twoagents9.madl", True)
    , ("generatedExamples/twoagents8.madl", True)
    , ("simpleTests/allprimWithVars.madl", True)
    , ("simpleTests/allprimWithVarsDL.madl", True)
    , ("simpleTests/constswidletn.madl", True)
    , ("simpleTests/constswidletnDL.madl", True)
    , ("simpleTests/constswtn.madl", True)
    , ("simpleTests/constswtn2.madl", True)
    , ("simpleTests/constswtnDL.madl", True)
    , ("simpleTests/dstn.madl", True)
    , ("simpleTests/fcjtn01.madl", True)
    , ("simpleTests/fcjtn01DL.madl", True)
    , ("simpleTests/ifThenElse1.madl", True)
    , ("simpleTests/ifThenElse2.madl", True)
    , ("simpleTests/int_switch.madl", True)
    , ("simpleTests/lbtn.madl", True)
    , ("simpleTests/lbtnDL.madl", True)
    , ("simpleTests/mtn01.madl", True)
    , ("simpleTests/mtn01DL.madl", True)
    , ("simpleTests/mtn02DL.madl", True)
    , ("simpleTests/processExample.madl", False) -- TODO(snnw) Cycles Automaton
    , ("simpleTests/processExampleWithFunction.madl", False) -- TODO(snnw) Cycles Automaton
    , ("simpleTests/processWithVariable.madl", False) -- TODO(snnw) Cycles Automaton
    , ("simpleTests/simpleMacro.madl", True)
    , ("simpleTests/testInclude.madl", True)
    , ("simpleTests/deadSource.madl", True)
    , ("simpleTests/testing_cross_invs_01.madl", False) -- TODO(snnw) Cycles Automaton
    , ("simpleTests/testing_cross_invs_02.madl", False) -- TODO(snnw) Cycles Automaton
    , ("simpleTests/testing_cross_invs_03.madl", False) -- TODO(snnw) Cycles Automaton
    , ("simpleTests/testing_cross_invs_04.madl", False) -- TODO(snnw) Cycles Automaton
    , ("simpleTests/tn_0000.madl", True)
    , ("simpleTests/tn_0001.madl", True)
    , ("simpleTests/tn_0002.madl", True)
    , ("simpleTests/tn_0003.madl", True)
    , ("simpleTests/tn_0004.madl", True)
    , ("simpleTests/tn_0005.madl", True)
    , ("simpleTests/tn_0006.madl", True)
    , ("simpleTests/tn_0007.madl", True)
    , ("simpleTests/tn_0008.madl", True)
    , ("simpleTests/tn_0009.madl", True)
    , ("simpleTests/tn_0010.madl", True)
    , ("simpleTests/tn_0011.madl", True)
    -- , ("prodcon_v00.madl", True) -- TODO(snnw) the network has a cycle!
    , ("prodcon_v01.madl", True)
    , ("twoagents2_2.madl", True)
    , ("vcwbn.madl", True)
    ]

unittests :: [(Text, Madl.MadlNetwork -> Assertion)]
unittests = [
      ("validNetwork", testValidNetwork)
    , ("unfoldMacros", testUnfoldMacros)
    , ("unfoldMacrosIdentity", testUnfoldMacrosIdentity)
    , ("consistency of network", testConsistency)
    , ("combinatorialCycleCheck", testCycles)
    ]

getTests :: (FilePath, Bool) -> IO [Test.HUnit.Base.Test]
getTests (file, runCycleCheck) = do
    netOrError <- readNetworkFromFile defaultReadOptions (pathToTests ++ file)
    let tstcase test = case netOrError of
            Left err -> assertFailure $ "Error in network " ++file ++": " ++ err
            Right net -> test net
    return $ map (\(name, test) -> TestLabel (file ++ "_" ++ (utxt name)) (TestCase $ tstcase test)) . (if runCycleCheck then id else reverse . drop 1 . reverse) $ unittests

testValidNetwork :: Madl.MadlNetwork -> Assertion
testValidNetwork = assertString . fromMaybe "" . Madl.validMaDLNetwork

testUnfoldMacros :: Madl.MadlNetwork -> Assertion
testUnfoldMacros net = assertString . (fromMaybe "") $ Madl.validMaDLNetwork net' where
    net' :: Madl.MadlNetwork
    net' = Madl.cast' $ Madl.unflatten net_flat
    net_flat :: Madl.FlatFlattenedNetwork
    net_flat = Madl.unfoldMacros net

testUnfoldMacrosIdentity :: Madl.MadlNetwork -> Assertion
testUnfoldMacrosIdentity net = case Madl.getMacros net of
    [] -> assertEqual "" net (Madl.cast' $ Madl.unflatten net_flat) where
        net_flat :: Madl.FlatFlattenedNetwork
        net_flat = Madl.unfoldMacros net
    _ -> assertBool "" True

testConsistency :: Madl.MadlNetwork -> Assertion
testConsistency net = do c1; c2 where
    c1 = assertBool "pre unfold consistent" (isConsistent net)
    c2 = assertBool "post unfold consistent" (isConsistent expanded)
    expanded :: Madl.FlatFlattenedNetwork
    expanded = Madl.unfoldMacros net

testCycles :: Madl.MadlNetwork -> Assertion
testCycles net = case checkCycles net' of
    [] -> assertBool "" True
    _cycles -> assertBool "The net contains combinatorial cycles" False
    where
        net' :: Madl.FlattenedNetwork
        net' = Madl.unflatten net_flat
        net_flat :: Madl.FlatFlattenedNetwork
        net_flat = Madl.unfoldMacros net
