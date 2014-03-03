module UnitTestHelper
(
    UnitTest,
    unitTest,
    unitTestGroup
) where

import Test.HUnit
import Test.Framework.Providers.HUnit
import qualified Test.Framework as TestFramework

data UnitTest = UnitTest String Assertion

unitTest :: String -> Assertion -> UnitTest
unitTest = UnitTest

unitTestToTest :: UnitTest -> TestFramework.Test
unitTestToTest (UnitTest name assertion) = testCase name assertion

unitTestsToTests :: [UnitTest] -> [TestFramework.Test]
unitTestsToTests = map unitTestToTest

unitTestGroup :: String -> [UnitTest] -> TestFramework.Test
unitTestGroup name unitTests = TestFramework.testGroup name tests
    where tests = unitTestsToTests unitTests

