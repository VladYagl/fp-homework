import Test.Hspec

import ArithTest
-- import OperatorTest
import ParserTest
import VarTest

main :: IO ()
main = do
    hspec evalSpec
    hspec parserSpec
    hspec varSpec
    -- hspec operatorSpec
