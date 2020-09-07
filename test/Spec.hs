import Test.HUnit
import Syntax
import Parser

main = runTestTT tests

tests = TestList 
    [ TestLabel "testParseVar" testParseVar
    , TestLabel "testParseApp" testParseApp
    , TestLabel "testParseAbstraction" testParseAbstraction
    ]

testParseVar = TestCase 
    ( assertEqual "parsing var:" 
        (parse variable "x")
        (Right $ Var "x")
    )

testParseApp = TestCase
    ( assertEqual "parsing app:"
        (parse application "(x y)")
        (Right $ App (Var "x") (Var "y"))
    )

testParseAbstraction = TestCase
    ( assertEqual "parsing abstraction:"
        (parse abstraction "\\x. y)")
        (Right $ Lam "x" (Var "y"))
    )