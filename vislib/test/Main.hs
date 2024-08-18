{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Test.HUnit
import VisLib.Shader.TH
import System.Exit (exitFailure)
import Control.Monad

test1 :: Test
test1 = TestCase $ do
    let a = 3 :: Int
    [shader|test $a lol|] @?= "test 3 lol"

genShader :: Float -> String
genShader i = [shader|
void main() {
#version 330 core

out vec4 color;
in vec3 v_color;

void main() {
    color = vec4(v_color*$i, 1.0);
}
}|]

test2 :: Test
test2 = TestCase $ do
    genShader 0.5 @?= "\nvoid main() {\n#version 330 core\n\nout vec4 color;\nin vec3 v_color;\n\nvoid main() {\n    color = vec4(v_color*0.5, 1.0);\n}\n}"

main :: IO ()
main = do
    count <- runTestTT $ TestList [test1, test2]
    when (errors count > 0 || failures count > 0) exitFailure
