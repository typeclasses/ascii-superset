module Main (main) where

import Test.Hspec

import ASCII.Case (Case (..))
import ASCII.CaseRefinement (ASCII'lower, ASCII'upper, asciiCaseUnsafe)
import ASCII.Char (Char (..))
import ASCII.Refinement (ASCII, asciiUnsafe)

import qualified ASCII.Case as Case
import qualified ASCII.Caseless as CC
import qualified ASCII.CaseRefinement as CaseRefinement
import qualified ASCII.Char as ASCII
import qualified ASCII.Lift as Lift
import qualified ASCII.Refinement as Refinement
import qualified ASCII.Superset as Superset

import qualified Data.Foldable as Foldable

import Data.Function ((&))
import Data.Text (Text)
import Data.Word (Word8)
import Numeric.Natural (Natural)
import Prelude

import qualified Data.Char as Unicode

main :: IO ()
main = hspec $ do

    describe "lift" $ do

        it "letter" $ do
            let f x = Lift.lift x :: Word8
            f CapitalLetterA `shouldBe` 65

        it "list" $ do
            let f x = Lift.lift x :: Text
            f [CapitalLetterH, SmallLetterI, ExclamationMark] `shouldBe` "Hi!"

    describe "refinement" $ do

        it "validateChar" $ do
            let f x = Refinement.validateChar x :: Maybe (ASCII Int)
            f (-1) `shouldBe` Nothing
            f 65 `shouldBe` Just (asciiUnsafe 65)
            f 97 `shouldBe` Just (asciiUnsafe 97)
            f 128 `shouldBe` Nothing

        it "fromCharList" $ do
            let f x = Refinement.fromCharList x :: ASCII Text
            f [CapitalLetterH, SmallLetterI, ExclamationMark] `shouldBe` asciiUnsafe "Hi!"

        it "toCharList" $ do
            let f x = Refinement.toCharList
                          (Refinement.substituteString x :: ASCII Text)
            f "Piñata" `shouldBe` [CapitalLetterP, SmallLetterI, Substitute,
                                   SmallLetterA, SmallLetterT, SmallLetterA]

        it "substituteString" $ do
            let f x = Refinement.substituteString x :: ASCII Text
            f "Cristóbal" `shouldBe` asciiUnsafe "Crist\SUBbal"

        it "validateString" $ do
            let f x = Refinement.validateString x :: Maybe (ASCII Text)
            f "Hello" `shouldBe` Just (asciiUnsafe "Hello")
            f "Cristóbal" `shouldBe` Nothing

    describe "case refinement" $ do

        describe "validateChar" $ do

            it "lower" $ do
                let f x = CaseRefinement.validateChar x :: Maybe (ASCII'lower Int)
                f (-1) `shouldBe` Nothing
                f 65 `shouldBe` Nothing
                f 97 `shouldBe` Just (asciiCaseUnsafe 97)
                f 128 `shouldBe` Nothing

            it "upper" $ do
                let f x = CaseRefinement.validateChar x :: Maybe (ASCII'upper Int)
                f (-1) `shouldBe` Nothing
                f 65 `shouldBe` Just (asciiCaseUnsafe 65)
                f 97 `shouldBe` Nothing
                f 128 `shouldBe` Nothing

        describe "fromCaselessCharList" $ do

            it "lower" $ do
                let f x = CaseRefinement.fromCaselessCharList x :: ASCII'lower Text
                f [CC.LetterH, CC.LetterI, CC.ExclamationMark] `shouldBe` asciiCaseUnsafe "hi!"

            it "upper" $ do
                let f x = CaseRefinement.fromCaselessCharList x :: ASCII'upper Text
                f [CC.LetterH, CC.LetterI, CC.ExclamationMark] `shouldBe` asciiCaseUnsafe "HI!"

        describe "toCaselessCharList" $ do

            it "lower" $ do
                let f x = CaseRefinement.toCaselessCharList (x :: ASCII'lower Text)
                f (asciiCaseUnsafe "hi!") `shouldBe` [CC.LetterH, CC.LetterI, CC.ExclamationMark]

            it "upper" $ do
                let f x = CaseRefinement.toCaselessCharList (x :: ASCII'upper Text)
                f (asciiCaseUnsafe "HI!") `shouldBe` [CC.LetterH, CC.LetterI, CC.ExclamationMark]

        describe "substituteString" $ do

            it "lower" $ do
                let f x = CaseRefinement.substituteString x :: ASCII'lower Text
                f "Cób" `shouldBe` asciiCaseUnsafe "\SUB\SUBb"

            it "upper" $ do
                let f x = CaseRefinement.substituteString x :: ASCII'upper Text
                f "Cób" `shouldBe` asciiCaseUnsafe "C\SUB\SUB"

        describe "validateString" $ do

            it "lower" $ do
                let f x = CaseRefinement.validateString x :: Maybe (ASCII'lower Text)
                f "hello" `shouldBe` Just (asciiCaseUnsafe "hello")
                f "Hello" `shouldBe` Nothing

            it "upper" $ do
                let f x = CaseRefinement.validateString x :: Maybe (ASCII'upper Text)
                f "HELLO" `shouldBe` Just (asciiCaseUnsafe "HELLO")
                f "Hello" `shouldBe` Nothing

    describe "case conversion" $ do

        describe "toCaseChar" $ do
            let check :: forall a. Eq a => Superset.CharSuperset a => Expectation
                check =
                    ([UpperCase, LowerCase] & Foldable.all (\c ->
                        ASCII.allCharacters & Foldable.all (\x ->
                            Superset.toCaseChar c (Superset.fromChar @a x)
                                == Superset.fromChar @a (Case.toCase c x)
                    ))) `shouldBe` True
            it "ASCII.Char" $ check @ASCII.Char
            it "Unicode.Char" $ check @Unicode.Char
            it "Natural" $ check @Natural
            it "Int" $ check @Int
            it "Word8" $ check @Word8

        describe "toCaseString" $ do
            it "Text" $ do
                let x = "012 abc DEF ﬓ" :: Text
                Superset.toCaseString UpperCase x `shouldBe` "012 ABC DEF ﬓ"
                Superset.toCaseString LowerCase x `shouldBe` "012 abc def ﬓ"

        describe "refineStringToCase" $ do
            it "Text" $ do
                let x = asciiUnsafe "Hi!" :: ASCII Text
                CaseRefinement.refineStringToCase x `shouldBe` (asciiCaseUnsafe "HI!" :: ASCII'upper Text)
                CaseRefinement.refineStringToCase x `shouldBe` (asciiCaseUnsafe "hi!" :: ASCII'lower Text)
