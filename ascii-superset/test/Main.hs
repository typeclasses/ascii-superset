module Main (main) where

import ASCII.Case (Case (..))
import ASCII.Case qualified as Case
import ASCII.CaseRefinement (ASCII'lower, ASCII'upper, asciiCaseUnsafe)
import ASCII.CaseRefinement qualified as CaseRefinement
import ASCII.Caseless qualified as CC
import ASCII.Char (Char (..))
import ASCII.Char qualified as ASCII
import ASCII.Refinement (ASCII, asciiUnsafe)
import ASCII.Refinement qualified as Refinement
import ASCII.Superset (fromChar, fromCharList)
import ASCII.Superset qualified as Superset
import Data.Char qualified as Unicode
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.Text (Text)
import Data.Word (Word8)
import Numeric.Natural (Natural)
import Test.Hspec
import Prelude

main :: IO ()
main = hspec $ do
  describe "fromChar" $ do
    it "letter" $ do
      let f x = fromChar x :: Word8
      f CapitalLetterA `shouldBe` 65

    it "to unicode" $ do
      let f = fromChar
      f CapitalLetterA `shouldBe` 'A'

    it "id" $ do
      let f x = fromChar (x :: ASCII.Char) :: ASCII.Char
      f CapitalLetterA `shouldBe` CapitalLetterA

  describe "refinement" $ do
    it "validateChar" $ do
      let f x = Refinement.validateChar x :: Maybe (ASCII Int)
      f (-1) `shouldBe` Nothing
      f 65 `shouldBe` Just (asciiUnsafe 65)
      f 97 `shouldBe` Just (asciiUnsafe 97)
      f 128 `shouldBe` Nothing

    describe "fromCharList" $ do
      it "Text" $ do
        let f x = fromCharList x :: Text
        f [CapitalLetterH, SmallLetterI, ExclamationMark] `shouldBe` "Hi!"

      it "ASCII Text" $ do
        let f x = Refinement.fromCharList x :: ASCII Text
        f [CapitalLetterH, SmallLetterI, ExclamationMark] `shouldBe` asciiUnsafe "Hi!"

    it "toCharList" $ do
      let f x =
            Refinement.toCharList
              (Refinement.substituteString x :: ASCII Text)
      f "Piñata"
        `shouldBe` [ CapitalLetterP,
                     SmallLetterI,
                     Substitute,
                     SmallLetterA,
                     SmallLetterT,
                     SmallLetterA
                   ]

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
            ( [UpperCase, LowerCase]
                & Foldable.all
                  ( \c ->
                      ASCII.allCharacters
                        & Foldable.all
                          ( \x ->
                              Superset.toCaseChar c (fromChar @a x)
                                == fromChar @a (Case.toCase c x)
                          )
                  )
            )
              `shouldBe` True
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

    describe "toCasefulChar" $ do
      describe "lower case" $ do
        let a :: Superset.ToCasefulChar 'LowerCase char => char
            a = Superset.toCasefulChar @'LowerCase CC.LetterA

        it "can be Char" $ a `shouldBe` SmallLetterA
        it "can be superset type" $ a `shouldBe` 'a'
        it "can be case-refined type" $ a `shouldBe` (asciiCaseUnsafe 'a' :: ASCII'lower Unicode.Char)

      describe "upper case" $ do
        let a :: Superset.ToCasefulChar 'UpperCase char => char
            a = Superset.toCasefulChar @'UpperCase CC.LetterA

        it "can be Char" $ a `shouldBe` CapitalLetterA
        it "can be superset type" $ a `shouldBe` 'A'
        it "can be case-refined type" $ a `shouldBe` (asciiCaseUnsafe 'A' :: ASCII'upper Unicode.Char)

    describe "toCasefulString" $ do
      describe "lower case" $ do
        let x :: Superset.ToCasefulString 'LowerCase string => string
            x = Superset.toCasefulString @'LowerCase [CC.LetterH, CC.LetterI, CC.ExclamationMark]

        it "can be [Char]" $ x `shouldBe` [SmallLetterH, SmallLetterI, ExclamationMark]
        it "can be superset type" $ x `shouldBe` ("hi!" :: Text)
        it "can be case-refined type" $ x `shouldBe` (asciiCaseUnsafe "hi!" :: ASCII'lower Text)

      describe "upper case" $ do
        let x :: Superset.ToCasefulString 'UpperCase string => string
            x = Superset.toCasefulString @'UpperCase [CC.LetterH, CC.LetterI, CC.ExclamationMark]

        it "can be [Char]" $ x `shouldBe` [CapitalLetterH, CapitalLetterI, ExclamationMark]
        it "can be superset type" $ x `shouldBe` ("HI!" :: Text)
        it "can be case-refined type" $ x `shouldBe` (asciiCaseUnsafe "HI!" :: ASCII'upper Text)
