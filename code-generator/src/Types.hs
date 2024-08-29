module Types where

import           Data.Text              (Text)
import           Data.Text.Lazy.Builder (Builder)

import           Asterix.Specs          (Asterix)

type Target = Text
type IsTestSpecs = Bool
type AstSpecsRef = Text
type AstSpecsDate = Text
type CodeGeneratorVersion = Text

type Generator = IsTestSpecs -> AstSpecsRef -> AstSpecsDate
        -> CodeGeneratorVersion -> [Asterix] -> Builder
