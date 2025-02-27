module Test.Registry.Foreign.SPDX (spec) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Registry.Foreign.SPDX as SPDX
import Registry.License as License
import Test.Assert as Assert
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.it "Suggests replacements for fixable malformed licenses" do
    for_ fixable \(Tuple input output) ->
      case SPDX.fuzzyMatchLicense input of
        Nothing ->
          Assert.fail $ Array.fold [ input, " had no matches, but should have matched ", output ]
        Just matches -> do
          let firstMatch = License.print $ NonEmptyArray.head matches
          unless (firstMatch == output) do
            Assert.fail $ Array.fold [ input, " matched with ", firstMatch, " but should have matched ", output ]

  Spec.it "Does not suggest replacements for unfixable malformed licenses" do
    for_ unfixable \input ->
      for_ (SPDX.fuzzyMatchLicense input) \matches ->
        Assert.fail $ Array.fold
          [ input
          , " should have no matches, but matched: "
          , String.joinWith ", " (NonEmptyArray.toArray (map License.print matches))
          ]

fixable :: Array (Tuple String String)
fixable =
  [ Tuple "Apache" "Apache-1.0"
  , Tuple "Apache-2" "Apache-2.0"
  , Tuple "Apache 2" "Apache-2.0"
  , Tuple "BSD-3" "BSD-3-Clause"
  ]

unfixable :: Array String
unfixable =
  [ "MIT AND BSD-3"
  ]
