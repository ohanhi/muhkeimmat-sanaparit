import Graphics.Element exposing (..)
import String
import Array
import List
import Set

-- `DataSource.alastalo` is the book "Alastalon Salissa" by Volter Kilpi
import DataSource

-- Configuration for running the solver
-- how many best single words to use as the base for cross-examination
initialTake = 20

type alias WordPair =
  { pair : (String, String), muhkeus : Int }

-- Valid characters from the exercise definition
validChars : Set.Set Char
validChars =
  Set.fromList
    [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'
    , 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't'
    , 'u', 'v', 'w', 'x', 'y', 'z', 'å', 'ä', 'ö'
    ]

{-
The main runnable function.
Shows the highest "muhkeus" value, and a list of word pairs
that reached that number.
-}
main : Element
main =
  let wordPairs = bestWordPairs (DataSource.alastalo)
      maxMuhkeus = highestMuhkeus wordPairs
      descriptionEl = show ("muhkeus: " ++ toString maxMuhkeus)
      tuples = toDedupedTuples wordPairs
      elements = descriptionEl :: (List.map show tuples)
  in  flow down elements

{-
Get `WordPair`s with most "muhkeus" in `text`.
In essence we
  1. find N best single words as the base sample
  2. cross-examine the sample to get best pairs
  3. find highest "muhkeus" within the pairs
  4. take all pairs with the highest possible "muhkeus"
-}
bestWordPairs : String -> List WordPair
bestWordPairs text =
  let uniqueWords = -- reduces the amount of calculations considerably
        text
          |> String.toLower
          |> String.words -- split to list containing single words
          |> Set.fromList -- transforming to set deduplicates the list
          |> Set.toList
      bestSingleWords =
        uniqueWords
          |> bestWords initialTake
      bestPairs =
        bestSingleWords
          |> List.map (bestMatches bestSingleWords)
          |> List.concat
          |> withMaximumMuhkeus
  in  bestPairs

-- Get the pairs with the highest muhkeus within the list
withMaximumMuhkeus : List WordPair -> List WordPair
withMaximumMuhkeus pairs =
  let max = highestMuhkeus pairs
  in  List.filter (\pair -> pair.muhkeus == max) pairs

-- Finds the maximum muhkeus in the list
highestMuhkeus : List WordPair -> Int
highestMuhkeus pairs =
  pairs
    |> List.map (\wp -> wp.muhkeus)
    |> List.maximum
    |> Maybe.withDefault 0

-- Check best matches for `a` from `words` list
bestMatches : List String -> String -> List WordPair
bestMatches words a =
  let wordPairs = List.map (toWordPair a) words
  in  withMaximumMuhkeus wordPairs

-- Get n words with most "muhkeus" from a list
bestWords : Int -> List String -> List String
bestWords n words =
  words
    |> List.sortBy muhkeus
    |> List.reverse
    |> List.take n

-- Get the "muhkeus" of the string
muhkeus : String -> Int
muhkeus string =
  string
    |> String.toLower
    |> String.toList
    |> Set.fromList             -- Set contains only unique chars
    |> Set.intersect validChars -- Intersect "filters" only valid chars
    |> Set.toList               -- Set doesn't have a length, but List does
    |> List.length

-- Transform two `String`s to a `WordPair` with those words
toWordPair : String -> String -> WordPair
toWordPair a b =
  { pair = (a, b)
  , muhkeus = muhkeus (String.append a b)
  }

-- Transform `WordPair`s into tuples and deduplicate
toDedupedTuples : List WordPair -> List (String, String)
toDedupedTuples wordPairs =
  wordPairs
    |> List.map (\w -> (w.pair))
    |> List.map (\(a, b) -> if a < b then (a, b) else (b, a))
    |> Set.fromList
    |> Set.toList
