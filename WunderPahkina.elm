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
  let bestWordPairs = bestWordPairs DataSource.alastalo
      maxMuhkeus = highestMuhkeus bestWordPairs
      tuples = wordPairsToDedupedTuples bestWordPairs
  in  flow down
        [ show ("muhkeus: " ++ toString maxMuhkeus)
        , show tuples
        ]

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
          |> String.words
          |> Set.fromList
          |> Set.toList
      bestSingleWords =
        uniqueWords
          |> bestWords initialTake
      bestPairs =
        bestSingleWords
          |> List.map (bestMatches bestSingleWords)
          |> List.concat
      maxValue  = highestMuhkeus bestPairs
  in  bestPairs
        |> List.filter (\wp -> wp.muhkeus == maxValue )

-- returns the pairs with the highest muhkeus within the list
withMaximumMuhkeus : List WordPair -> List WordPair
withMaximumMuhkeus pairs =
  let max = highestMuhkeus pairs
  in  List.filter (\pair -> pair.muhkeus == max) pairs

-- finds the maximum muhkeus in the list
highestMuhkeus : List WordPair -> Int
highestMuhkeus pairs =
  pairs
    |> List.map (\wp -> wp.muhkeus)
    |> List.maximum
    |> Maybe.withDefault 0

-- check best matches for `a` from `words` list
bestMatches : List String -> String -> List WordPair
bestMatches words a =
  let wl    = String.length a
      wordPairs =
        List.map
          (\b ->  { pair = (a, b)
                  , muhkeus = tupleMuhkeus (a, b)
                  })
          words
  in  withMaximumMuhkeus wordPairs

-- get n words with most "muhkeus" from a list
bestWords : Int -> List String -> List String
bestWords n words =
  words
    |> List.sortBy muhkeus
    |> List.reverse
    |> List.take n

-- "muhkeus" of a word tuple
tupleMuhkeus : (String, String) -> Int
tupleMuhkeus (a, b) =
  String.append a b
    |> muhkeus

-- the "muhkeus" of the string
muhkeus : String -> Int
muhkeus string =
  string
    |> String.toLower
    |> String.toList
    |> Set.fromList             -- Set contains only unique chars
    |> Set.intersect validChars -- Intersect "filters" only valid chars
    |> Set.toList               -- Set doesn't have a length, but List does
    |> List.length

-- Transform `WordPair`s into tuples and deduplicate
wordPairsToDedupedTuples : List WordPair -> List (String, String)
wordPairsToDedupedTuples wordPairs =
  wordPairs
    |> List.map (\w -> (w.pair))
    |> List.map (\(a, b) -> if a < b then (a, b) else (b, a))
    |> Set.fromList
    |> Set.toList
