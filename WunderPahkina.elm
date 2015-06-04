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
  { wordA : String, wordB : String, muhkeus : Int }

-- Valid characters from the exercise definition
validChars : Set.Set Char
validChars =
  Set.fromList
    [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'
    , 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't'
    , 'u', 'v', 'w', 'x', 'y', 'z', 'å', 'ä', 'ö'
    ]

main : Element
main =
  DataSource.alastalo
    |> bestWordPairs
    |> show

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
  let bestSingleWords =
        text
          |> String.words
          |> bestWords initialTake
      bestPairs =
        bestSingleWords
          |> List.map (bestMatches bestSingleWords)
          |> List.concat
      highestMuhkeus =
        bestPairs
          |> List.map (\wp -> wp.muhkeus)
          |> List.maximum
          |> Maybe.withDefault 0
  in  bestPairs
        |> List.filter (\wp -> wp.muhkeus == highestMuhkeus)

-- check best matches for `wordA` from `words` list
bestMatches : List String -> String -> List WordPair
bestMatches words wordA =
  let wl    = String.length wordA
      wordPairs =
        words
          |> List.map (\w -> { wordA   = wordA
                             , wordB   = w
                             , muhkeus = tupleMuhkeus(wordA,w)
                             })
          |> List.sortBy .muhkeus
          |> List.reverse
          |> List.take 5
  in  wordPairs

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

