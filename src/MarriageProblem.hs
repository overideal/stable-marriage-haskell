{-# LANGUAGE TupleSections #-}

module MarriageProblem (Pref, PrefProfile, Matching, deferredAcceptanceAlg,
                        deferredAcceptanceAlgWomen, isStableMatching,
                        hasFavorite, computeMatchings, computeAllMatchings,
                        computeUniqueMatchings, computeAllUniqueMatchings,
                        listToPrefProfile) where

import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Tuple.Extra
import qualified Math.Combinatorics.Multiset as MSet



-- * Types

type Pref a b = Map.Map a [b]

type PrefProfile a b = (Pref a b, Pref b a)

type PartialMatching a b = Map.Map b a

type Matching a b = Map.Map a b



-- * Main Algorithm

deferredAcceptanceAlg :: (Ord a, Ord b) => PrefProfile a b -> Matching a b
deferredAcceptanceAlg =
  Map.fromList . map swap . Map.toList . deferredAcceptanceAlgWomen . swap


deferredAcceptanceAlgWomen :: (Ord a, Ord b) => PrefProfile a b -> Matching a b
deferredAcceptanceAlgWomen (prefs_men, prefs_women) =
  snd . until (null . fst . fst) iterator $ ((women, prefs_women), Map.empty)
  where
    -- iterator :: (([a], Pref a b), PartialMatching a b) -> (([a],Pref a b), PartialMatching a b)
    iterator ((w:women,prefs_women), matching) =
      let (new_matching, new_prefs_women, new_woman) =
            evalProposal w (getFavorite w prefs_women) prefs_women prefs_men matching
      in ((update_women new_woman women, new_prefs_women), new_matching)
    update_women :: Maybe a -> [a] -> [a]
    update_women (Just w) women = w : women
    update_women Nothing women = women
    women = Map.keys prefs_women


-- Evaluate the proposal that the woman receives from the man.
evalProposal :: (Ord a, Ord b) => a -> b -> Pref a b -> Pref b a -> PartialMatching a b -> (PartialMatching a b, Pref a b, Maybe a)
evalProposal man woman prefs_men prefs_women matching =
  if woman_prefers_man (Map.findWithDefault (error "No preference found")
                        woman prefs_women) man old_man
  -- Make the man the partner of the woman in the matching.
  then (Map.insert woman man matching, forgetFavorite old_man prefs_men, old_man)
  else (matching, forgetFavorite (Just man) prefs_men, Just man)
  where
    -- Return True if the first man is preferred by the woman.
    -- woman_prefers_man :: [a] -> a -> Maybe a -> Bool
    woman_prefers_man pref m1 (Just m2) =
      fromJust $ (<) <$> elemIndex m1 pref <*> elemIndex m2 pref
    woman_prefers_man _ _ _ = True
    -- old_man :: Maybe a
    old_man = Map.lookup woman matching


-- Return the favorite woman of the man or vice versa.
getFavorite :: Ord a => a -> Pref a b -> b
getFavorite man = head . Map.findWithDefault (error "No preference found") man


-- Remove the man's favorite woman from the man's preference list.
-- The man will forget the woman, knowing he has no chance with her.
forgetFavorite :: Ord a => Maybe a -> Pref a b -> Pref a b
forgetFavorite (Just man) = Map.adjust tail man
forgetFavorite Nothing = id



-- * Checks

-- ** Check if a matching is stable

-- Check whether the given matching is stable.
isStableMatching :: (Ord a, Ord b) => PrefProfile a b -> Matching a b -> Bool
isStableMatching (prefs_men, prefs_women) matching =
  not . any mapper $ matching_list
  where
    -- mapper :: (a, b) -> Bool
    mapper (m,w) = any (new_man_is_preferred m) .
                   takeWhile (/= w) .
                   Map.findWithDefault (error "No preference found") m $ prefs_men
    -- new_man_is_preferred :: a -> b -> Bool
    new_man_is_preferred new_man woman =
      let old_man = getPartner matching_list woman
      in (== new_man) . fromJust . find (`elem` [old_man,new_man]) .
      Map.findWithDefault (error "No preference found") woman $ prefs_women
    matching_list = Map.toList matching


getPartner :: Eq b => [(a, b)] -> b -> a
getPartner matching w = fst . fromJust $ find ((== w) . snd) matching


-- ** Check if someone gets favorite person

-- Given a matching, a preference of men and a preference of women,
-- return True if and only if some person receives their favorite partner.
hasFavorite :: (Ord a, Ord b) => PrefProfile a b -> Matching a b -> Bool
hasFavorite (prefs_men, prefs_women) matching = any is_favorite . Map.toList $ matching
  where
    -- is_favorite :: (a, b) -> Bool
    is_favorite (man, woman) =
      getFavorite man prefs_men == woman || getFavorite woman prefs_women == man



-- * Exhaustive Search


computeMatchings :: Int -> [(PrefProfile Int Int, Matching Int Int)]
computeMatchings =
  map (id &&& deferredAcceptanceAlg) . generatePrefsPerm


computeAllMatchings :: Int -> [(PrefProfile Int Int, Matching Int Int)]
computeAllMatchings =
  map (id &&& deferredAcceptanceAlg) . generatePrefs


-- Find matchings for setups with n actors per gender from the deferredAcceptanceAlg
-- that agree for both men and women.
computeUniqueMatchings :: Int -> [(PrefProfile Int Int, Matching Int Int)]
computeUniqueMatchings =
  map (second fst)
  . filter (uncurry (==) . snd)
  . map (second (deferredAcceptanceAlg &&&
                deferredAcceptanceAlgWomen) . dupe)
  . generatePrefsPerm


computeAllUniqueMatchings :: Int -> [(PrefProfile Int Int, Matching Int Int)]
computeAllUniqueMatchings =
  map (second fst)
  . filter (uncurry (==) . snd)
  . map (second (deferredAcceptanceAlg &&&
                deferredAcceptanceAlgWomen) . dupe)
  . generatePrefs


-- Generate all preference profiles, up to permutations.
generatePrefsPerm :: Int -> [PrefProfile Int Int]
generatePrefsPerm n = (,) <$> prefs_men <*> prefs_women
  where
    prefs_men :: [Pref Int Int]
    prefs_men = map make_pref . MSet.kSubsets (n-1) . MSet.fromCounts . map (, n-1) $ perms
    prefs_women = map (Map.fromDistinctAscList . zip [1..n]) . drawWithOrderRepeating n . permutations $ [1..n]
    perms :: [[Int]]
    perms = permutations [1..n]
    make_pref :: MSet.Multiset [Int] -> Pref Int Int
    make_pref = Map.fromList . zip [1..n] . ([1..n]:) . concatMap (uncurry $ flip replicate) . MSet.toCounts


-- Generate all preference profiles.
-- This is very inefficient, since permutations are not considered.
generatePrefs :: Int -> [PrefProfile Int Int]
generatePrefs n = (,) <$> prefs <*> prefs
  where
    prefs = map (Map.fromDistinctAscList . zip [1..n]) . drawWithOrderRepeating n . permutations $ [1..n]


-- Generate all n element sets, where order matters and elements may be drawn multiple times.
-- If the given list contains k lements, then the returned list thus contains k^n elements.
drawWithOrderRepeating :: Int -> [a] -> [[a]]
drawWithOrderRepeating 0 _ = []
drawWithOrderRepeating 1 list = map singleton list
drawWithOrderRepeating n list = concat [map (element:) (drawWithOrderRepeating (n-1) list) | element <- list]



-- * Converters

listToPrefProfile :: (Ord a, Ord b) => ([(a,[b])], [(b,[a])]) -> PrefProfile a b
listToPrefProfile (prefs_men, prefs_women) = (Map.fromList prefs_men, Map.fromList prefs_women)
