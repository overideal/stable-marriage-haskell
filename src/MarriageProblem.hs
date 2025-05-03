{-# LANGUAGE TupleSections #-}
-- Permits multiple declarations of the same attribute in different classes.
-- {-# LANGUAGE DuplicateRecordFields #-}

module MarriageProblem where

import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Tuple.Extra -- (&&&), uncurry3, dupe, first, swap
import qualified Math.Combinatorics.Multiset as MSet



-- * Types

type Pref a b = Map.Map a [b]

type PartialMatching a b = Map.Map b a

type Matching a b = Map.Map a b



-- * Main Algorithm

deferredAcceptanceAlg :: (Ord a, Ord b) => Pref a b -> Pref b a -> Matching a b
deferredAcceptanceAlg prefs_men =
  Map.fromList . map swap . Map.toList . flip deferredAcceptanceAlgWomen prefs_men


deferredAcceptanceAlgWomen :: (Ord a, Ord b) => Pref a b -> Pref b a -> Matching a b
deferredAcceptanceAlgWomen prefs_men prefs_women =
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



-- * Example Generation


computeMatchings :: Int -> [(Matching Int Int, (Pref Int Int, Pref Int Int))]
computeMatchings =
  map (uncurry deferredAcceptanceAlg &&& id) . generatePrefsPerm


computeAllMatchings :: Int -> [(Matching Int Int, (Pref Int Int, Pref Int Int))]
computeAllMatchings =
  map (uncurry deferredAcceptanceAlg &&& id) . generatePrefs


-- Find matchings for setups with n actors per gender from the deferredAcceptanceAlg
-- that agree for both men and women.
computeFairMatchings :: Int -> [(Matching Int Int, (Pref Int Int, Pref Int Int))]
computeFairMatchings =
  map (first fst)
  . filter (uncurry (==) . fst)
  . map (first (uncurry deferredAcceptanceAlg &&&
                uncurry deferredAcceptanceAlgWomen) . dupe)
  . generatePrefsPerm


-- Generate all preference profiles, up to permutations.
generatePrefsPerm :: Int -> [(Pref Int Int, Pref Int Int)]
generatePrefsPerm n = (,) <$> prefs_men <*> prefs_women
  where
    prefs_men :: [Pref Int Int]
    prefs_men = map make_pref . MSet.kSubsets (n-1) . MSet.fromCounts . map (, n-1) $ perms
    prefs_women = map (Map.fromDistinctAscList . zip [1..n]) . draw_with_order_repeating n . permutations $ [1..n]
    perms :: [[Int]]
    perms = permutations [1..n]
    make_pref :: MSet.Multiset [Int] -> Pref Int Int
    make_pref = Map.fromList . zip [1..n] . ([1..n]:) . concatMap (uncurry $ flip replicate) . MSet.toCounts


-- Generate all preference profiles.
-- This is very inefficient, since permutations are not considered.
generatePrefs :: Int -> [(Pref Int Int, Pref Int Int)]
generatePrefs n = (,) <$> prefs <*> prefs
  where
    prefs = map (Map.fromDistinctAscList . zip [1..n]) . draw_with_order_repeating n . permutations $ [1..n]


-- Generate all n element sets, where order matters and elements may be drawn multiple times.
-- If the given list contains k lements, then the returned list thus contains k^n elements.
draw_with_order_repeating :: Int -> [a] -> [[a]]
draw_with_order_repeating 0 _ = []
draw_with_order_repeating 1 list = map singleton list
draw_with_order_repeating n list = concat [map (element:) (draw_with_order_repeating (n-1) list) | element <- list]



-- * Checks

-- ** Check if a matching is stable

-- Check whether the given matching is stable.
isStableMatching :: (Ord a, Ord b) => Pref a b -> Pref b a -> Matching a b -> Bool
isStableMatching prefs_men prefs_women matching =
  any mapper matching_list
  where
    -- mapper :: (a, b) -> Bool
    mapper (m,w) = any (new_man_is_preferred m (getPartner matching_list w)) . takeWhile (/= w) . Map.findWithDefault (error "No preference found") m $ prefs_men
    -- new_man_is_preferred :: a -> a -> b -> Bool
    new_man_is_preferred new_man old_man woman =
      (== new_man) . fromJust . find (`elem` [old_man,new_man]) .
      Map.findWithDefault (error "No preference found") woman $ prefs_women
    matching_list = Map.toList matching

getPartner :: Eq b => [(a, b)] -> b -> a
getPartner matching w = fst . fromJust $ find ((== w) . snd) matching


-- ** Check if someone gets favorite person

-- Given a matching, a preference of men and a preference of women,
-- return True if and only if some person receives their favorite partner.
hasFavorite :: (Ord a, Ord b) => Matching a b -> Pref a b -> Pref b a -> Bool
hasFavorite matching prefs_men prefs_women = any is_favorite . Map.toList $ matching
  where
    -- is_favorite :: (a, b) -> Bool
    is_favorite (man, woman) =
      getFavorite man prefs_men == woman || getFavorite woman prefs_women == man


onLists :: (Ord a, Ord b) => (Pref a b -> Pref b a -> c) -> ([(a,[b])], [(b,[a])]) -> c
onLists f (prefs_men, prefs_women) = f (Map.fromList prefs_men) (Map.fromList prefs_women)
