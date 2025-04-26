-- Permits multiple declarations of the same attribute in different classes.
-- {-# LANGUAGE DuplicateRecordFields #-}


module MarriageProblem where

import Data.List -- For sort, findIndex
import qualified Data.Map as Map
import Data.Maybe -- For fromJust
import Data.Tuple.Extra -- For &&&, snd3, uncurry3


type Man a = a
type Woman a = a

type PrefMen a b = [(Man a,[Woman b])]
type PrefWomen a b = [(Woman b,[Man a])]

type PartialMatching a b = Map.Map (Woman b) (Man a)

type Matching a b = [(Man a, Woman b)]

deferredAcceptanceAlgorithm :: (Eq a, Ord b) => PrefMen a b -> PrefWomen a b -> Matching a b
deferredAcceptanceAlgorithm prefs_men prefs_women =
  map swap . Map.toList . snd .
  until (null . fst . fst) iterator $ ((men, prefs_men), Map.empty)
  where
    -- iterator :: (([Man a], PrefMen a b), PartialMatching a b) -> (([Man a],PrefMen a b), PartialMatching a b)
    iterator ((m:men,prefs_men), matching) =
      let (new_matching, new_prefs_men, new_man) =
            eval_proposal m (get_favorite_woman m prefs_men) prefs_men prefs_women matching
      in ((update_men new_man men, new_prefs_men), new_matching)
    update_men :: Maybe (Man a) -> [Man a] -> [Man a]
    update_men (Just m) men = m : men
    update_men Nothing men = men
    men = map fst prefs_men


deferredAcceptanceAlgorithmWomenPropose :: (Ord a, Eq b) => PrefMen a b -> PrefWomen a b -> Matching a b
deferredAcceptanceAlgorithmWomenPropose prefs_men =
   map swap . flip deferredAcceptanceAlgorithm prefs_men


-- Evaluate the proposal that the woman receives from the man.
eval_proposal :: (Eq a, Ord b) => Man a -> Woman b -> PrefMen a b -> PrefWomen a b -> PartialMatching a b -> (PartialMatching a b, PrefMen a b, Maybe (Man a))
eval_proposal man woman prefs_men prefs_women matching =
  if woman_prefers_man (get_woman_pref prefs_women woman) man old_man
  -- Make the man the partner of the woman in the matching.
  then (Map.insert woman man matching, forget_favorite_woman old_man prefs_men, old_man)
  else (matching, forget_favorite_woman (Just man) prefs_men, Just man)
  where
    -- Return True if the first man is preferred by the woman.
    -- woman_prefers_man :: [Man a] -> Man a -> Maybe (Man a) -> Bool
    woman_prefers_man pref m1 (Just m2) =
      fromJust $ (<) <$> elemIndex m1 pref <*> elemIndex m2 pref
    woman_prefers_man _ _ _ = True
    -- old_man :: Maybe (Man a)
    old_man = Map.lookup woman matching



-- Return the favorite woman of the man.
get_favorite_woman :: Eq a => Man a -> PrefMen a b -> Woman b
get_favorite_woman man = head . snd . fromJust . find ((==man) . fst)

-- Return the favorite man of the woman.
get_favorite_man :: Eq b => Woman b -> PrefWomen a b -> Man a
get_favorite_man woman = head . snd . fromJust . find ((==woman) . fst)


-- Remove the man's favorite woman from the man's preference list.
-- The man will forget the woman, knowing he has no chance with her.
forget_favorite_woman :: Eq a => Maybe (Man a) -> PrefMen a b -> PrefMen a b
forget_favorite_woman (Just man) = change_list ((==man) . fst) (second tail)
forget_favorite_woman Nothing = id


-- Given a preference list and a woman, return the preferences of that woman.
get_woman_pref :: Eq b => PrefWomen a b -> Woman b -> [Man a]
get_woman_pref prefs woman = snd . fromJust . find ((==woman) . fst) $ prefs


-- TODO: Could save some permutations for the women
generate_all :: Int -> [(PrefMen String String,PrefWomen String String)]
generate_all n = (,) <$> prefs_men <*> prefs_women
  where
    men = map (("m"++) . show) [1..n]
    women = map (("w"++) . show) [1..n]
    prefs_men = map (zip men) . draw_with_putting_back n . permutations $ women
    -- Maybe could save work here if men and women are named the same...
    prefs_women = map (zip women) . draw_with_putting_back n . permutations $ men

-- Generate all n element sets, where order matters and elements may be drawn multiple times.
-- If the given list contains k lements, then the returned list thus contains k^n elements.
draw_with_putting_back :: Int -> [a] -> [[a]]
draw_with_putting_back 0 _ = []
draw_with_putting_back 1 list = map singleton list
draw_with_putting_back n list = concat [map (element:) (draw_with_putting_back (n-1) list) | element <- list]


-- Given the list and the predicate function, call the other function on all elements that matches.
change_list :: (a -> Bool) -> (a -> a) -> [a] -> [a]
change_list p f = map (\a -> if p a then f a else a)


-- My main motivation for this project was to answer the following question:
-- If a matching is the result of the deferred acceptance algorithm if both men and women propose (call it "fair"),
-- then at least one person gets their first choice.


-- Find matchings for setups with n actors per gender from the deferredAcceptanceAlgorithm
-- that agree for both men and women.
-- Very inefficent since it checks all permutations.
compute_fair_matchings :: Int -> [(Matching String String, PrefMen String String, PrefWomen String String)]
compute_fair_matchings =
  map (\((matching,_),(prefs_men, prefs_women)) -> (matching, prefs_men, prefs_women))
  . filter (uncurry (==) . fst)
  . map (first (uncurry deferredAcceptanceAlgorithm &&& uncurry deferredAcceptanceAlgorithmWomenPropose) . dupe) . generate_all

compute_all_matchings :: Int -> [(Matching String String,PrefMen String String,PrefWomen String String)]
compute_all_matchings =
  map ((\(matching,(prefs_men, prefs_women)) -> (matching, prefs_men, prefs_women)) . first (uncurry deferredAcceptanceAlgorithm) . dupe) . generate_all


filter_no_favorite_person :: (Eq a, Eq b) => [(Matching a b,PrefMen a b,PrefWomen a b)] -> [(Matching a b,PrefMen a b,PrefWomen a b)]
filter_no_favorite_person = filter (uncurry3 no_favorite_person)


-- Given a matching, a preference of men and a preference of women,
-- return True if and only if no person receives their favorite partner.
no_favorite_person :: (Eq a, Eq b) => Matching a b -> PrefMen a b -> PrefWomen a b -> Bool
no_favorite_person matching prefs_men prefs_women = null . filter is_favorite $ matching
  where
    -- is_favorite :: (Man a, Woman b) -> Bool
    is_favorite (man, woman) =
      (get_favorite_woman man prefs_men) == woman || (get_favorite_man woman prefs_women) == man

-- Return the deferredAcceptanceAlgorithm matching if and only if no person receives their favorite partner.
-- Just a wrapper around no_favorite_person for convenience.
check_for_favorite_person :: (Eq a, Ord b) => (PrefMen a b, PrefWomen a b) -> Maybe (Matching a b)
check_for_favorite_person prefs@(prefs_men, prefs_women) =
  if no_favorite_person matching prefs_men prefs_women
  then Just matching
  else Nothing
  where matching = uncurry deferredAcceptanceAlgorithm prefs

-- Check whether the given matching is stable.
isStableMatching :: (Eq a, Eq b) => PrefMen a b -> PrefWomen a b -> Matching a b -> Bool
isStableMatching prefs_men prefs_women matching =
  any mapper matching
  where
    -- mapper :: (Man a, Woman b) -> Bool
    mapper (m,w) = any (new_man_is_preferred m (get_partner_woman matching w)) . takeWhile (/= w) . snd . fromJust . find ((== m) . fst) $ prefs_men
    -- new_man_is_preferred :: Man a -> Man a -> Woman b -> Bool
    new_man_is_preferred new_man old_man woman = (== new_man) . fromJust . find (`elem` [old_man,new_man]) . snd .
      fromJust . find ((== woman) . fst) $ prefs_women


get_partner_woman :: Eq b => Matching a b -> Woman b -> Man a
get_partner_woman matching w = fst . fromJust $ find ((== w) . snd) matching
