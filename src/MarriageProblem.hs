-- Permits multiple declarations of the same attribute in different classes.
-- {-# LANGUAGE DuplicateRecordFields #-}

-- module MarriageProblem where

import Data.List -- For sort, findIndex
import qualified Data.Map as Map
import Data.Maybe -- For fromJust
import Data.Tuple.Extra -- For &&&, snd3, uncurry3


type Man = String
type Woman = String

type PrefMen = [(Man,[Woman])]
type PrefWomen = [(Woman,[Man])]

type PartialMatching = Map.Map Woman Man

type Matching = [(Man,Woman)]

deferredAcceptanceAlgorithm :: PrefMen -> PrefWomen -> Matching
deferredAcceptanceAlgorithm prefs_men prefs_women =
  map swap . Map.toList . snd .
  until (null . fst . fst) iterator $ ((men, prefs_men), Map.empty)
  where
    iterator :: (([Man],PrefMen), PartialMatching) -> (([Man],PrefMen), PartialMatching)
    iterator ((m:men,prefs_men), matching) =
      let (new_matching, new_prefs_men, new_man) =
            eval_proposal m (get_favorite_woman m prefs_men) prefs_men prefs_women matching
      in ((update_men new_man men, new_prefs_men), new_matching)
    update_men :: Maybe Man -> [Man] -> [Man]
    update_men (Just m) men = m : men
    update_men Nothing men = men
    men = map fst prefs_men


deferredAcceptanceAlgorithm_women_propose :: PrefMen -> PrefWomen -> Matching
deferredAcceptanceAlgorithm_women_propose prefs_men =
   map swap . flip deferredAcceptanceAlgorithm prefs_men
   

-- Evaluate the proposal that the woman receives from the man.
eval_proposal :: Man -> Woman -> PrefMen -> PrefWomen -> PartialMatching -> (PartialMatching, PrefMen, Maybe Man)
eval_proposal man woman prefs_men prefs_women matching =
  if woman_prefers_man (get_woman_pref prefs_women woman) man old_man
  -- Make the man the partner of the woman in the matching.
  then (Map.insert woman man matching, forget_favorite_woman old_man prefs_men, old_man)
  else (matching, forget_favorite_woman (Just man) prefs_men, Just man)
  where
    -- Return True if the first man is preferred by the woman.
    woman_prefers_man :: [Man] -> Man -> Maybe Man -> Bool
    woman_prefers_man pref m1 (Just m2) =
      fromJust $ (<) <$> (findIndex (== m1) pref) <*> findIndex (== m2) pref
    woman_prefers_man _ _ _ = True
    old_man :: Maybe Man
    old_man = Map.lookup woman matching



-- Return the favorite woman of the man.
get_favorite_woman :: Man -> PrefMen -> Woman
get_favorite_woman man = head . snd . fromJust . find ((==man) . fst)

-- Return the favorite man of the woman.
get_favorite_man :: Woman -> PrefWomen -> Man
get_favorite_man woman = head . snd . fromJust . find ((==woman) . fst)


-- Remove the man's favorite woman from the man's preference list.
-- The man will forget the woman, knowing he has no chance with her.
forget_favorite_woman :: Maybe Man -> PrefMen -> PrefMen
forget_favorite_woman (Just man) = change_list ((==man) . fst) (second tail)
forget_favorite_woman Nothing = id


-- Given a preference list and a woman, return the preferences of that woman.
get_woman_pref :: PrefWomen -> Woman -> [Man]
get_woman_pref prefs woman = snd . fromJust . find ((==woman) . fst) $ prefs

-- Given a matching and a man, return his partner, if existent.
-- get_woman :: PartialMatching -> Man -> Maybe Woman
-- get_woman match man = fmap snd . find ((==(Just man)) . fst) $ match


-- Could save some permutations for the women
generate_all :: Int -> [(PrefMen,PrefWomen)]
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
draw_with_putting_back 1 list = map (\s -> [s]) list
draw_with_putting_back n list = concat [map (element:) (draw_with_putting_back (n-1) list) | element <- list]


-- Given the list and the predicate function, call the other function on all elements that matches.
change_list :: (a -> Bool) -> (a -> a) -> [a] -> [a]
change_list p f = map (\a -> if p a then f a else a)


-- My main motivation for this project was to answer the following question:
-- If a matching is the result of the deferred acceptance algorithm if both men and women propose (call it "fair"),
-- then at least one person gets their first choice.

-- Numbers for n = 3:
-- number of possibilities: 46656
-- number of possibilites for which the matching is fair: 34080

-- Find matchings for setups with n actors per gender from the deferredAcceptanceAlgorithm
-- that agree for both men and women.
-- Very inefficent, since it checks all permutations.
compute_fair_matchings :: Int -> [(Matching,PrefMen,PrefWomen)]
compute_fair_matchings =
  map (\((matching,_),(prefs_men, prefs_women)) -> (matching, prefs_men, prefs_women))
  . filter (uncurry (==) . fst)
  . map (first ((uncurry deferredAcceptanceAlgorithm) &&& (uncurry deferredAcceptanceAlgorithm_women_propose)))
  . map dupe
  . generate_all

compute_all_matchings :: Int -> [(Matching,PrefMen,PrefWomen)]
compute_all_matchings =
  map (\(matching,(prefs_men, prefs_women)) -> (matching, prefs_men, prefs_women))
  . map (first (uncurry deferredAcceptanceAlgorithm))
  . map dupe
  . generate_all


filter_no_favorite_person :: [(Matching,PrefMen,PrefWomen)] -> [(Matching,PrefMen,PrefWomen)]
filter_no_favorite_person = filter (uncurry3 no_favorite_person)


-- Given a matching, a preference of men and a preference of women,
-- return True if and only if no person receives their favorite partner.
no_favorite_person :: Matching -> PrefMen -> PrefWomen -> Bool
no_favorite_person matching prefs_men prefs_women = null . filter is_favorite $ matching
  where
    is_favorite :: (Man, Woman) -> Bool
    is_favorite (man, woman) =
      (get_favorite_woman man prefs_men) == woman || (get_favorite_man woman prefs_women) == man

-- Return the deferredAcceptanceAlgorithm matching if and only if no person receives their favorite partner.
-- Just a wrapper around no_favorite_person for convenience.
check_for_favorite_person :: (PrefMen, PrefWomen) -> Maybe Matching
check_for_favorite_person prefs@(prefs_men, prefs_women) =
  if no_favorite_person matching prefs_men prefs_women
  then Just matching
  else Nothing
  where matching = (uncurry deferredAcceptanceAlgorithm) prefs



-- Test for correctness, but not fair.
prefs :: (PrefMen, PrefWomen)
prefs = ([("m1", ["w1", "w3", "w4", "w2"]), ("m2", ["w1", "w3", "w2", "w4"]),
          ("m3", ["w2", "w1", "w3", "w4"]), ("m4" , ["w2", "w4" , "w1", "w3"])],
         [("w1", ["m4", "m3", "m1", "m2"]), ("w2", ["m2", "m4", "m1", "m3"]),
          ("w3", ["m4", "m1", "m2", "m3"]), ("w4", ["m3", "m2", "m1", "m4"])])
-- Works. Tested with:
-- (uncurry deferredAcceptanceAlgorithm) prefs,
-- (uncurry deferredAcceptanceAlgorithm_women_propose) prefs

-- This example shows that the stable matchings are not optimal for all men (strongly Pareto optimal)
-- when considering the set of all (not necessarily stable) matchings.
-- That the resulting matching is fair can be checked directly or by "looking it up":
-- (,,) ((uncurry deferredAcceptanceAlgorithm) prefs2) (fst prefs2) (snd prefs2) `elem` (compute_fair_matchings $ 3).
-- The matching is [("m2","w1"),("m1","w2"),("m3","w3")] and an improvement for the men is [("m1", "w1"), ("m2","w2"), ("m3","w3")].
prefs2 :: (PrefMen, PrefWomen)
prefs2 = ([("m1", ["w1", "w2", "w3"]), ("m2", ["w2", "w1", "w3"]), ("m3", ["w1", "w2", "w3"])],
          [("w1", ["m2", "m3", "m1"]), ("w2", ["m1", "m3", "m2"]), ("w3", ["m1", "m2", "m3"])])

-- This provides a counterexample to my original question.
-- I discovered it here: https://math.stackexchange.com/questions/1332591/does-the-gale-shapley-stable-marriage-algorithm-give-at-least-one-person-his-or.
-- Check via
-- check_for_favorite_person prefs2.
prefs3 :: (PrefMen, PrefWomen)
prefs3 = ([("m1", ["w1", "w2", "w3", "w4"]), ("m2", ["w1", "w4", "w3", "w2"]),
          ("m3", ["w2", "w1", "w3", "w4"]), ("m4" , ["w4", "w2" , "w3", "w1"])],
         [("w1", ["m4", "m3", "m1", "m2"]), ("w2", ["m2", "m4", "m1", "m3"]),
          ("w3", ["m4", "m1", "m2", "m3"]), ("w4", ["m3", "m2", "m1", "m4"])])


-- First personal counter example, found using
-- head . filter_no_favorite_person . compute_fair_matchings $ 4
-- (Matching [("m3","w1"),("m1","w2"),("m2","w3"),("m4","w4")],[("m1",["w1","w2","w3","w4"]),("m2",["w1","w2","w3","w4"]),("m3",["w2","w1","w3","w4"]),("m4",["w3","w4","w2","w1"])],[("w1",["m4","m3","m2","m1"]),("w2",["m4","m1","m2","m3"]),("w3",["m1","m2","m3","m4"]),("w4",["m3","m4","m2","m1"])])
