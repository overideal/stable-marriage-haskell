-- Permits multiple declarations of the same attribute in different classes.
-- {-# LANGUAGE DuplicateRecordFields #-}


import Data.List -- For sort, findIndex
import Data.Maybe -- For fromJust
import Data.Tuple.Extra -- For &&&, snd3
import Data.Bifunctor

-- import Data.Tuple

type Man = String
type Woman = String

type PrefMen = [(Man,[Woman])]
type PrefWomen = [(Woman,[Man])]

type TempMatching = [(Maybe Man, Woman)]

data Matching = Matching [(Man,Woman)] deriving Show

-- Could be made more efficient by directly sorting in all functions that return matchings and replace this special
-- implementation of equality with the default one that cares for order
instance Eq Matching where
  (Matching matching1) == (Matching matching2) = sort matching1 == sort matching2
  -- The following assumes that matching1 is ordered by women.
  -- (Matching matching1) == (Matching matching2) = matching1 == sortBy ((>) . (Data.Bifunctor.bimap snd snd)) matching2
  -- (Matching matching1) == (Matching matching2) = and . zipWith (==) (map snd matching1) $
  --   where men = map fst matching1

deferred_acceptance_algorithm :: PrefMen -> PrefWomen -> Matching
deferred_acceptance_algorithm prefs_men prefs_women =
  Matching . map (Data.Bifunctor.first fromJust) . snd3 .
  until (null . fst3) iterator $ (men, start_matching, prefs_men)
  where
    iterator :: ([Man], TempMatching, PrefMen) -> ([Man],TempMatching, PrefMen)
    iterator ((m:men), matching, prefs_men) =
      let (new_matching, new_prefs_men, new_man) =
            eval_proposal m (get_favorite_woman m prefs_men) prefs_men prefs_women matching
      in (update_men new_man men, new_matching, new_prefs_men)
    update_men :: Maybe Man -> [Man] -> [Man]
    update_men (Just m) men = m : men
    update_men Nothing men = men
    men = map fst prefs_men
    women = map fst prefs_women
    start_matching :: TempMatching
    start_matching = zip (repeat Nothing) women

deferred_acceptance_algorithm_women_propose :: PrefMen -> PrefWomen -> Matching
deferred_acceptance_algorithm_women_propose prefs_men =
   (\(Matching m) -> Matching (map swap m)) . flip deferred_acceptance_algorithm prefs_men

-- -- deferred_acceptance_algorithm_helper :: [Man] -> [Woman] -> Matching -> Matching
-- -- deferred_acceptance_algorithm_helper (m:men) women matching =
--   -- m proposes


-- Evaluate the proposal that the woman receives from the man.
eval_proposal :: Man -> Woman -> PrefMen -> PrefWomen -> TempMatching -> (TempMatching, PrefMen, Maybe Man)
eval_proposal man woman prefs_men prefs_women matching =
  if woman_prefers_man (get_woman_pref prefs_women woman) woman (Just man) old_man
  then (update_partner woman man matching, forget_favorite_woman old_man prefs_men, old_man)
  else (matching, forget_favorite_woman (Just man) prefs_men, Just man)
  where
    old_man :: Maybe Man
    old_man = (get_man matching woman)


-- Return True if first man is preferred. One of the men (but not both) may be Nothing.
woman_prefers_man :: [Man] -> Woman -> Maybe Man -> Maybe Man -> Bool
woman_prefers_man pref woman (Just m1) (Just m2) =
  fromJust $ (<) <$> (findIndex (== m1) pref) <*> findIndex (== m2) pref
woman_prefers_man _ _ (Just _) Nothing = True
woman_prefers_man _ _ Nothing (Just _) = False
woman_prefers_man _ _ _ _ = error "Not both men in woman_prefers_man may be Nothing."


-- Make the man the partner of the woman in the matching.
update_partner ::  Woman -> Man -> TempMatching -> TempMatching
update_partner woman man = change_list ((==woman) . snd) (Data.Bifunctor.first (const (Just man)))

-- Return the favorite woman of the man.
get_favorite_woman :: Man -> PrefMen -> Woman
get_favorite_woman man = head . snd . fromJust . find ((==man) . fst)

-- Return the favorite man of the woman.
get_favorite_man :: Woman -> PrefWomen -> Man
get_favorite_man woman = head . snd . fromJust . find ((==woman) . fst)


-- Remove the man's favorite woman from the man's preference list.
-- The man will forget the woman, knowing he has no chance with her.
forget_favorite_woman :: Maybe Man -> PrefMen -> PrefMen
forget_favorite_woman (Just man) = change_list ((==man) . fst) (Data.Bifunctor.second tail)
forget_favorite_woman Nothing = id


-- Given a preference list and a woman, return the preferences of that woman.
get_woman_pref :: PrefWomen -> Woman -> [Man]
get_woman_pref prefs woman = snd . fromJust . find ((==woman) . fst) $ prefs

-- Given a matching and a man, return his partner, if existent.
-- get_woman :: TempMatching -> Man -> Maybe Woman
-- get_woman match man = fmap snd . find ((==(Just man)) . fst) $ match

-- Given a matching and a woman, return her partner, if existent.
get_man :: TempMatching -> Woman -> Maybe Man
get_man match woman = fst . fromJust . find ((==woman) . snd) $ match



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

-- Find matchings for setups with n actors per gender from the deferred_acceptance_algorithm
-- that agree for both men and women.
-- Very inefficent, since it checks all permutations.
compute_fair_matchings :: Int -> [(Matching,PrefMen,PrefWomen)]
compute_fair_matchings =
  map (\((matching,_),(prefs_men, prefs_women)) -> (matching, prefs_men, prefs_women))
  . filter (uncurry (==) . fst)
  . map (Data.Bifunctor.first ((uncurry deferred_acceptance_algorithm) &&& (uncurry deferred_acceptance_algorithm_women_propose)))
  . map dupe
  . generate_all

compute_all_matchings :: Int -> [(Matching,PrefMen,PrefWomen)]
compute_all_matchings =
  map (\(matching,(prefs_men, prefs_women)) -> (matching, prefs_men, prefs_women))
  . map (Data.Bifunctor.first (uncurry deferred_acceptance_algorithm))
  . map dupe
  . generate_all


filter_no_favorite_person :: [(Matching,PrefMen,PrefWomen)] -> [(Matching,PrefMen,PrefWomen)]
filter_no_favorite_person = filter (uncurry3 no_favorite_person)


-- Given a matching, a preference of men and a preference of women,
-- return True if and only if no person receives their favorite partner.
no_favorite_person :: Matching -> PrefMen -> PrefWomen -> Bool
no_favorite_person (Matching matching) prefs_men prefs_women = null . filter is_favorite $ matching
  where
    is_favorite :: (Man, Woman) -> Bool
    is_favorite (man, woman) =
      (get_favorite_woman man prefs_men) == woman || (get_favorite_man woman prefs_women) == man

-- Return the deferred_acceptance_algorithm matching if and only if no person receives their favorite partner.
-- Just a wrapper around no_favorite_person for convenience.
check_for_favorite_person :: (PrefMen, PrefWomen) -> Maybe Matching
check_for_favorite_person prefs@(prefs_men, prefs_women) =
  if no_favorite_person matching prefs_men prefs_women
  then Just matching
  else Nothing
  where matching = (uncurry deferred_acceptance_algorithm) prefs


--- Tests

-- Test for correctness, but not fair.
prefs :: (PrefMen, PrefWomen)
prefs = ([("m1", ["w1", "w3", "w4", "w2"]), ("m2", ["w1", "w3", "w2", "w4"]),
          ("m3", ["w2", "w1", "w3", "w4"]), ("m4" , ["w2", "w4" , "w1", "w3"])],
         [("w1", ["m4", "m3", "m1", "m2"]), ("w2", ["m2", "m4", "m1", "m3"]),
          ("w3", ["m4", "m1", "m2", "m3"]), ("w4", ["m3", "m2", "m1", "m4"])])
-- Works. Tested with:
-- (uncurry deferred_acceptance_algorithm) prefs,
-- (uncurry deferred_acceptance_algorithm_women_propose) prefs

-- This example shows that the stable matchings are not optimal for all men (strongly Pareto optimal)
-- when considering the set of all (not necessarily stable) matchings.
-- That the resulting matching is fair can be checked directly or by "looking it up":
-- (,,) ((uncurry deferred_acceptance_algorithm) prefs2) (fst prefs2) (snd prefs2) `elem` (compute_fair_matchings $ 3).
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


