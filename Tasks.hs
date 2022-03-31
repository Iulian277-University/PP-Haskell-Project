-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array
import GHC.Float (int2Float)

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]


-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}


-- Task 1
row :: Row
row = ["Name","10","11","12","13","14","15","16","17"]

row2 :: Row
row2 = ["Layla Muhammad","29","0","0","0","0","0","319","225"]

eight_hours2 :: Table
eight_hours2 =
    [["Name","10","11","12","13","14","15","16","17"],
    ["Olivia Noah","373","160","151","0","0","0","0","0"],
    ["Riley Jackson","31","0","0","7","0","0","0","0"],
    ["Emma Aiden","45","8","0","0","0","0","0","0"],
    ["Aria Lucas","0","0","0","0","0","0","0","0"]]

compute_header_of_table :: Row -> Row
compute_header_of_table r = [(head r), "Average Number of Steps"] 

compute_steps_for_row :: Row -> Float
compute_steps_for_row r = (foldr (\x acc -> (read x :: Float) + acc) 0) (tail r)

compute_results_for_row :: Row -> Row
compute_results_for_row r = [(head r), printf "%.2f" $ (compute_steps_for_row r / 8)]

compute_average_steps :: Table -> Table
compute_average_steps t = [compute_header_of_table (head t)] ++ (map compute_results_for_row (tail t))


-- Task 2

-- Number of people who have achieved their goal:
threshold_goal :: Float
threshold_goal = 1000
get_passed_people_num :: Table -> Int
get_passed_people_num t = (foldr (\x acc -> if x >= threshold_goal then acc + 1 else acc) 0) $ (map compute_steps_for_row (tail t)) 

-- Percentage of people who have achieved their goal:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage t = (int2Float $ get_passed_people_num t) / (int2Float $ length t)

-- Average number of daily steps
get_steps_total :: Table -> Float
get_steps_total t = (foldr (\x total_steps -> total_steps + x) 0) $ (map compute_steps_for_row t) 

get_steps_avg :: Table -> Float
get_steps_avg t = (get_steps_total (tail t)) / (int2Float $ (length t - 1))


-- Task 3
-- I need to compute the sum on columns
-- For that, I will transpose the matrix (table) and compute sum on rows
get_total_steps_per_h :: Table -> [Float]
get_total_steps_per_h t = map compute_steps_for_row $ tail (transpose t) -- this `tail` removes the names

get_avg_steps_per_h_list :: Table -> [Float]
get_avg_steps_per_h_list t = foldr (\x acc -> (x / (int2Float $ (length t - 1))):acc) [] (get_total_steps_per_h t) -- this `tail` removes the header

float_list_to_row :: Table -> Row
float_list_to_row t = map (printf "%.2f") (get_avg_steps_per_h_list t)

steps_per_h_header :: Row
steps_per_h_header = ["H10","H11","H12","H13","H14","H15","H16","H17"]

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h t = [steps_per_h_header] ++ [float_list_to_row t]


-- Task 4
physical_activity2 :: Table
physical_activity2 =
    [["Name","TotalSteps","TotalDistance","VeryActiveMinutes","FairlyActiveMinutes","LightlyActiveMinutes"],
    ["Olivia Noah","13162","8.50","25","13","328"],
    ["Riley Jackson","10735","6.97","21","19","217"],
    ["Emma Aiden","10460","6.74","30","11","181"],
    ["Ava Elijah","9762","6.28","29","34","209"],
    ["Isabella Grayson","12669","8.16","36","10","221"]]

-- Ranges
range1_lo = 0
range1_hi = 50

range2_lo = range1_hi
range2_hi = 100

range3_lo = range2_hi
range3_hi = 500

-- VeryActiveMinutes    - at pos (length (head t) - 3)
-- FairlyActiveMinutes  - at pos (length (head t) - 2)
-- LightlyActiveMinutes - at pos (length (head t) - 1)

group_range :: Integer -> Integer -> Row -> Integer 
group_range range_lo range_hi = foldr (\x acc -> if ((read x :: Integer) >= range_lo) && ((read x :: Integer) < range_hi) then acc + 1 else acc) 0

group_minutes :: Row -> Row
group_minutes l = [(head l), (show $ group_range range1_lo range1_hi (tail l)), (show $ group_range range2_lo range2_hi (tail l)), (show $ group_range range3_lo range3_hi (tail l))]

get_activ_summary_intensity :: Table -> Int -> Row
get_activ_summary_intensity t idx = group_minutes $ (transpose t) !! idx

get_activ_summary_LA :: Table -> Row
get_activ_summary_LA t = group_minutes $ (transpose t) !! (length (head t) - 1)

get_activ_summary_header :: Row
get_activ_summary_header = ["column","range1","range2","range3"]

get_activ_summary :: Table -> Table
get_activ_summary t = [get_activ_summary_header, get_activ_summary_intensity t (length (head t) - 3), get_activ_summary_intensity t (length (head t) - 2), get_activ_summary_intensity t (length (head t) - 1)]


-- Task 5
get_ranking_header :: Row
get_ranking_header = ["Name","Total Steps"]

get_ranking :: Table -> Table
get_ranking t = [get_ranking_header] ++ sortBy (\p1 p2 -> compare (read $ (p1 !! 1) :: Integer) (read $ (p2 !! 1) :: Integer)) (tail t)

-- Task 6
eight_hours3 :: Table
eight_hours3 =
    [["Name","10","11","12","13","14","15","16","17"],
    ["Elizabeth Nathan","0","805","826","394","476","12","433","146"],
    ["Grace Eli","556","1417","0","0","19","1131","99","0"],
    ["Anna Hudson","0","688","7","386","0","185","777","577"],
    ["Mackenzie John","643","179","440","601","1179","4688","0","2670"],
    ["Kaylee Zane","53","431","0","0","2284","361","587","25"],
    ["Victoria Connor","30","0","88","0","1470","194","0","67"]]

get_steps_diff_table_header :: Row
get_steps_diff_table_header = ["Name","Average first 4h","Average last 4h","Difference"]

row_steps_to_sum :: Row -> Float
row_steps_to_sum = foldr (\x acc -> acc + (read x :: Float)) 0
row_steps_to_avg :: Row -> Float
row_steps_to_avg l = row_steps_to_sum l / 4

get_steps_names :: Table -> Row
get_steps_names t = head (transpose t)

get_steps_first4h :: Table -> [Float]
get_steps_first4h t = map row_steps_to_avg $ map (take 4) $ map (tail) t

get_steps_last4h :: Table -> [Float]
get_steps_last4h t = map row_steps_to_avg $ map (drop 4) $ map (tail) t

get_steps_diff :: [Float] -> [Float] -> [Float]
get_steps_diff l1 l2 = map abs (zipWith (-) l1 l2)

float_list_to_row2 :: [Float] -> Row
float_list_to_row2 = foldr (\x acc -> (printf "%.2f" x):acc) []

compute_4_rows_table :: Table -> Table
compute_4_rows_table t = transpose ([(get_steps_names t)] ++ [(float_list_to_row2 $ get_steps_first4h t)] ++ [(float_list_to_row2 $ get_steps_last4h t)] ++ [(float_list_to_row2 $ get_steps_diff (get_steps_first4h t) (get_steps_last4h t))])

get_steps_diff_table :: Table -> Table
get_steps_diff_table t = [get_steps_diff_table_header] ++ sortBy (\p1 p2 -> compare (read $ (p1 !! 3) :: Float) (read $ (p2 !! 3) :: Float)) (compute_4_rows_table (tail t))


-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f m = undefined


-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = undefined


get_sleep_total :: Row -> Row
get_sleep_total r = undefined
