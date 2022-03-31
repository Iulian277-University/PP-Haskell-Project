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
threshold :: Float
threshold = 1000
get_passed_people_num :: Table -> Int
get_passed_people_num t = (foldr (\x acc -> if x >= threshold then acc + 1 else acc) 0) $ (map compute_steps_for_row (tail t)) 

-- Percentage of people who have achieved their goal:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage t = (int2Float $ get_passed_people_num t) / (int2Float $ length t)

-- Average number of daily steps
get_steps_total :: Table -> Float
get_steps_total t = (foldr (\x total_steps -> total_steps + x) 0) $ (map compute_steps_for_row (tail t)) 

get_steps_avg :: Table -> Float
get_steps_avg t = (get_steps_total t) / (int2Float $ (length t - 1))

-- Task 3

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m = undefined


-- Task 4

get_activ_summary :: Table -> Table
get_activ_summary m = undefined


-- Task 5

get_ranking :: Table -> Table
get_ranking m = undefined


-- Task 6

get_steps_diff_table :: Table -> Table
get_steps_diff_table m = undefined


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
