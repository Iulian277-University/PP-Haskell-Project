
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "" #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array
import GHC.Float (int2Float)
import Data.Maybe
import Text.Read (readMaybe)

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

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
-- Average number of steps per given day --

-- Header of table
header_of_table :: Row
header_of_table = ["Name", "Average Number of Steps"]

-- Compute the total steps from that given row
compute_steps_for_row :: Row -> Float
compute_steps_for_row r = foldr (\x acc -> (read x :: Float) + acc) 0 (tail r)

-- Generate output in the desired format (average with 2 decimal places)
compute_results_for_row :: Row -> Row
compute_results_for_row r = [head r, printf "%.2f" (compute_steps_for_row r / 8)]

-- For each entry, calculate the `average` number of steps
compute_average_steps :: Table -> Table
compute_average_steps t = header_of_table : map compute_results_for_row (tail t)


-- Task 2
-- Number of people who have achieved their goal --

-- Customizable `threshold`
threshold_goal :: Float
threshold_goal = 1000

-- Count the total number of people with more steps than `threshold_goal`
get_passed_people_num :: Table -> Int
get_passed_people_num t = foldr
    (\x acc -> if x >= threshold_goal then acc + 1 else acc) 0 (map compute_steps_for_row (tail t))

-- Percentage of people who have achieved their goal
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage t = int2Float (get_passed_people_num t) / int2Float (length t)

-- Total number of steps (for all people)
get_steps_total :: Table -> Float
get_steps_total t = foldr (\x total_steps -> total_steps + x) 0 (map compute_steps_for_row t)

-- Average number of steps (for all people)
get_steps_avg :: Table -> Float
get_steps_avg t = (get_steps_total (tail t)) / (int2Float (length t - 1))


-- Task 3
-- I need to compute the sum on columns
-- For that, I will transpose the matrix (table) and compute sum on rows

-- For each hour, compute the `total steps` for each hour (H10, H11, ..., H17)
get_total_steps_per_h :: Table -> [Float]
get_total_steps_per_h t = map compute_steps_for_row $ tail (transpose t)

-- Compute the `average steps` for each hour (H10, H11, ..., H17) 
get_avg_steps_per_h_list :: Table -> [Float]
get_avg_steps_per_h_list t = map (\x -> x / (int2Float (length t - 1))) (get_total_steps_per_h t)

-- Convert to the desired output
float_list_to_row :: Table -> Row
float_list_to_row t = map (printf "%.2f") (get_avg_steps_per_h_list t)

-- Header containing the hours
steps_per_h_header :: Row
steps_per_h_header = ["H10", "H11", "H12", "H13", "H14", "H15", "H16", "H17"]

-- Combine the `header` with computed `avearage steps`
get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h t = steps_per_h_header : [float_list_to_row t]


-- Task 4
-- Count minutes grouped by intensity  --

-- Ranges
range1_lo :: Integer
range1_lo = 0
range1_hi :: Integer
range1_hi = 50

range2_lo :: Integer
range2_lo = range1_hi
range2_hi :: Integer
range2_hi = 100

range3_lo :: Integer
range3_lo = range2_hi
range3_hi :: Integer
range3_hi = 500

-- VeryActiveMinutes    - at pos (length (head t) - 3)
-- FairlyActiveMinutes  - at pos (length (head t) - 2)
-- LightlyActiveMinutes - at pos (length (head t) - 1)

-- Functions which takes a current value `x`, an `acc`, 2 `interval limits`
-- and returns 1 if the `x` is in range and 0 otherwise
check_in_range :: Value -> Integer -> Integer -> Integer -> Integer
check_in_range x acc range_lo range_hi = if ((read x :: Integer) >= range_lo) && ((read x :: Integer) < range_hi)
    then acc + 1
    else acc

-- For each entry (row), count how many minutes are in the range [range_lo, range_hi)
group_range :: Integer -> Integer -> Row -> Integer
group_range range_lo range_hi = foldr (\x acc -> check_in_range x acc range_lo range_hi) 0

-- Count the minutes for each range: `range_1`, `range_2`, `range_3`
range1_group :: Row -> Integer
range1_group r = group_range range1_lo range1_hi (tail r)
range2_group :: Row -> Integer
range2_group r = group_range range2_lo range2_hi (tail r)
range3_group :: Row -> Integer
range3_group r = group_range range3_lo range3_hi (tail r)

-- Group minutes based on the range
group_minutes :: Row -> Row
group_minutes r = [head r, show (range1_group r), show (range2_group r), show (range3_group r)]

-- `activ_summary` for each intensity (`VeryActiveMinutes`, `FairlyActiveMinutes`, `LightlyActiveMinutes`)
activ_summary_intensity_VA :: Table -> Row
activ_summary_intensity_VA t = group_minutes $ (transpose t) !! (length (head t) - 3)
activ_summary_intensity_FA :: Table -> Row
activ_summary_intensity_FA t = group_minutes $ (transpose t) !! (length (head t) - 2)
activ_summary_intensity_LA :: Table -> Row
activ_summary_intensity_LA t = group_minutes $ (transpose t) !! (length (head t) - 1)

-- Header of `activ_summary`
activ_summary_header :: Row
activ_summary_header = ["column", "range1", "range2", "range3"]

-- Compute the desired output using all the above functions
get_activ_summary :: Table -> Table
get_activ_summary t = [activ_summary_header,
    activ_summary_intensity_VA t,
    activ_summary_intensity_FA t,
    activ_summary_intensity_LA t]


-- Task 5
-- Sort people ascending by their total number of steps --

-- Header
ranking_header :: Row
ranking_header = ["Name", "Total Steps"]

-- Sort the people by comparing the total number of steps (second column in the table -> idx = 1)
get_ranking :: Table -> Table
get_ranking t = ranking_header :
    sortBy (\p1 p2 -> compare (read (p1 !! 1) :: Integer) (read (p2 !! 1) :: Integer)) (map (take 2) (tail t))


-- Task 6
-- Compute the difference between 2 parts of the day regarding the number of steps --
-- Create a table with 4 columns: `Name`, `Average first 4h`, `Average last 4h`, `Difference` --

-- Header of the table
steps_diff_table_header :: Row
steps_diff_table_header = ["Name","Average first 4h","Average last 4h","Difference"]

-- Compute the `total steps` from a given row
row_steps_sum :: Row -> Float
row_steps_sum = foldr (\x acc -> acc + (read x :: Float)) 0

-- Compute the `average steps` from a given row 
row_steps_avg :: Row -> Float
row_steps_avg r = row_steps_sum r / 4

-- Select the first column of the table (names)
get_steps_names :: Table -> Row
get_steps_names t = head (transpose t)

-- Compute a list with `avg_steps` for the first 4h (list[i] = avg_steps_first4h_for_person_i)
-- map (tail) t -> remove the first column of the table
-- map (take) x -> keep only the first `x` columns from the table
-- map (drop) x -> keep only the last  `x` columns from the table
get_steps_first4h :: Table -> [Float]
get_steps_first4h t = map row_steps_avg $ map (take 4) $ map (tail) t

-- Compute a list with `avg_steps` for the last 4h (list[i] = avg_steps_last4h_for_person_i)
get_steps_last4h :: Table -> [Float]
get_steps_last4h t = map row_steps_avg $ map (drop 4) $ map (tail) t

-- Compute the absolute difference between 2 lists 
get_steps_diff :: [Float] -> [Float] -> [Float]
get_steps_diff l1 l2 = map abs (zipWith (-) l1 l2)

-- Convert to the desired output: from [Float] to [String] (Row)
float_list_to_row2 :: [Float] -> Row
float_list_to_row2 = map (printf "%.2f")

-- Let's compute the columns of the final table
diff_table_first4h :: Table -> Row
diff_table_first4h t = float_list_to_row2 $ get_steps_first4h t
diff_table_last4h :: Table -> Row
diff_table_last4h t = float_list_to_row2 $ get_steps_last4h t
diff_table_diff :: Table -> Row
diff_table_diff t = float_list_to_row2 $ get_steps_diff (get_steps_first4h t) (get_steps_last4h t)

-- Merge all the columns and generate the table
compute_4_rows_table :: Table -> Table
compute_4_rows_table t = transpose
    ([get_steps_names t] ++ [diff_table_first4h t] ++ [diff_table_last4h t] ++ [diff_table_diff t])

-- Sort the `final table` based on the `diff` column (idx = 3) and append the header
get_steps_diff_table :: Table -> Table
get_steps_diff_table t = steps_diff_table_header :
    sortBy (\p1 p2 -> compare (read (p1 !! 3) :: Float) (read (p2 !! 3) :: Float)) (compute_4_rows_table (tail t))


-- Task 7
-- Implement a function which applies a function to all values in a table --
table_test_task7 :: Table
table_test_task7 =
    [["Olivia Noah","373","160","151","0","","","","0"],
    ["Riley Jackson","31","","0","7","0","","0","0"],
    ["","45","8","0","0","0","0","0","0"],
    ["Aria Lucas","0","0","0","0","0","0","0","0"],
    ["Aaliyah Oliver","0","","0","0","4","0","20","0"],
    ["Amelia Caden","0","0","","0","0","0","0","847"],
    [""]]

-- Applies the given function (f) to all the values from the table (t)
vmap :: (Value -> Value) -> Table -> Table
vmap f = map (map f)

-- This function will replace the null string "" with "NaN" (similar to pandas framework)
f_test :: Value -> Value
f_test x = if x == "" then "NaN" else x

-- Tester function
vmap_test :: Table
vmap_test = vmap f_test table_test_task7


-- Task 8
-- Implement a function which applies a function (f) to all rows, then add a column to the table --
table_test_task8 :: Table
table_test_task8 =
    [["Olivia Noah","373","160","151"],
    ["Riley Jackson","31","0","0","7","0","0","0","0"],
    ["Emma Aiden","0","0"],
    ["Aria Lucas","0","0","0","0","0","0","0","0"],
    ["Aaliyah Oliver"],
    ["Amelia Caden","0","0","0","0","0","0","0","847"],
    ["Layla Muhammad","29","0","0","0","0","0","319","225","159","223"]]

-- Applies the given function to all the entries (rows)
-- Apply the function `f` to each row of the table `t`
-- Then remove the first column of the table
-- Lastly, merge the table with the given [String] `s` 
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s t = zipWith (:) s $ map (tail . f) t

-- Customizable `threshold`
threshold_length :: Int
threshold_length = 5
-- This function appends (at the end) "Big" if the row has more
--than a `threshold_length` elements and "Small" otherwise
f_row_test :: Row -> Row
f_row_test r = if length r > threshold_length
    then reverse("Big":(reverse r))
    else reverse("Small":(reverse r))

-- Apply the function `f_row_test` to the table `table_test_task8`
-- and change the column names with the given ones
rmap_test :: Table
rmap_test = rmap f_row_test ["Name1", "Name2", "Name3", "Name4", "Name5", "Name6", "Name7"] table_test_task8

-- Compute the sum of slept minutes, given a row of minutes
get_total_slept_mins :: Row -> Float
get_total_slept_mins = foldr (\x acc -> (read x :: Float) + acc) 0

-- Merge the header (`email`) with the computed `total_slept_mins`, printing with 2 decimal places
get_sleep_total :: Row -> Row
get_sleep_total r = (head r) : [printf "%.2f" $ get_total_slept_mins (tail r)]




{-
    TASK SET 2
-}

-- Return the index of the first occurrence (if any) of value in the list
-- We are sure that the `column_name` is present in the table
get_column_index :: ColumnName -> Table -> Int
get_column_index c t = fromJust $ elemIndex c (head t)


eight_hours_21 = 
    [["Name","10","11","12","13","14","15","16","17"],
    ["Olivia Noah","373","160","151","0","0","0","0","0"],
    ["Riley Jackson","31","0","0","7","0","0","0","0"],
    ["Emma Aiden","45","8","0","0","0","0","0","0"],
    ["Aria Lucas","0","0","0","0","0","0","0","0"],
    ["Aaliyah Oliver","0","0","0","0","4","0","20","0"],
    ["Amelia Caden","","0","0","0","0","0","0","847"],
    ["Layla Muhammad","29","0","0","0","0","0","319","225"]]

emails_21 = 
    [["Name","Email"],
    ["Olivia Noah","Olivia.Noah@stud.cs.pub.ro"],
    ["Riley Jackson","Riley.Jackson@stud.cs.pub.ro"],
    ["","Emma.Aiden@stud.cs.pub.ro"],
    ["Eva Elijah","Ava.Elijah@stud.cs.pub.ro"],
    ["Isabela Grayson","Emma.Aiden@stud.cs.pub.ro"],
    ["Aria Lucas","Aria.Lucas@stud.cs.pub.ro"]]

-- Task 1
-- Sort ascending the table `t` based on a column `c`
-- If multiple entries have the same values, then it's sorted by the first column 
tsort :: ColumnName -> Table -> Table
tsort c t = (head t) : sortBy (\entry1 entry2 -> compare_aux entry1 entry2) (tail t)
    where
        compare_aux entry1 entry2
            | entry1 !! (get_column_index c t) == "" = LT
            | entry2 !! (get_column_index c t) == "" = GT
            | (readMaybe (entry1 !! (get_column_index c t)) :: Maybe Double) == Nothing = -- strings
                compare ((entry1 !! (get_column_index c t)), (entry1 !! 0))
                        ((entry2 !! (get_column_index c t)), (entry2 !! 0))
            | otherwise = -- values
                 compare ((read (entry1 !! (get_column_index c t)) :: Double), (entry1 !! 0)) 
                         ((read (entry2 !! (get_column_index c t)) :: Double), (entry2 !! 0))

-- Base implementation
-- tsort :: ColumnName -> Table -> Table
-- tsort c t =
--     (head t) :
--     sortBy (\entry1 entry2 ->
--         compare
--             ((read (entry1 !! (get_column_index c t)) :: Double), (entry1 !! 0))
--             ((read (entry2 !! (get_column_index c t)) :: Double), (entry2 !! 0)))
--     (tail t)

-- Task 2
-- t1 = [[col_x1, col_x2, ...]] and t2 = [[col_y1, col_y2, ...]]
-- if col_x* == col_y* then append rows_t2 to rows_t1
-- otherwise = t1 remains unchanged
vunion :: Table -> Table -> Table
vunion t1 t2
    | (head t1) /= (head t2) = t1
    | otherwise = t1 ++ (tail t2)


-- Task 3
-- Add padding ("") to a table `t` given the expected number of rows `r`
add_padding :: Table -> Int -> Table
add_padding t r                         -- number of cols from table t
    | length t < r = add_padding (t ++ [replicate (length $ head t) ""]) r
    | otherwise = t

-- Extends each row of `t1` with a row of `t2`, adding the padding if necessary
hunion :: Table -> Table -> Table
hunion t1 t2 = zipWith (++)
    (add_padding t1 $ max (length t1) (length t2))
    (add_padding t2 $ max (length t1) (length t2))


-- Task 4
-- We are sure that the `column_name` is present in the `row`
get_column_index_from_row :: ColumnName -> Row -> Int
get_column_index_from_row column_name row = fromJust $ elemIndex column_name row

-- For each row from `t2`, compare the value `row_t1` with respect to the `key_column` index
-- If they are the same, concatenate the rows (without the `key_column` from the second row)
-- Otherwise, return only the empty list `[]` - This allows us to filter the empty lists from the final table
tjoin_helper :: ColumnName -> Int -> Row -> Table -> Row
tjoin_helper key_column key_idx_t1 row_t1 t2 =
    foldr (\row_t2 acc ->
        if (row_t2 !! (get_column_index key_column t2))    ==   (row_t1 !! key_idx_t1)
            then row_t1 ++ (take (get_column_index_from_row     (row_t1 !! key_idx_t1) row_t2) row_t2 ++
                           (drop (1 + get_column_index_from_row (row_t1 !! key_idx_t1) row_t2) row_t2))
            else acc
    ) [] t2

-- For each row from `t1`, search through the rows of table `t2` for the column named `key_colum`
tjoin_tables :: ColumnName -> Table -> Table -> Table
tjoin_tables key_column t1 t2 =
    filter (\row -> length row > 0)
        (foldr (\row_t1 acc -> (tjoin_helper key_column (get_column_index key_column t1) row_t1 t2) : acc) [] t1)

-- Remove the columns from the `t1` if there is another column in the `t2` with the same name
tjoin_override :: Table -> Table
tjoin_override t =
    transpose $
    foldr (\row acc ->
        if (length (elemIndices (head row) (head t)) > 1)
            then transpose (map (take (head $ elemIndices (head row) (head t))) t) ++
                 transpose (map (drop (1 + (head $ elemIndices (head row) (head t)))) t)
            else
                acc
    ) [[]] (transpose t)

-- Join 2 tables `t1` and `t2` with respect to a key (column name)
tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2
    | length (head $ tjoin_tables key_column t1 t2) == length (nub $ head $ tjoin_tables key_column t1 t2)
                = tjoin_tables key_column t1 t2
    | otherwise = tjoin_override (tjoin_tables key_column t1 t2)


-- Task 5
-- This function takes an operation `op`, a row `row_t1` and the table `t2`
-- Applies the `op` between `row_t1` and each row of `t2`
cartesian_helper :: (Row -> Row -> Row) -> Row -> Table -> Table
cartesian_helper new_row_function row_t1 t2 = map (new_row_function row_t1) (tail t2)

-- For each row from `t1` call the `cartesian_helper` function
cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 =
    new_column_names : foldr (\row_t1 acc -> (cartesian_helper new_row_function row_t1 t2) ++ acc) [] (tail t1)

-- Task 6
-- General case for `projection` - it extracts only the given columns from the table `t`
projection_helper :: [ColumnName] -> Table -> Table
projection_helper columns_to_extract t =
    foldr (\col table -> (map (!! (get_column_index col t)) t) : table) [] columns_to_extract

-- If the `columns_to_extract` has only one element, we need to convert from [[String]] to [[String],[String],...]
-- Otherwise, just transpose the result of the `helper` function to get the correct orientation of the table
projection :: [ColumnName] -> Table -> Table
projection columns_to_extract t
    | length (projection_helper columns_to_extract t) == 1 = map (\x -> [x]) (concat (projection_helper columns_to_extract t))
    | otherwise = transpose (projection_helper columns_to_extract t)

-- Task 7
-- For each row (entry), keep the entry if the condition is met
filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = (head t) : (filter (\row -> condition (row !! (get_column_index key_column t))) (tail t))
