
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array
import GHC.Float (int2Float)
import Data.Maybe
import Text.Read (readMaybe)

import Common

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

-- For each row from `t1`, search through the rows of table `t2` for the column named `key_column`
tjoin_tables :: ColumnName -> Table -> Table -> Table
tjoin_tables key_column t1 t2 =
    filter (\row -> length row > 0)
        (foldr (\row_t1 acc -> (tjoin_helper key_column (get_column_index key_column t1) row_t1 t2) : acc) [] t1)

-- Remove the columns from `t1` if there is another column in `t2` with the same name
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

{-
    TASK SET 3
-}


-- 3.1
data Query =
    FromTable Table
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query -- 3.3
    | Graph EdgeOp Query -- 3.4

-- Enroll `QResult` in the class `Show`
instance Show QResult where
    show (List  l) = show l
    show (Table t) = show t

-- Evaluation class for `Query`
class Eval a where
    eval :: a -> QResult

qresult_to_table :: QResult -> Table
qresult_to_table (Table t) = t

-- Enroll `Query` in class `Eval`
instance Eval Query where
    eval (FromTable t)                  = Table t
    eval (AsList c q)                   = List  $ tail $ concat $ projection [c] (qresult_to_table (eval q))
    eval (Sort c q)                     = Table $ tsort c (qresult_to_table (eval q))
    eval (ValueMap op q)                = Table $ vmap op (qresult_to_table (eval q))
    eval (RowMap op colnames q)         = Table $ colnames : (map op (tail $ qresult_to_table (eval q)))
    eval (VUnion q1 q2)                 = Table $ vunion (qresult_to_table (eval q1)) (qresult_to_table (eval q2))
    eval (HUnion q1 q2)                 = Table $ hunion (qresult_to_table (eval q1)) (qresult_to_table (eval q2))
    eval (TableJoin c q1 q2)            = Table $ tjoin c (qresult_to_table (eval q1)) (qresult_to_table (eval q2))
    eval (Cartesian op colnames q1 q2)  = Table $ cartesian op colnames (qresult_to_table (eval q1)) (qresult_to_table (eval q2))
    eval (Projection colnames q)        = Table $ projection colnames (qresult_to_table (eval q))

    eval (Filter filter_cond q) = 
        Table $ (head $ qresult_to_table (eval q)) : 
                filter (feval (head (qresult_to_table (eval q))) filter_cond) (tail $ qresult_to_table (eval q))

    eval (Graph edgeop q) = Table $ graph edgeop (qresult_to_table (eval q))


-- 3.2 & 3.3
type FilterOp = Row -> Bool

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

-- Enroll `Float` (FilterCondition Float) in class `FEval`
instance FEval Float where
    feval colnames (Eq colname ref)   = \row -> (read (row !! (get_column_index_from_row colname colnames)) :: Float) == ref
    feval colnames (Lt colname ref)   = \row -> (read (row !! (get_column_index_from_row colname colnames)) :: Float)  < ref
    feval colnames (Gt colname ref)   = \row -> (read (row !! (get_column_index_from_row colname colnames)) :: Float)  > ref
    feval colnames (In colname list)  = \row -> elem (read (row !! (get_column_index_from_row colname colnames)) :: Float) list
    feval colnames (FNot filter_cond) = \row -> not (feval colnames filter_cond row)
    feval colnames (FieldEq colname1 colname2) = \row ->
        (read (row !! (get_column_index_from_row colname1 colnames)) :: Float) == 
        (read (row !! (get_column_index_from_row colname2 colnames)) :: Float)

-- Enroll `String` (FilterCondition String) in class `FEval`
instance FEval String where
    feval colnames (Eq colname ref)   = \row -> (row !! (get_column_index_from_row colname colnames)) == ref
    feval colnames (Lt colname ref)   = \row -> (row !! (get_column_index_from_row colname colnames))  < ref
    feval colnames (Gt colname ref)   = \row -> (row !! (get_column_index_from_row colname colnames))  > ref
    feval colnames (In colname list)  = \row -> elem (row !! (get_column_index_from_row colname colnames)) list
    feval colnames (FNot filter_cond) = \row -> not (feval colnames filter_cond row) 
    feval colnames (FieldEq colname1 colname2) = \row ->
        (row !! (get_column_index_from_row colname1 colnames)) ==
        (row !! (get_column_index_from_row colname2 colnames))


-- 3.4
-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

graph_header = ["From", "To", "Value"]

-- Given the row `row_t1`, for each row of table `t`:
-- if the condition is met, then add that `edge` to the graph
graph_aux :: EdgeOp -> Row -> Table -> Table
graph_aux edgeop row_t1 t =
    foldr (\row_t2 acc -> if (edgeop row_t1 row_t2) == Nothing 
        then acc 
        else [[row_t1 !! 0, row_t2 !! 0, fromJust (edgeop row_t1 row_t2)]] ++ acc) [] (tail t)

-- For each row `row_t1` from table `t`, call `graph_aux`
-- on the table which begins from the next row of `row_t1`
graph_helper :: EdgeOp -> Table -> Table
graph_helper edgeop t = foldr (\row_t1 acc -> (graph_aux edgeop row_t1 (drop (fromJust $ elemIndex row_t1 t) t) ++ acc)) [] (tail t)

-- ["From", "To", "Value"]
-- "From" value should be lexicographically before "To" value ("interchange" them if needed) 
graph_sorter :: Table -> Table
graph_sorter t =
    foldr (\row acc -> if ((row !! 0) > (row !! 1)) then ([row !! 1, row !! 0, row !! 2] : acc) else (row : acc)) [] t

-- Compute and sort the graph, then add the graph header
graph :: EdgeOp -> Table -> Table
graph edgeop t = [graph_header] ++ nub (graph_sorter (graph_helper edgeop t))

-- 3.5
-- Compare 2 given rows (lists), using a tail-recursive approach
-- Alternative implementation: compare_lists a = length . filter id . zipWith (==) a
compare_lists :: [String] -> [String] -> Int
compare_lists [] _ = 0
compare_lists _ [] = 0
compare_lists l1 l2 =
    if (head l1 == head l2) then
        1 + compare_lists (tail l1) (tail l2)
    else
        compare_lists (tail l1) (tail l2)

-- Define the `edge operation` used in the `similarities query`
edge_op_sim (n1:l1) (n2:l2)
    | (compare_lists l1 l2 >= 5) = Just (show (compare_lists l1 l2) :: Value)
    | otherwise = Nothing

-- Compute the `similarities query` by generating the graph,
-- filtering the empty values from columns `From` and `To`,
-- then perform an ascending sort based on the column `Value`
similarities_query :: Query
similarities_query = Sort "Value"
                    $ Filter (FNot $ Eq "From" "")
                    $ Filter (FNot $ Eq "To" "")
                    $ FromTable (graph edge_op_sim eight_hours)

-- 3.6 (Typos)

-- Column `Name` can be everywhere (it is not supposed to be the first column)
-- Take the columns before the column `Name`, add the correct column names
-- and then append the columns after the initial column `Name` 
correct_table :: String -> Table -> Table -> Table
correct_table col_name t1 t2 =
    zipWith (++) 
        (zipWith (++) ((map (take (get_column_index col_name t1)) t1)) ([col_name] : (correct_table_aux col_name t1 t2)))
        (map (drop (1 + get_column_index col_name t1)) t1)


-- This function generates the `cartesian` between the columns of `t_wrong` and `t_good`
-- Then computes the `levenshtein` table with most likely replacements
-- After that, sorts those possible "correct_names" by the Levenshtein distance
-- At the end, replace the column `Name` from the wrong table with the corrected names
correct_table_aux :: String -> Table -> Table -> Table
correct_table_aux col_name t1 t2 = 
    replace_typos t1 t2 $
    sort_only_typos $
    compute_lev_table $
    cartesian (++) ["Name_wrong", "Name_good"] (projection [col_name] t1) (projection [col_name] t2)


-- Calculate Levenshtein distance between 2 strings (DP approach)
-- It uses memoization, because it's way faster than the tail-recursive method
-- This implementation is a variation of the following reference
-- Ref: https://en.wikipedia.org/wiki/Levenshtein_distance#Iterative_with_full_matrix
levenshtein :: String -> String -> Int
levenshtein x y = dp ! (m, n)
    where
        dp = listArray ((0, 0), (m, n)) [compute i j | i <- [0 .. m], j <- [0 .. n]]
        compute i 0 = i -- source prefixes can be transformed into empty string by dropping all characters
        compute 0 j = j -- target prefixes can be reached from empty source prefix by inserting every character
        compute i j     -- compute the `dp` matrix
            | x !! (i - 1) == y !! (j - 1) = (dp !) (i - 1, j - 1)
            | otherwise = 1 + (minimum $ map (dp !) [(i - 1, j),      -- deletion
                                                     (i    , j - 1),  -- insertion
                                                     (i - 1, j - 1)]) -- substitution
        m = length x
        n = length y


-- For each entry of the table `t_wrong`, compute the Levenshtein distance to each entry name from `t_good`
lev_thresh = 2
compute_lev_table :: Table -> Table
compute_lev_table t = foldr (\row acc -> if (get_lev_dist row /= "") then (row ++ [get_lev_dist row]) : acc else acc) [] (tail t)
    where get_lev_dist row
            | (levenshtein (row !! 0) (row !! 1) /= 0) &&
              (fromIntegral (levenshtein (row !! 0) (row !! 1))) <= (fromIntegral (length (row !! 0)) / (fromIntegral lev_thresh))
                        = show $ levenshtein (row !! 0) (row !! 1)
            | otherwise = []


-- Sort typos by the smallest Levenshtein distance (Later I will select the first occurence)
sort_only_typos :: Table -> Table
sort_only_typos t = sortBy (\[c1,w1,l1] [c2,w2,l2] -> compare (c1, read l1 :: Float) (c2, read l2 :: Float)) t


-- If there is already a perfect match, use that name
-- If we can correct the typo, choose the first entry (we've already sorted  the entries by the smallest Levenshtein distance)
-- If we can't correct the typo, use the initial misspelled name
replace_typos_aux :: Row -> Table -> Table -> Table
replace_typos_aux row_tw tg tc
    | find (\row -> row_tw !! 0 == row !! 0) tg == Nothing =
        if (find (\row -> row_tw !! 0 == row !! 0) tc) == Nothing
            then [row_tw]
            else [[(fromJust (find (\row -> row_tw !! 0 == row !! 0) tc)) !! 1]]
    | otherwise = [row_tw]


-- For each row of `t_wrong`, correct the `Name` (calling the `replace_typos_aux` function)
--              t_wrong  t_good  t_changed
replace_typos :: Table -> Table -> Table -> Table
replace_typos tw tg tc = foldr (\row_tw acc -> (replace_typos_aux row_tw tg tc) ++ acc) [] (tail (projection ["Name"] tw))
