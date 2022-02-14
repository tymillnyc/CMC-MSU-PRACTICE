main :: IO ()
main = do print(qsort[1,10,2,15])
qsort :: [Int] -> [Int]
qsort [] = []
qsort (elem:list) = (qsort less) ++ [elem] ++ (qsort great)
    where less = filter (<= elem) list
          great = filter (> elem) list
