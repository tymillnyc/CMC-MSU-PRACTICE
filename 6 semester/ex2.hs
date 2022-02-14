import Data.Char

main :: IO ()
main =  do print(capitalize ["hello", "darkness", "my", "old", "friend"])

capitalize :: [String] -> [String]
capitalize [] = []
capitalize (elem:list) = (checkEmptyWorld elem):capitalize list

checkEmptyWorld:: String -> String
checkEmptyWorld "" = ""
checkEmptyWorld elem = toUpper(head(elem)) : tail(elem)

