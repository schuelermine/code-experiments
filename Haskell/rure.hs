import Data.Char
g 'ё' = Just "ö"
g 'я' = Just "ä"
g 'в' = Just "v"
g 'е' = Just "ë"
g 'р' = Just "r"
g 'т' = Just "t"
g 'з' = Just "z"
g 'у' = Just "u"
g 'и' = Just "i"
g 'о' = Just "o"
g 'п' = Just "p"
g 'ш' = Just "c"
g 'щ' = Just "ċ"
g 'ъ' = Just "·"
g 'а' = Just "a"
g 'с' = Just "s"
g 'д' = Just "d"
g 'ф' = Just "f"
g 'г' = Just "g"
g 'х' = Just "h"
g 'й' = Just "y"
g 'к' = Just "k"
g 'л' = Just "l"
g 'ч' = Just "tc"
g 'э' = Just "e"
g 'ю' = Just "ü"
g 'ы' = Just "ı"
g 'ж' = Just "j"
g 'ц' = Just "ts"
g 'ь' = Just "’"
g 'б' = Just "b"
g 'н' = Just "n"
g 'м' = Just "m"
g _ = Nothing

g1 c = case g c of
    Just a -> a
    Nothing -> case g (toLower c) of
        Just a -> a
        Nothing -> [c]