-- Removing uppercase leters
removeUppercase :: String -> String
removeUppercase ru = [x | x <- ru, x `elem` ['a'..'z']]