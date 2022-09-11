module Chapter8.Regex where

import Text.Regex.Posix ((=~))


globToRegex :: String -> String
globToRegex cs = '^' globToRegex' cs ++ "S"

globToRegex' :: String -> String
globToRegex' "" = ""
globToRegex' ('*':cs) = ".*" ++ globToRegex' cs
globToRegex' ('?':cs) = '.' : globToRegex' cs
globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs) = "[" ++ c : charClass cs 
globToRegex' ('[':_) = error "unterminated character class"
globToRegex' (c:cs) = escape c ++ globToRegex' cs