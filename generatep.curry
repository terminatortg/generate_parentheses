-- https://leetcode.com/problems/generate-parentheses/description
-- Given n pairs of parentheses, write a function to generate
-- all combinations of well-formed parentheses.

-- For example, given n = 3, a solutions set is:
-- [
--   "((()))",
--   "(()())",
--   "(())()",
--   "()(())",
--   "()()()"
-- ]

import SetFunctions
import Test.Prop

-- Comprehension to generate a String of '('
generateOpen :: Int -> [Char]
generateOpen  x = ['(' | _ <- [1..x]]

-- Comprehension to generate a String of ')'
generateClose :: Int -> [Char]
generateClose x = [')' | _ <- [1..x]]

-- GenerateP function takes an integer
-- and outputs the valid combinations of 
generateP :: Int -> [Char]
generateP x = (head gOpen):(aux (tail gOpen) (generateClose x)) where
  gOpen = generateOpen x
  aux [] [] = ""
  aux [] (c:cs) = c:(aux [] cs)
  aux open@(o:os) close@(c:cs)
    | length close < length open = failed
    | otherwise = o:(aux os close) ? c:(aux open cs)

threeParentheses :: [String]
threeParentheses = sortValues (set1 generateP 3)

fourParentheses :: [String]
fourParentheses = sortValues (set1 generateP 4)
 
threeP :: Test.Prop.Prop
threeP = threeParentheses -=- ["((()))", "(()())", "(())()", "()(())", "()()()"]
