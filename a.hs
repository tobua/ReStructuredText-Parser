-- ##################################################################################################################
-- ReStructuredText Parser - 2013 - Matthias Giger ##################################################################
-- ##################################################################################################################

import Text.ParserCombinators.Parsec

-- Read the file, parse the input and print the result

main = do contents <- readFile "b.rest"
          let results = parseAll (contents)
          writeFile "c.tex" results

-- ## General #######################################################################################################

-- Apply all Parsing functions to the input
parseAll :: String -> String
parseAll input =  addHeader $ apply' parseItemizedList $ apply' parseSection $ apply' parseSubsection $ apply parseStrong $ apply parseItalic input

-- Adds LaTeX document specific header
addHeader :: String -> String
addHeader input = "\\documentclass{article}\n\\begin{document}\n" ++ input ++ "\\end{document}"

-- Apply the parser and convert result back to parsable string
apply :: (String -> Either ParseError [String]) -> String -> String
apply parser input = convertToString $ parser input

apply' :: (String -> Either ParseError String) -> String -> String
apply' parser input = convertToString' $ parser input

-- Converts a list of string to s string
convertToString :: Either ParseError [String] -> String
convertToString (Left xs) = "ParseError"
convertToString (Right []) = [] 
convertToString (Right (x:[])) = x ++ "\n"
convertToString (Right (x:xs)) = x ++ "\n" ++ (convertToString (Right xs))

convertToString' :: Either ParseError String -> String
convertToString' (Left (xs)) = "ParseError"
convertToString' (Right (xs)) = xs

-- ## Strong Text ###################################################################################################

-- Look for strong formatted text
parseStrong :: String -> Either ParseError [String]
parseStrong input = parse strongText "(unknown)" input

strongText = endBy line eol

line = 
         do
              outside <- many content
	      formatted <- isStrongComing
	      end <- isMoreComing
	      return (outside ++ formatted ++ end)

content = noneOf ['*', '\n']

isStrongComing = 
           ((try (string "**")) >> strong)
           <|> return []

isMoreComing = 
                (lookAhead (noneOf ['\n']) >> line)
                <|> return []

strong = do
	    inside <- many content
            string "**" <?> "no ending for strong text"                                                           --
	    return ("\\textbf{" ++ inside ++ "}")

eol = char '\n' <?> "end of line missing"

-- ## Italic Text ###################################################################################################

-- Look for italic formatted text
parseItalic :: String -> Either ParseError [String]
parseItalic input = parse italicText "(unknown)" input

italicText = endBy italicLine eol

italicLine = 
         do
              outside <- many italicContent
	      formatted <- isItalicComing
	      end <- isMoreNonItalicComing
	      return (outside ++ formatted ++ end)

italicContent = noneOf ['*', '\n']

isItalicComing = 
           (try (string "**"))
           <|> (try (char '*') >> italic)
           <|> return []

isMoreNonItalicComing = 
                (lookAhead (noneOf ['\n']) >> italicLine)
                <|> return []

italic = do
	    inside <- many content
            char '*' <?> "no ending for strong text"
	    return ("\\textit{" ++ inside ++ "}")

-- ## Section #######################################################################################################

parseSection :: String -> Either ParseError String
parseSection input = parse title "(unknown)" input

title = 
        do
           regular <- regularLine
           title <- titleLine (length (last regular))
           end <- isMoreNonTitleComing
	   return (makeStr (init regular) ++                                                                       --
                             (if (title == "True") then ("\\section{" ++ (last regular) ++ "}\n")
                                                   else ((last regular) ++ "\n" ++ title))
                   ++ end)

makeStr [] = []                                                                                                    --
makeStr (x:[]) = x ++ "\n"
makeStr (x:xs) = x ++ "\n" ++ makeStr xs

regularLine = endBy (many (noneOf ['=', '\n'])) eol

-- check if the line of '=' matches the length of the title
titleLine titleLength = (lookAhead (try (string (take titleLength ['=','='..])))                                   --
                        >> (try (string ((take titleLength ['=','='..]) ++ "\n"))) 
                        >> (return "True")) 
                        <|> many anyChar

isMoreNonTitleComing = 
                ((lookAhead (noneOf ['\n'])) >> title)
                <|> return []

-- ## Subsection ####################################################################################################

parseSubsection :: String -> Either ParseError String
parseSubsection input = parse subsection "(unknown)" input

subsection = do regular <- ssRegularLine
                ss <- ssLine (length (last regular))
                end <- isMoreNonSubsectionComing
	        return (makeStr (init regular) ++ 
                        (if (ss == "True") then ("\\subsection{" ++ (last regular) ++ "}\n")
                                           else ((last regular) ++ "\n" ++ ss))
                       ++ end)

ssRegularLine = endBy (many (noneOf ['-', '\n'])) eol

ssLine titleLength = (lookAhead (try (string (take titleLength ['-','-'..]))) 
                        >> (try (string ((take titleLength ['-','-'..]) ++ "\n"))) 
                        >> (return "True")) 
                        <|> many anyChar

isMoreNonSubsectionComing = (lookAhead (noneOf ['\n']) >> subsection) 
                            <|> return []

-- ## List Itemize ##################################################################################################

-- Look for a '-' list
parseItemizedList :: String -> Either ParseError String
parseItemizedList input = parse list1 "(unknown)" input

list1 = do list <- many1 listOrRegular
           return (foldr (++) [] list)                                                                             --

listOrRegular = list1Parser <|> list1Regular

list1Parser = do list <- many1 list1More
                 return (
                          if ((length list) > 0) 
                          then ("\\begin{itemize}\n" ++ (createList1Items list) ++ "\\end{itemize}\n") 
                          else ""
                        )

createList1Items ([]) = []
createList1Items (x:[]) = "\\item " ++ x ++ "\n"
createList1Items (x:xs) = "\\item " ++ x ++ "\n" ++ (createList1Items xs)

list1More = do char '-'
               char ' '
               list <- many (noneOf ['\n'])
               char '\n'
               return list

list1Regular = do a <- try (lookAhead (string "- ") >> return "") 
                       <|> try (lookAhead (noneOf ['-']) >> return "") 
                       <|> many1 (char '-')
                  b <- many1 (noneOf ['-'])
                  c <- try (lookAhead (eof) >> return "") 
                       <|> try (lookAhead (string "- ") >> return "") 
                       <|> list1Regular
                  return (a ++ b ++ c)

-- ## List Enumerate ################################################################################################

-- Look for a '-' list
parseEnumeratedList :: String -> Either ParseError String
parseEnumeratedList input = parse list2Regular "(unknown)" input

list2 = do list <- many1 $ list2Parser <|> list2Regular
           return (foldr (++) [] list)

list2Parser = do list <- many1 list2More
                 return (
                          if ((length list) > 0) 
                             then ("\\begin{enumerate}\n" ++ (createList2Items list) ++ "\\end{enumerate}\n") 
                             else ""
                        )

-- TODO: Check if this function can be consolidated with the one from above
createList2Items ([]) = []
createList2Items (x:[]) = "\\item " ++ x ++ "\n"
createList2Items (x:xs) = "\\item " ++ x ++ "\n" ++ (createList1Items xs)

list2More = do digit
               char '.'
               char ' '
               list <- many (noneOf ['\n'])
               char '\n'
               return list

list2Regular = do a <- l2list
                       <|> many ((try $ lookAhead $ oneOf "0123456789") <|> anyChar)                               --
                  c <- try (lookAhead (eof) >> return "eof")
                       <|> list2Regular
                  return (a ++ c)

l2list = try ((try (many1 l2Marker) >>  (string ". ")) >> return "start")

l2nonMarker =  do ((try $ lookAhead $ char '6') >> return 'a') 
                  <|> anyChar
                  <|> noneOf "a"

l2Marker = digit