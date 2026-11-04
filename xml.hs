import Parser

--Тип за XML------------------------------------------------------------------------------------------------------------
data Content = Text String | Children [XMLelement]
    deriving(Show)

data XMLelement = XMLelement {
                                name :: String
                                ,xmlID:: String
                                ,attributes :: [(String,String)]
                                ,content :: Content
                             }
    deriving(Show)

data Errors = MissingText String| NoChildren String| InvalidID String| MissingChild Int
    deriving (Show)

--Уникални идентификатори-----------------------------------------------------------------------------------------------„
uniqueID :: XMLelement -> [String] -> Int -> (XMLelement,[String],Int)
uniqueID el@(XMLelement n i a c) used count = (el {xmlID = newID,content = newContent},finalUsed,finalCount)
    where
        (newID,newUsed,newCount) --определя идентификатора на самия елемент
            |null i = let autoID = "auto" ++ show count in (autoID,autoID : used,count +1)
            |i `elem` used = let suffix = length (filter (==i) used) + 1 
                                 uniq = i ++ "_" ++ show suffix
                             in (uniq,uniq : used,count)
            |otherwise = (i,i:used,count)
        (newContent,finalUsed,finalCount) = case c of --определя идентификаторите на децата
                                                Text t -> (Text t,newUsed,newCount)
                                                Children list -> (Children updateChildren,updateUsed,updateCount)
                                                   where (updateChildren,updateUsed,updateCount) = uniqueChildren list newUsed newCount

--Рекурсивно прилага uniqueId за списък от деца
uniqueChildren ::[XMLelement] -> [String] -> Int -> ([XMLelement],[String],Int)
uniqueChildren [] used count = ([],used,count)
uniqueChildren (x:xs) used count = (updateX : updateRest,usedFinal,countFinal)
    where 
        (updateX,used',count') = uniqueID x used count
        (updateRest,usedFinal,countFinal) = uniqueChildren xs used' count'

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (x:xs) = case x of
                    Just _ -> x
                    Nothing -> firstJust xs
--test for uniqueID -> allIDs
{-
name1 = XMLelement {name="name", xmlID="n", attributes=[], content=Text "Alice"}
name2 = XMLelement {name="name", xmlID="n", attributes=[], content=Text "Bob"}
addr1 = XMLelement {name="address", xmlID="a", attributes=[], content=Text "NYC"}
addr2 = XMLelement {name="address", xmlID="", attributes=[], content=Text "London"}
person = XMLelement {name="person", xmlID="p", attributes=[], content=Children [name1, name2, addr1, addr2]}

(uniquePerson, _, _) = uniqueID person [] 0
allIDs = map xmlID (getChildren uniquePerson)
-}

findByID :: String -> XMLelement -> Maybe XMLelement
findByID i x@(XMLelement {xmlID = idVal,content = c})
    |i == idVal = Just x
    |otherwise = case c of
                    Text _ -> Nothing
                    Children list -> firstJust $ map (findByID i) list
--test for findByID -> findByID "n1" personEl
{-
nameEl :: XMLelement
nameEl = XMLelement {name = "name", xmlID = "n1", attributes = [],content = Text "Johnn"}
addressEl ::XMLelement
addressEl = XMLelement {name = "address", xmlID = "a1", attributes = [("type","home")],content = Text "London"}
personEl :: XMLelement
personEl = XMLelement {name = "person", xmlID = "p1", attributes = [("id","0")],content = Children[nameEl,addressEl]}
-}

--Функции за достъп до съдържанието на елементите---------------------------------------------------------------------
getText :: XMLelement -> Maybe String
getText (XMLelement n i a (Text t)) = Just t
getText (XMLelement n i a (Children _)) = Nothing

getChildren :: XMLelement -> [XMLelement]
getChildren (XMLelement n i a (Text _)) = []
getChildren (XMLelement n i a (Children list)) = list

getAttrValue :: String -> XMLelement -> Maybe String -- взима стойността на атрибут по ключ
getAttrValue key (XMLelement _ _ attrs _) = lookup key attrs
--lookup :: Eq a => a -> [(a, b)] -> Maybe b

getChild :: Int -> XMLelement -> Maybe XMLelement
getChild n el = let list = getChildren el 
    in if n < length list then Just (list !! n) else Nothing

--test for getText -> getText addressEl
--test for getChild -> getChild 0 personEl

text :: String -> XMLelement -> Either Errors String
text i root = case (findByID i root) of
                    Nothing -> Left (InvalidID i)
                    Just x -> case getText x of
                                Nothing -> Left (MissingText (name x))
                                Just t -> Right t
--test for text ->  text "n1" personEl // text "p1" personEl // text "kd" personEl

hasChildWithText :: String -> String -> XMLelement -> Bool
hasChildWithText childName text el = any match (getChildren el)
    where 
        match (XMLelement n _ _ (Text t)) = n==childName && t==text
        match (XMLelement n _ _ (Children list)) = hasChildWithText childName text (XMLelement n "" [] (Children list))

allDescendants :: XMLelement -> [XMLelement]
allDescendants el = el : concatMap allDescendants (getChildren el)

--Движение по стъпки------------------------------------------------------------------------------------------------
data Step = ByName String | ByIndex Int | ByAttr String | ByContent String | ByChildEquals String String | Descendant String
    deriving(Show)

--Можем да имаме 4 вида стъпки в пътя
--ByName - избираме само деца с конкретното име
--ByIndex - избираме n-тото дете от списъка
--ByAttr - избираме атрибут на текущите елементи
--ByContent - филтрираме децата по определено съдържание

--Функция, която реализира поведение подобно на Xpath
select :: [Step] -> [XMLelement] -> [XMLelement]
select [] elems = elems
--Търсим и взимаме I-тия елемент от текущия списък
select (ByName n : ByIndex i : ss) elems = select ss matched
    where 
        matched = concatMap pickIndex elems
        pickIndex el = if i >= 0 && i < length kids then [kids!!i] else []
            where kids = filter (\c -> name c == n) (getChildren el)

--Търси се елемент с име n, като за всеки текущ елемент се филтрират децата му, които носят името
select (ByName n : ss) elems = select ss matched
    where 
        matched = filter (\el -> name el == n) (concatMap getChildren elems)

select (ByIndex i : ss) elems = select ss picked
    where
        picked = concatMap pick elems
        pick el = if i >= 0 && i < length kids then [kids !! i] else []
            where kids = getChildren el

--Взимаме стойността на атрибута
select (ByAttr att : ss) elems = select ss attributs
    where 
        attributs = concatMap (\el -> case lookup att (attributes el) of
                                            Just  v -> [XMLelement att "" [] (Text v)]
                                            Nothing -> []) elems
--Взимаме текущите елементи, които имат дете с име n и текст c
select (ByContent c : ss) elems = select ss filtered
    where 
        filtered = filter hasChild elems
        hasChild el = any (\child-> getText child == Just c) (getChildren el) 

select (ByChildEquals n v : ss) elems = select ss (filter hasMatch elems)
    where hasMatch el = any (\c -> name c == n && getText c == Just v) (getChildren el)

select (Descendant n : ss) elems = select ss matched
    where matched = filter ((==n) . name) (concatMap allDescendants elems)
{-
-- tests for select
nameEl1 = XMLelement "name" "n1" [] (Text "John")
addressEl1 = XMLelement "address" "a1" [("type","home")] (Text "Brooklyn, NY")
addressEl2 = XMLelement "address" "a2" [("type","work")] (Text "Manhattan, NYC")
personEl1 = XMLelement "person" "p1" [("id","0")] (Children [nameEl1,addressEl1,addressEl2])
nameEl2 = XMLelement "name" "n2" [] (Text "Ivan")
addressEl3 = XMLelement "address" "a3" [("type","home")] (Text "Bulgaria")
personEl2 = XMLelement "person" "p2" [("id","1")] (Children [nameEl2,addressEl3])

allPeople = [personEl1, personEl2]
virtualRoot = [XMLelement "virtual" "" [] (Children allPeople)]

-- 1. Избиране на всички address елементи на person (/)
result1 = select [ByName "person", ByName "address"] virtualRoot
-- Очакван резултат: [addressEl1, addressEl2, addressEl3]
-- 2. Избиране на първия address на всеки person ([] индекс)
result2 = select [ByName "person", ByName "address", ByIndex 0] virtualRoot
-- Очакван резултат: [addressEl1, addressEl3]
-- 3. Избиране на всички address с атрибут type (@)
result3 = select [ByName "person", ByName "address", ByAttr "type"] virtualRoot
-- Очакван резултат: XML елементи с текстовите стойности на атрибут type ["home","work","home"]
-- 4. Филтриране на address с конкретно съдържание (ByContent)
result4 = select [ByName "person", ByName "address", ByContent "Bulgaria"] virtualRoot
-- Очакван резултат: [addressEl3]
-- 5. Комбинация: person с address="Bulgaria" -> name на този person
result5 = select [ByName "person", ByContent "Bulgaria", ByName "name"] virtualRoot
-- Очакван резултат: [nameEl2]
-- 6. Избиране на първия name на всеки person
result6 = select [ByName "person", ByName "name", ByIndex 0] virtualRoot
-- Очакван резултат: [nameEl1, nameEl2]
-}

--Парсър--------------------------------------------------------------------------------------------------------------
--Трябват допълнитебни функции, които да ни помогнат да работим с цели думи,интервали и структури, а не само с единични символи, както е в Parser.hs
isWS :: Char -> Bool
isWS c = c `elem` [' ','\t', '\n','\r','\f','\v']

isAlphaNumber :: Char -> Bool
isAlphaNumber c = isAlpha1 c || isDigit1 c
    where
        isAlpha1 ch = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
        isDigit1 ch = ch >= '0' && ch <= '9' 

myToLower :: Char -> Char
myToLower c
    |c>= 'A' && c <='Z' = toEnum (fromEnum c + 32)
    |otherwise = c

isDigit1 :: Char -> Bool
isDigit1 ch = ch >= '0' && ch <= '9'

--Функция,която премахва 1 или повече интервала 
whiteSpaces :: Parser String
whiteSpaces = many (sat isWS)  -- sat 

--Функция, която търси конкретна функция 
--Използваме например когато търсим </ за затваряне на таг
stringParser :: String -> Parser String
stringParser = traverse (\c -> sat (==c))
--traverse -> взима стринг и прилага (sat == c) за всеки символ, след което ги залепя в общ парсър

--Функция, която парсва атрибутите
attrParser :: Parser (String,String)
attrParser = do
    whiteSpaces
    key <- many1 (sat isAlphaNumber)
    _ <- sat (== '=')
    val <- between (sat (== '"')) (many (but (== '"'))) (sat (== '"'))
    return (key,val)

--Функция,която парсва стъпка и проверява дали дете с дадено име има конкретен текст
childEqualsParser :: Parser Step
childEqualsParser = do
    _ <- sat (=='[')
    childName <- many1 (sat isAlphaNumber)
    _ <- sat (=='=')
    val <- between (sat (=='"')) (many ( but(=='"'))) (sat (=='"'))
    _ <- sat (==']')
    return $ByChildEquals childName val

--Функция, която парсва стъпка и връща всички наследници с дадено име 
descendantParser :: Parser Step
descendantParser = do
    _ <- stringParser "//"
    Descendant <$> many1 (sat isAlphaNumber)

--Функция за самозатварящ се таг
selfParser :: Parser XMLelement
selfParser = do
    whiteSpaces >> sat (== '<')
    tagName <- many1 (sat isAlphaNumber)
    attrs <- many attrParser
    whiteSpaces >> stringParser "/>"
    let idValue = maybe "" id (lookup "id" attrs)
    return $XMLelement (map myToLower tagName) idValue attrs (Children [])

--Функция,която парсва
normalXmlElementParser :: Parser XMLelement
normalXmlElementParser = do 
    --отварящ таг
    whiteSpaces >> sat (=='<')
    nameOfTag <- many1 (sat isAlphaNumber)
    parsedAttributes <- many attrParser
    whiteSpaces >> sat (== '>')
    --съдържание
    cont <-choice [Children <$> many1 xmlElementParser, Text <$> many (but (== '<'))]
    --затварящ таг
    whiteSpaces >> stringParser "</"
    _ <- stringParser nameOfTag
    whiteSpaces >> sat (== '>')
    let idValue = maybe "" id (lookup "id" parsedAttributes)
    return $ XMLelement (map myToLower nameOfTag) idValue parsedAttributes cont
                      
xmlElementParser :: Parser XMLelement
xmlElementParser = choice [selfParser,normalXmlElementParser]

instance Parseable XMLelement where
    parser = xmlElementParser

--test for parser -> testParser
--testParser :: IO ()
--testParser = case run "<root><item id=\"A\">text</item></root>" :: Either ParseError XMLelement of
--    Right el -> print el
--    Left err -> print err

--Формат--------------------------------------------------------------------------------------------------------------
formatAttributes :: [(String,String)] -> String
formatAttributes [] = ""
formatAttributes a = concatMap (\(k,v) -> " " ++ k ++ "=\"" ++ v ++ "\"") a

formatId :: String -> String
formatId "" = ""
formatId i = "id=\"" ++ i ++ "\""

prettyPrint :: Int -> XMLelement ->String
prettyPrint k (XMLelement n i attr cont) = replicate k ' ' ++ "<" ++ n ++ formatAttributes attr ++ formatId i ++ ">" ++
                                            case cont of
                                                Text t -> t ++ "</" ++ n ++ ">\n"
                                                Children c -> "\n" ++ concatMap (prettyPrint (k +2)) c ++ replicate k ' ' ++ "</" ++ n ++ ">\n"

--test for prettyPrint -> testPretty
--testPretty :: IO ()
--testPretty = do
--    let inner = XMLelement "name" "n1" [] (Text "Ivan")
--    let root = XMLelement "person" "p1" [("status", "active")] (Children [inner])
--    putStrLn $ prettyPrint 0 root

--Превръщане на низове в стъпки---------------------------------------------------------------------------------------
indexStep :: Parser Step
indexStep = do
    _ <- sat (== '[')
    numbers <- many1 (sat isDigit1)
    _ <- sat (== ']')
    return $ ByIndex (read numbers)

attributesParser :: Parser Step
attributesParser = do
    _ <- stringParser "[@"
    attrName <- many1 (sat isAlphaNumber)
    _ <- sat (== ']')
    return $ ByAttr attrName

stepParser :: Parser Step
stepParser = choice [indexStep,descendantParser,childEqualsParser,attributesParser,ByName <$> many1 (sat isAlphaNumber)]

slash :: Parser ()
slash = (sat (=='/')) >> pure()

xPathParser :: Parser [Step]
xPathParser = sepBy slash stepParser

-- test for stepParser -> testSteps
--instance Parseable [Step] where
--    parser = xPathParser
--
--testSteps :: IO ()
--testSteps = case run "person/address[0]" :: Either ParseError [Step] of
--    Right steps -> print steps
--    Left err    -> print err

--Модификация---------------------------------------------------------------------------------------------------------
deleteByPath :: [Step] -> XMLelement ->XMLelement
deleteByPath [] el = el
deleteByPath [ByName n] el = case content el of
                                Children children -> el {content = Children (filter (\child -> name child /= n) children)}
                                Text _ -> el
deleteByPath (ByName n : rest) el = case content el of
                                        Children ch -> el {content = Children (map descend ch)}
                                        Text _ -> el
    where descend child
            |name child == n = deleteByPath rest child
            |otherwise = child

--test for deleteByPath -> testDelete
--testDelete :: IO ()
--testDelete = do
--    -- Вход: <root><item>Data</item><keep>This</keep></root>
--    let xml = XMLelement "root" "" [] (Children [
--                XMLelement "item" "" [] (Text "Data"),
--                XMLelement "keep" "" [] (Text "This")])
--    let path = [ ByName "item"]
--    let result = deleteByPath path xml
--    putStrLn "--- Преди изтриване ---"
--    putStrLn $ prettyPrint 0 xml
--    putStrLn "\n--- След изтриване на 'item' ---"
--    putStrLn $ prettyPrint 0 result   

--Функции-----------------------------------------------------------------------------------------------------------
--print -> prettyPrint

--select <id> <key> Извежда стойност на атрибут по даден идентификатор на елемента и ключ на атрибута
selectAttr :: String -> String -> XMLelement -> Either Errors String
selectAttr id key root = case findByID id root of
                            Nothing -> Left (InvalidID id)
                            Just el -> case getAttrValue key el of
                                            Nothing -> Left (MissingText (name el))
                                            Just v -> Right v

--set <id> <key> <value> Присвояване на стойност на атрибут
setAttr :: String -> String -> String -> XMLelement ->XMLelement
setAttr tagID key val el@(XMLelement n i attrs cont)
        |i == tagID = el {attributes = (key,val) : filter ((/=key).fst) attrs}
        |otherwise = el {content = case cont of
                    Text t -> Text t
                    Children ch ->  Children (map (setAttr tagID key val) ch)}  

--children <id> Списък с атрибути на вложените елементи
children :: String -> XMLelement -> Either Errors [[(String,String)]]
children i root = case (findByID i root) of
                    Nothing -> Left (InvalidID i)
                    Just x -> case content x of
                                Text _ -> Left (NoChildren (name x)) 
                                Children c -> if null c then Left (NoChildren (name x)) else Right (map attributes c)

--child <id> <n> Достъп до n-тия наследник на елемент
child :: String -> Int -> XMLelement -> Either Errors XMLelement
child i n root = if n < 0 then Left (MissingChild n) else case (findByID i root) of
                                                            Nothing -> Left (InvalidID i)
                                                            Just x -> case getChild n x of
                                                                Nothing -> Left (MissingChild n)
                                                                Just c -> Right c

--text <id> Достъп до текста на елемент
textFunction :: String -> XMLelement ->Either Errors String
textFunction i root = case findByID i root of
                Nothing -> Left (InvalidID i)
                Just x -> case getText x of
                            Nothing -> Left (MissingText (name x))
                            Just t -> Right t

--delete <id> <key> Изтриване на атрибут на елемент по ключ
deleteAttr :: String -> String -> XMLelement -> XMLelement
deleteAttr tagID key el@(XMLelement n i a c)
    |i==tagID = el {attributes = filter ((/=key).fst) a}
    |otherwise = case c of
                    Text _ -> el
                    Children ch -> el {content = Children (map (deleteAttr tagID key) ch)}

--newchild <id> Добавяне на НОВ наследник на елемент. Новият елемент няма никакви атрибути, освен идентификатор
newChildHelper :: String -> XMLelement -> XMLelement
newChildHelper tagID el@(XMLelement n i a c)
        |i == tagID = case c of
                        Text _ -> el
                        Children ch -> el {content = Children (ch ++ [XMLelement "child" "" [] (Children [])])}
        |otherwise = case c of
                        Text _ -> el
                        Children ch -> el {content =  Children (map (newChildHelper tagID) ch)}

newChild :: String -> XMLelement -> XMLelement
newChild tagId root = fixed
    where (fixed,_,_) = uniqueID (newChildHelper tagId root) [] 0

--xpath <XPath> операции за изпълнение на прости XPath 2.0 заявки към кореновия елемент
instance Parseable [Step] where
    parser = xPathParser

xpath :: String-> XMLelement -> Either ParseError [XMLelement]
xpath str root = do
    steps <- run str
    let result = select steps [root]
    return result

--test for functions
{-
testXML :: XMLelement
testXML = XMLelement "root" "r1" [] (Children
        [ XMLelement "person" "p1" [("id","1")] (Children[ XMLelement "name" "n1" [] (Text "Ivan"), XMLelement "address" "a1" [] (Text "Bulgaria")])
        , XMLelement "person" "p2" [("id","2")] (Children[ XMLelement "name" "n2" [] (Text "Maria")])])

testSet :: IO ()
testSet = do
    let result = setAttr "p1" "status" "active" testXML
    putStrLn "---- setAttr ----"
    putStrLn $ prettyPrint 0 result

testDeleteAttr :: IO ()
testDeleteAttr = do
    let result = deleteAttr "p1" "id" testXML
    putStrLn "---- deleteAttr ----"
    putStrLn $ prettyPrint 0 result

testNewChild :: IO ()
testNewChild = do
    let withChild = newChild "p2" testXML
    let (final,_,_) = uniqueID withChild [] 0
    putStrLn "---- newChild ----"
    putStrLn $ prettyPrint 0 final

testXPath :: IO ()
testXPath = do
    let xml = XMLelement "root" "r1" [] (Children
                  [ XMLelement "person" "p1" [] (Children
                      [ XMLelement "name" "n1" [] (Text "Ivan")
                      , XMLelement "address" "a1" [] (Text "Bulgaria")])
                  , XMLelement "person" "p2" [] (Children
                      [ XMLelement "name" "n2" [] (Text "Maria")])])
    case xpath "person/address[0]" xml of
        Left err -> print err
        Right res -> mapM_ (putStrLn . prettyPrint 2) res
-}
--Команди------------------------------------------------------------------------------------------------------
openXMLelement :: FilePath -> IO (Either ParseError XMLelement)
openXMLelement path = do
                    contents <- readFile path
                    return (run contents)

saveXMLelement :: FilePath -> XMLelement -> IO()
saveXMLelement path el = writeFile path (prettyPrint 0 el)

saveAs :: FilePath -> XMLelement -> IO()
saveAs = saveXMLelement

close :: IO (Maybe XMLelement)
close = return Nothing

exit :: IO()
exit = putStrLn "EXIT..." >> return ()

help :: IO()
help = do
    putStrLn "Valid commands:"
    putStrLn "open/save/saveAs/close/exit"
    putStrLn "print/select/set/children/child/text"
    putStrLn "delete/newChild/xpath"

--------------------------------------------------------------------------------------------------------------- 
--main 
main :: IO ()
main = do
    putStrLn "XMLParser.Type 'help' for commands:"
    loop Nothing

--Основен цикъл за командите
loop currentXML = do
    putStr "xml >"
    line <- getLine
    let (cmd,rest) = break (== ' ') line
    let args = dropWhile (== ' ') rest
    case cmd of
        "xpath" -> case currentXML of
            Just xml -> do
                case xpath args xml of
                    Left err -> print err
                    Right res -> mapM_ (putStrLn . prettyPrint 2) res
                loop currentXML
            Nothing -> putStrLn "No element loaded" >> loop currentXML
        "open" -> do
            res <- openXMLelement args
            case res of
                Left err -> print err >> loop currentXML
                Right xml -> putStrLn "File loaded" >> loop (Just xml)
        "help" -> help >> loop currentXML
        "exit" -> exit 
        _ -> putStrLn "Unknown command" >> loop currentXML

--test
{-
ghci> main
XMLParser.Type 'help' for commands:
xml >open data.xml
File loaded
xml >xpath root
xml >xpath person/address
  <address id="a1" type="home"id="a1">Sofia, Bulgaria</address>

  <address id="a2" type="work"id="a2">Plovdiv, Bulgaria</address>

  <address id="a3" type="home"id="a3">Varna, Bulgaria</address>
-}