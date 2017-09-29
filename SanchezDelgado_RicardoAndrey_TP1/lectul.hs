import System.IO
import Data.Char(toUpper)
import Data.Array
import Data.Maybe 
import Data.List 
import Control.Monad (when)

type Estado = [([Char],[[[Char]]])]
delimiters = ",;:"

--Data types
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [([Char], JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

--Get from 
getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getInt :: Integral a => JValue -> Maybe a
getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _         = Nothing

getObject :: JValue -> Maybe [([Char], JValue)]
getObject (JObject o) = Just o
getObject _           = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray a) = Just a
getArray _          = Nothing

isNull :: JValue -> Bool
isNull v            = v == JNull

--Rendering JValues for printing
renderJValue :: JValue -> String
renderJValue (JString s)   =  show s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue JNull         = "null"
renderJValue (JObject o) = "{" ++ pairs o ++ "} "
  where pairs [] = ""
        pairs ps = intercalate ", " (map renderPair ps)
        renderPair (k,v)   =  k ++ ": " ++ renderJValue v

renderJValue (JArray a) = "[" ++ values a ++ "]"
  where values [] = ""
        values vs = intercalate ", " (map renderJValue vs)

renderList :: [JValue] -> [String]
renderList [] = []
renderList (x:xs) = renderJValue x : renderList xs

--Printing JValue
putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)

fields :: [Char] -> [[Char]]
fields [] = []
fields xs = let (item, rest) = break (`elem` delimiters) xs
                (_,    next) = break (`notElem` delimiters) rest
    in item : fields next

fieldArray :: [[e]] -> Maybe (Array (Int, Int) e)
fieldArray [] = Nothing
fieldArray xs = Just $ listArray ((1,1), (length xs, length $ head xs))
    $ concat xs
fieldsFromFile :: FilePath -> IO (Maybe (Array (Int, Int) [Char]))
fieldsFromFile = fmap (fieldArray . map fields . lines) . readFile
 
convertToList :: [Char] -> [[Char]]
convertToList x = fields(x++"")

rendertJsn :: [[Char]] -> [[Char]] -> [[String]] -> [JValue]
rendertJsn [] _ _ = []
rendertJsn _ _ [] = []
rendertJsn _ [] _ = []
rendertJsn (names) (types) (x:xs) = JObject( rendertJsnAux names types x ) : rendertJsn names types xs

rendertJsnAux :: [[Char]] -> [[Char]] -> [String] -> [([Char], JValue)]
rendertJsnAux [] _ _ = []
rendertJsnAux _ [] _= []
rendertJsnAux (name:namesR) (types:ret) (x:xs) 
  |types=="N"   = (name++"", JNumber(read x :: Double )) : rendertJsnAux namesR ret xs
  |types=="B"   = (name++"", JBool( wBool x ))   : rendertJsnAux namesR ret xs
  |types=="X"   = (name++"", JString( x )) : rendertJsnAux namesR ret xs
 
wBool :: [Char] -> Bool
wBool (x)
  |x=="si" = True
  |x=="Si" = True
  |x=="SI" = True
  |x=="no" = False
  |x=="No" = False
  |x=="NO" = False
  |x=="true" = True
  |x=="True" = True
  |x=="False" = False
  |x=="false" = False
  |otherwise = True

getIndexAux :: (Eq a1, Num a) => a1 -> [a1] -> a
getIndexAux name (x:xs)
  |x==name = 0
  |(x:xs) == [] = -1
  |not(x==name) = 1+getIndexAux name xs 
  
getIndex name x = getIndexAux name x
--conv :: IO()

oJoin :: Eq a => [a] -> Int -> Int -> [[a]] -> [[a]]
oJoin x n1 n2 (y:ys)
  |n1<0 = []
  |n2<0 = []
  |null(x) ==True = []
  |null(y) == True = []
  |null(ys) == True = []
  |isInLista (x!!n1) (y!!n2) == True = [x++y] ++ oJoin x n1 n2 ys
  |isInLista (x!!n1) (y!!n2) == False = oJoin x n1 n2 ys
  |otherwise = []

cJoin :: Eq a => [[a]] -> Int -> Int -> [[a]] -> [[a]]
cJoin [] _ _ _= []
cJoin (x:xs) n1 n2 y = oJoin x n1 n2 y ++ cJoin xs n1 n2 y

isInLista :: Eq a => a -> a -> Bool
isInLista x y
  |y == x = True
  |x/=y = False

join :: FilePath -> FilePath -> [Char] -> FilePath -> IO ()
join nombreC1 nombreC2 beEqu1 nombreJ = do 
       p1 <- openArchive nombreC1
       p2 <- openArchive nombreC2
       res <- joinAux p1 p2 beEqu1
       c <- renderJsn res
       let g =head(c)
       saveArchive nombreJ g
       print ""

joinAux :: Monad m => [[[Char]]] -> [[[Char]]] -> [Char] -> m [[[Char]]]
joinAux p1 p2 beEqu = do
  let names1 = head(p1)
  let n1 = getIndex beEqu names1
--  print n
  let types1 = p1!!1
  let rest1 = tail(tail(p1))
  let names2 = head(p2)
  let n2 = getIndex beEqu names2
  let types2 = p2!!1
  let rest2 = tail(tail(p2))
     --  print rest2
  let res = cJoin rest1 n1 n2 rest2
  let namesT = map ( "1." ++)(p1!!0) ++ map ("2." ++ )(p2!!0)
  let typesT = types1 ++ types2
  let temp = [namesT]++[typesT]++res  --print temp
  return temp

conv :: FilePath -> FilePath -> IO [[[Char]]]
conv nombreC nombreJ =  do 
       p <- openArchive nombreC
       c <- renderJsn p
       let g = head(c)
       saveArchive nombreJ g
       return p

putJsn :: [JValue] -> IO ()
putJsn [] = return()
putJsn (x:xs) = do
  putJValue x
  putJsn(xs)

renderJsn :: [[[Char]]] -> IO [String]
renderJsn list = do
  let names = head(list)
  let types = list!!1
  let rest = tail(tail(list))
  let x = rendertJsn names types rest
  putJsn [JArray(x)]
  let c = renderList([JArray(x)])
  return c

openArchive :: FilePath -> IO [[[Char]]]
openArchive name = do
  inh <- openFile name ReadMode
  p <- readLoop inh []
  hClose inh
  return p

saveArchive :: FilePath -> String -> IO ()
saveArchive name content = do
  outh <- openFile name WriteMode
  hPutStr outh content
  hClose outh

readLoop :: Handle -> [[[Char]]] -> IO [[[Char]]]
readLoop inh lista = 
    do ineof <- hIsEOF inh
       if ineof
           then return lista
           else do inpStr <- hGetLine inh
                   let c = convertToList inpStr
                   readLoop inh (lista++[c])
main :: IO ()
main = do 
       mainloop []

mainloop :: Estado -> IO ()
mainloop estado = do
    putStr ">> "
    inpStr <- getLine
    (terminar,nuevoestado,salida) <- process inpStr estado
    if terminar
       then return ()
       else mainloop nuevoestado

--process :: String -> Estado -> IO (Bool, Estado, String)
process :: String -> Estado -> IO (Bool, Estado, String)
process command estado =
     case tokens!!0 of
          "load"   -> cmd_load (tail tokens) estado
          "save"   -> cmd_save (tail tokens) estado
          "join"   -> cmd_join (tail tokens) estado
          "exit"    -> return (True, estado, "Coming out...")
       where tokens = words command

cmd_load::[String]->Estado-> IO(Bool, Estado, String)
cmd_load (v:r:[]) estado = do
     let c = getContent v estado
     if(null c) then do
      p <- openArchive r
      let var = estado ++ [(v,p)]
      return (False, var, "") 
     else do
      print (v++" already exists")
      return (False, estado, "loaded")


getContentAux::[Char] -> ([Char],[[[Char]]]) -> [[[Char]]]
getContentAux name (x,xs)
  |name == x = xs
  |name /= x = []

getContent::[Char] -> Estado -> [[[Char]]]
getContent name (x:xs) = getContentAux name x ++  getContent name xs
getContent _ [] = []

cmd_save::[String]->Estado-> IO(Bool, Estado, String)
cmd_save (v:r:[]) estado = do
    let p = getContent v estado
    c <- renderJsn p
    let content = head(c)
    saveArchive r content
    return (False, estado, "Created")
  
cmd_join ::[[Char]]-> [([Char], [[[Char]]])]-> IO (Bool, [([Char], [[[Char]]])], [Char])
cmd_join (id1:id2:atribut:id3:[]) estado = do
    let p1 = getContent id1 estado
    let p2 = getContent id2 estado
    res <- joinAux p1 p2 atribut

    let c = getContent id3 estado
    if(null c) then do
       let temp = estado ++ [(id3,res)]
       c <- renderJsn res
       return (False, temp, "Se ha realizado el join")
     else do
      print (id3++" already exists")
      return (False, estado, "loaded")
    
cmd_desconocido :: String -> String -> Estado -> (Bool, Estado, String)
cmd_desconocido cmd command estado = (False, estado, mensaje)
       where mensaje = "command desconocido ("++ cmd ++"): '" ++ command ++ "'"

cmd_fin :: Estado -> (Bool, Estado, String)
cmd_fin estado = (False, estado, show estado)

     