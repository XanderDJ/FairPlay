module Main where

import CommandLine
import Data.Char

main :: IO ()
main = askOption "Do you want to encode (e) or decode (d)?" "e" "d" mainEncode mainDecode


mainEncode :: IO ()
mainEncode = do 
    key <- askStr "What's the encryption key"
    txt <- askStr "What do you want to encrypt (non letters will be dropped)"
    let fpm = keyToFPM key
        encodedText = encodeTxt fpm txt
    putStrLn "This is the encoded text"
    putStrLn encodedText


mainDecode :: IO ()
mainDecode = do 
    key <- askStr "What's the encryption key"
    txt <- askStr "What do you want to decrypt"
    let fpm = keyToFPM key
        decodedText = decodeTxt fpm txt
    putStrLn "This is the decoded text"
    putStrLn decodedText


data FPMatrix = MChar Char FPMatrix | FPMI FPMatrix | Empty deriving Show

data Relation = Row | Col | Corners deriving Show


emptyFPM :: FPMatrix
emptyFPM = Empty


alphabet = ['a' .. 'z']


encodeTxt :: FPMatrix -> String -> String
encodeTxt fpm  = (concatMap (\(c1,c2) -> c1:c2:[]) . map (encodePair fpm) . txtToPairs . filter isLower . map toLower )


decodeTxt fpm = (concatMap (\(c1,c2) -> c1:c2:[]) . map (decodePair fpm) . txtToPairs . filter isLower . map toLower )

encodePair :: FPMatrix -> (Char, Char) -> (Char, Char)
encodePair fpm pair = getPair encodeRelatedPositions fpm pair 

decodePair :: FPMatrix -> (Char, Char) -> (Char, Char)
decodePair fpm pair = getPair decodeRelatedPositions fpm pair 


getPair :: (Relation -> (Int,Int) -> (Int,Int) -> ((Int,Int), (Int, Int))) -> FPMatrix -> (Char, Char) -> (Char,Char)
getPair method fpm (c1,c2) = (c1',c2')
 where p1 = getPosition fpm c1
       p2 = getPosition fpm c2
       rel = relation p1 p2
       (p1', p2') = method rel p1 p2
       c1' = getCharFPM fpm p1'
       c2' = getCharFPM fpm p2'


relation :: (Int, Int) -> (Int, Int) -> Relation
relation (x1, y1) (x2 , y2) 
 | x1 == x2 = Row
 | y1 == y2 = Col
 | otherwise = Corners


encodeRelatedPositions :: Relation -> (Int, Int) -> (Int, Int) -> ((Int,Int), (Int, Int))
encodeRelatedPositions Row (x1,y1) (x2,y2) = ((wrap (x1 + 1) , y1), (wrap (x2 +1) , y2))
encodeRelatedPositions Col (x1,y1) (x2,y2) = ((x1, wrap (y1 + 1)), (x2, wrap (y2 +1)))
encodeRelatedPositions Corners (x1, y1) (x2, y2) = ((x1, y2), (x2, y1))


decodeRelatedPositions :: Relation -> (Int, Int) -> (Int, Int) -> ((Int,Int), (Int, Int))
decodeRelatedPositions Row (x1,y1) (x2,y2) = ((wrap (x1 - 1) , y1), (wrap (x2 -1) , y2))
decodeRelatedPositions Col (x1,y1) (x2,y2) = ((x1, wrap (y1 - 1)), (x2, wrap (y2 -1)))
decodeRelatedPositions Corners (x1, y1) (x2, y2) = ((x1, y2), (x2, y1))


wrap :: Int -> Int
wrap n = mod n 5


txtToPairs :: String -> [(Char, Char)]
txtToPairs [c] = (c, 'x') : []
txtToPairs (c1 : c2 : []) = (c1, c2) : []
txtToPairs (c1 : c2 : cs) = if c1 == c2 then (c1 , 'x') : txtToPairs (c2 : cs) else (c1,c2) : txtToPairs cs


keyToFPM :: String -> FPMatrix
keyToFPM key = fmatrix
 where partialMatrix = foldl insert emptyFPM $ (filter isLower . map toLower) key
       fmatrix = foldl insert partialMatrix alphabet


getPosition :: FPMatrix -> Char -> (Int, Int)
getPosition m c = (row, col)
 where n = count m c
       col = n `mod` 5
       row = n `div` 5


getCharFPM :: FPMatrix -> (Int, Int) -> Char
getCharFPM fpm (x1,y1) = getChar' fpm (x1 * 5 + y1)
 where
     getChar' :: FPMatrix -> Int -> Char
     getChar' (MChar ch fpm) n = if n == 0 then ch else getChar' fpm (n-1)
     getChar' (FPMI fpm) n = if n == 0 then 'i' else getChar' fpm (n-1)
     getChar' (Empty) n = error "Position out of bounds while getting character from fpm"





count :: FPMatrix -> Char -> Int
count m c = go m c 0
 where go (MChar ch fpm) c n = if c == ch then n else go fpm c (n+1)
       go (FPMI fpm) c n = if c == 'i' || c  == 'j' then n else go fpm c (n+1)
       go (Empty) c n = n 



insert :: FPMatrix -> Char -> FPMatrix
insert (MChar ch m) c = if c == ch then MChar ch m else MChar ch (insert m c)
insert (FPMI m) c = if c == 'i' || c == 'j' then FPMI m else FPMI (insert m c)
insert (Empty) c =  if c == 'i' || c == 'j' then FPMI Empty else MChar c Empty