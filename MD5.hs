import Data.Bits
import Data.Char
import Data.List
import Data.Word
import System.Environment

type Digest = (Word32, Word32, Word32, Word32)

-- given a 64-bit integer, returns a character list consisting of the bytes of such integer, in little-endian order
intToBytes :: Int -> String
intToBytes n = [chr $ (n `shiftR` i) `mod` 256 | i <- [0,8..63]]

-- given a list of chars, it pads it with 0 such that its length is divisible by 64
padded :: String -> String
padded xs = xs ++ '\x80':take padding nullbytes ++ intToBytes (lxs*8)
        where
                lxs = length xs
                padding = (56 - 1 - lxs) `mod` 64
                nullbytes = repeat '\0'

-- splits a list into a list of N-sized chunks 
splittedByN :: Int -> [a] -> [[a]]
splittedByN n x = h : if null t then [] else splittedByN n t
        where (h,t) = splitAt n x

-- given a list of chars, turn them into a list of 32-bit words (in little-endian order) 
toWord32Array :: String -> [Word32]
toWord32Array xs = case xs of
        []     -> []
        (_:_)  -> foldr (\c i -> fromIntegral (ord c) + i * 256) 0 (take 4 xs) : toWord32Array (drop 4 xs)


-- Given a list of chars, it pads it to a multiple of 64, turns it into a list of 32-bit words and splits such words into sublists of size 16
stringToWordArray :: String -> [[Word32]]
stringToWordArray = splittedByN 16 . toWord32Array . padded

-- The initial value of an MD5 digest
initValue :: Digest
initValue = (0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476)

-- digest a single 512 bit chunk of a message
chunkDigest :: Digest -> [Word32] -> Digest
chunkDigest dig0 message = foldl (\dig@(a,b,c,d) i -> (d, b + (f dig i `rotateL` s i), b, c)) dig0 [0..63]
        where
                s :: Word32 -> Int
                s i = case i of {
                        0  ->  7; 1  -> 12; 2  -> 17; 3  -> 22;
                        4  ->  7; 5  -> 12; 6  -> 17; 7  -> 22;
                        8  ->  7; 9  -> 12; 10 -> 17; 11 -> 22;
                        12 ->  7; 13 -> 12; 14 -> 17; 15 -> 22;
                        16 ->  5; 17 ->  9; 18 -> 14; 19 -> 20;
                        20 ->  5; 21 ->  9; 22 -> 14; 23 -> 20;
                        24 ->  5; 25 ->  9; 26 -> 14; 27 -> 20;
                        28 ->  5; 29 ->  9; 30 -> 14; 31 -> 20;
                        32  -> 4; 33 -> 11; 34 -> 16; 35 -> 23;
                        36 ->  4; 37 -> 11; 38 -> 16; 39 -> 23;
                        40 ->  4; 41 -> 11; 42 -> 16; 43 -> 23;
                        44 ->  4; 45 -> 11; 46 -> 16; 47 -> 23;
                        48 ->  6; 49 -> 10; 50 -> 15; 51 -> 21;
                        52 ->  6; 53 -> 10; 54 -> 15; 55 -> 21;
                        56 ->  6; 57 -> 10; 58 -> 15; 59 -> 21;
                        60 ->  6; 61 -> 10; 62 -> 15; 63 -> 21;
                }
                k :: Word32 -> Word32
                k i = case i of {
                        0  -> 0xd76aa478; 1  -> 0xe8c7b756; 2  -> 0x242070db; 3  -> 0xc1bdceee;
                        4  -> 0xf57c0faf; 5  -> 0x4787c62a; 6  -> 0xa8304613; 7  -> 0xfd469501;
                        8  -> 0x698098d8; 9  -> 0x8b44f7af; 10 -> 0xffff5bb1; 11 -> 0x895cd7be;
                        12 -> 0x6b901122; 13 -> 0xfd987193; 14 -> 0xa679438e; 15 -> 0x49b40821;
                        16 -> 0xf61e2562; 17 -> 0xc040b340; 18 -> 0x265e5a51; 19 -> 0xe9b6c7aa;
                        20 -> 0xd62f105d; 21 -> 0x02441453; 22 -> 0xd8a1e681; 23 -> 0xe7d3fbc8;
                        24 -> 0x21e1cde6; 25 -> 0xc33707d6; 26 -> 0xf4d50d87; 27 -> 0x455a14ed;
                        28 -> 0xa9e3e905; 29 -> 0xfcefa3f8; 30 -> 0x676f02d9; 31 -> 0x8d2a4c8a;
                        32 -> 0xfffa3942; 33 -> 0x8771f681; 34 -> 0x6d9d6122; 35 -> 0xfde5380c;
                        36 -> 0xa4beea44; 37 -> 0x4bdecfa9; 38 -> 0xf6bb4b60; 39 -> 0xbebfbc70;
                        40 -> 0x289b7ec6; 41 -> 0xeaa127fa; 42 -> 0xd4ef3085; 43 -> 0x04881d05;
                        44 -> 0xd9d4d039; 45 -> 0xe6db99e5; 46 -> 0x1fa27cf8; 47 -> 0xc4ac5665;
                        48 -> 0xf4292244; 49 -> 0x432aff97; 50 -> 0xab9423a7; 51 -> 0xfc93a039;
                        52 -> 0x655b59c3; 53 -> 0x8f0ccc92; 54 -> 0xffeff47d; 55 -> 0x85845dd1;
                        56 -> 0x6fa87e4f; 57 -> 0xfe2ce6e0; 58 -> 0xa3014314; 59 -> 0x4e0811a1;
                        60 -> 0xf7537e82; 61 -> 0xbd3af235; 62 -> 0x2ad7d2bb; 63 -> 0xeb86d391;
                }
                g :: Word32 -> Int
                g i = fromIntegral $ case i `div` 16 of
                        0 -> i
                        1 -> (i*5+1) `mod` 16
                        2 -> (i*3+5) `mod` 16
                        3 -> (i*7)   `mod` 16

                f0 :: Word32 -> Word32 -> Word32 -> Word32 -> Word32
                f0 b c d i = case i `div` 16 of
                        0 -> (b .&. c) .|. (complement b .&. d)
                        1 -> (d .&. b) .|. (complement d .&. c)
                        2 -> b `xor` c `xor` d
                        3 -> c `xor` (b .|. complement d)

                f :: Digest -> Word32 -> Word32
                f (a,b,c,d) i = f0 b c d i + a + k i + message !! g i


-- add two 128-bit digest results
digestAdd :: Digest -> Digest -> Digest
digestAdd (a,b,c,d) (e,f,g,h) = (a+e,b+f,c+g,d+h)

-- compute the MD5 digest of a message
md5Digest :: String -> Digest
md5Digest s = foldl (\dig msg -> digestAdd dig (chunkDigest dig msg) ) initValue (stringToWordArray s)

-- turn a byte into a hexadecimal string
byteToHex :: Word32 -> String
byteToHex i = intToDigit (fromIntegral i `div` 16) : intToDigit (fromIntegral i `mod` 16) : ""

-- turn a 32-bit word into a hexadecimal string
hex :: Word32 -> String
hex i = foldr (\n ss -> byteToHex ((i `div` (256^n)) `mod` 256) ++ ss) [] [0..3]

-- print a digest result in hex
showDigest :: Digest -> String
showDigest (a,b,c,d) = concatMap hex [a,b,c,d]

-- compute a string digest and return it as hexadecimal
md5 :: String -> String
md5 = showDigest . md5Digest

main :: IO ()
main = getArgs >>= (
        \argv -> case argv of 
                (path:_) -> md5 <$> readFile path
                []       -> md5 <$> getLine
        >>= putStrLn
        )
