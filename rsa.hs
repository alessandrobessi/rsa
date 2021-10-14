
module Main (main) where
import Data.Bits
import Control.Monad.Random

modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
    where t = if testBit e 0 then b `mod` m else 1

data PublicKey = PublicKey {publicKey :: Integer, e :: Integer} deriving Show
data PrivateKey = PrivateKey {privateKey :: Integer, d :: Integer} deriving Show

encrypt :: PublicKey -> Integer -> Integer
encrypt (PublicKey n e) x = modExp x e n

decrypt :: PrivateKey -> Integer -> Integer
decrypt (PrivateKey n d) x = modExp x d n

isCoprime :: Integer -> Integer -> Bool
isCoprime a b = a < b && gcd a b == 1

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) =  x : sieve [y | y <- xs, y `mod` x /= 0]

primes :: Integer -> Integer -> [Integer]
primes a b = dropWhile (< a) $ takeWhile (< b) (sieve [2..])

keyLength = 30
nMin :: Integer
nMin = Data.Bits.shiftL 1 (keyLength - 1)
nMax :: Integer
nMax = Data.Bits.shiftL 1 keyLength - 1

start :: Integer
start = Data.Bits.shiftL 1 (keyLength `div` 2 - 1)
stop :: Integer
stop = Data.Bits.shiftL 1 (keyLength `div` 2 + 1)

ps = primes start stop

takeRandomElement :: (MonadRandom m) => Int -> [Integer] -> m Integer
takeRandomElement n lst = do
    i <- getRandomR (0, n-1)
    return $ lst !! i

newtype NoQuotes = NoQuotes String
instance Show NoQuotes where show (NoQuotes str) = str

main :: IO ()
main = do
    p <- takeRandomElement (length ps) ps
    let qs = [q | q <- ps, q /= p && nMin <= p * q && p * q <= nMax]
    q <- takeRandomElement (length qs) qs
    let end = (p - 1) * (q - 1)
    let e = head [x | x <- [3, 5..end], isCoprime x end]
    let d = head [x | x <- [3, 5..end], x * e `mod` end == 1]
    let public = PublicKey {publicKey = p * q, e = e}
    let private =  PrivateKey {privateKey = p * q, d = d}
    
    print public
    print private

    let message = primes 900 930
    let encrypted = map (encrypt public) message
    let decrypted = map (decrypt private) encrypted
    
    print $ NoQuotes("original msg: " ++ show message)
    print $ NoQuotes("encrypted msg: " ++ show encrypted)
    print $ NoQuotes("decrypted msg: " ++ show decrypted)
    print $ NoQuotes("original msg == decrypted msg is " ++ show (message == decrypted))

