import System.Environment
import System.Directory
import System.IO
import Data.List
import System.Random
import Data.Char
import Data.Bits
import Data.Time

randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (0,1)
  rs <- randomList (n-1)
  return (r:rs) 


binarioDeci :: [Int] -> Int -> Int
binarioDeci [] c = 0
binarioDeci xs c = (last xs)*(2^c) + binarioDeci (init xs) (c+1) 


codificate :: [Int] -> Int -> [Int]
codificate xsveci r 
               | testBit r (binarioDeci xsveci 0) = [1]
               | otherwise = [0] 



veciList :: [Int] -> Int -> Int -> Int -> [Int]
veciList xs ind r 1 = codificate [xs!!(ind-1),xs!!ind,head xs] r
veciList xs ind r n  = codificate [xs!!(ind-1),xs!!ind,xs!!(ind+1)] r ++ veciList xs (ind+1) r (n-1)





newList :: [Int] -> Int -> Int -> Int -> [Int]
newList xs ind r n
                  | ind == 0 = codificate[last xs,xs!!ind,xs!!(ind+1)] r ++ newList xs 1 r (n-1)
                  | otherwise = veciList xs 1 r n
        

simulation :: [Int] -> Int -> Int -> Int -> Int -> IO UTCTime
simulation xs ind r n 0 = getCurrentTime
simulation xs ind r n t = do 
            let xs2 = newList xs ind r n 
            print $ map(\y -> if y==1 then '*' else ' ') xs2
            --print $ xs2
            simulation xs2 ind r n (t-1)

main = do
    (t:n:r:xs) <- getArgs 
    let nTransition = read t :: Int
        large = read n :: Int   
        regla = read r :: Int
   
    --let regla = read r :: Int
    --pirnt $ regla
    lista <- randomList large
    --print lista
    print $ map(\y -> if y==1 then '*' else ' ') lista
    start <- getCurrentTime 
    stop <- simulation lista 0 regla large nTransition
    --stop <- getCurrentTime 
    print $ diffUTCTime stop start


   --print $ a
    --print $ large
    --num <- randomRIO(0,large)  :: IO Int
    --print $  num
   --prefixfunc f xs = [foldl1 f (take n xs) | n<-[1.. length xs]]
    --print $ num
    --let a = [ rand<-randomRIO(0,large)  :: IO Int | p<-[1..large]]
    --print $ a 
    --print $ a 
    --foldl(\acc y -> [randomRIO(0,1) :: Int] : acc) [] 
    

    --prefixfunc f (x:xs) = reverse( foldl(\acc y -> f y (head acc) : acc) [x] xs 
    --print $ num 
    
  
