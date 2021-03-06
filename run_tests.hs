
import Playground
import Vector
import qualified Tree as T


--test :: (Num b) => a -> b
--test :: (Show a) => a -> String
test x
    | sum [1,2,3]      /= 6     = error "test failed"
    | min' 9 10        /= 9     = error "min failed"
    | min' 10 10       /= 10    = error "min failed"
    | min' 10 9        /= 9     = error "min failed"
    | min' "asd" "bsd" /= "asd" = error "min failed"
    | max' 9 10        /= 10    = error "max failed"
    | max' 10 10       /= 10    = error "max failed"
    | max' 10 9        /= 10    = error "max failed"
    | max' "asd" "bsd" /= "bsd" = error "max failed"
    | head' [5,4,3,2,1] /= 5    = error "head failed"
    | tail' [5,4,3,2,1] /= [4,3,2,1] = error "tail failed"
    | tail' [5]         /= []        = error "tail failed"
    | last' [5,4,3,2,1] /= 1         = error "tail failed"
    | init' [5,4,3,2,1] /= [5,4,3,2] = error "tail failed"
    | init' [1]         /= []        = error "tail failed"
    | length' [5,4,3,2,1]  /= 5 = error "length failed"
    | length' []           /= 0 = error "length failed"
    | null' [1,2,3]        /= False = error "null on non-empty list"
    | null' []             /= True  = error "null on empty list"
    | reverse' [5,4,3,2,1] /= [1,2,3,4,5] = error "reverse"
    | (null $ reverse' []) /= True        = error "reverse"
    | take' 3 [5,4,3,2,1]          /= [5,4,3] = error "take" 
    | take' 1 [3,9,3]              /= [3]     = error "take" 
    | take' 5 [1,2]                /= [1,2]   = error "take" 
    | (null $ take' 0 [6,6,6])     /= True    = error "take" 
    | (null $ take' (-3) [6,6,6])  /= True    = error "take" 
    | take'' 3 [5,4,3,2,1]         /= [5,4,3] = error "take" 
    | take'' 1 [3,9,3]             /= [3]     = error "take" 
    | take'' 5 [1,2]               /= [1,2]   = error "take" 
    | (null $ take'' 0 [6,6,6])    /= True    = error "take" 
    | (null $ take'' (-3) [6,6,6]) /= True    = error "take" 
    | drop' 3 [8,4,2,1,5,6]         /= [1,5,6] = error "drop"
    | drop' 0 [1,5,6]               /= [1,5,6] = error "drop"
    | (null $ drop' 100 [1,2,1,5])  /= True    = error "drop"
    | (null $ drop' 1 [])           /= True    = error "drop"
    | (null $ drop' 0 [])           /= True    = error "drop"
    | maximum' [1,2,6,3,1]   /= 6   = error "maximum"
    | maximum' [1,6,6,3,1]   /= 6   = error "maximum"
    | maximum' [1]           /= 1   = error "maximum"
    | maximum' "abcdefgabcd" /= 'g' = error "maximum"
    | minimum' [1,2,6,3,1]   /= 1   = error "maximum"
    | minimum' [1,6,6,3,0]   /= 0   = error "maximum"
    | minimum' [1]           /= 1   = error "maximum"
    | minimum' "abcdefgabcd" /= 'a' = error "maximum"
    | maximum'' [1,2,6,3,1]   /= 6   = error "maximum"
    | maximum'' [1,6,6,3,1]   /= 6   = error "maximum"
    | maximum'' [1]           /= 1   = error "maximum"
    | maximum'' "abcdefgabcd" /= 'g' = error "maximum"
    | minimum'' [1,2,6,3,1]   /= 1   = error "maximum"
    | minimum'' [1,6,6,3,0]   /= 0   = error "maximum"
    | minimum'' [1]           /= 1   = error "maximum"
    | minimum'' "abcdefgabcd" /= 'a' = error "maximum"
    | sum' [5,2,1,6,3,2,5,7] /= 31 = error "sum"
    | sum' [5]               /= 5  = error "sum"
    | sum' []                /= 0  = error "sum"
    | product' [1,2,5,6,7,9,2,0] /= 0  = error "product"
    | product' [6,2,1,2]         /= 24 = error "product"
    | product' []                /= 1  = error "product"
    | elem' 1 []         /= False = error "elem"
    | elem' 4 [3,4,5,6]  /= True  = error "elem"
    | elem' 10 [3,4,5,6] /= False = error "elem"
    | take 10 (cycle' [1,2,3]) /= [1,2,3,1,2,3,1,2,3,1] = error "cycle"
    | take 12 (cycle' "LOL ")  /= "LOL LOL LOL "        = error "cycle"
    | take 10 (repeat' 5) /= [5,5,5,5,5,5,5,5,5,5] = error "repeat"
    | replicate' 3 10             /= [10,10,10] = error "replicate"
    | (null $ replicate' 0 10)    /= True       = error "replicate"
    | (null $ replicate' (-2) 10) /= True       = error "replicate"
    | fst' (8,11) /= 8 = error "fst"
    | snd' (8,11) /= 11 = error "snd"
    | zip' [1 .. 5] ["one", "two", "three", "four", "five"] 
        /= [(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five")] 
            = error "zip"
    | (null $ zip' [] [1,2,3,4]) /= True          = error "zip"
    | (null $ zip' [1,2,3,4] []) /= True          = error "zip"
    | zip' [1,2,3,4] [1,2]       /= [(1,1),(2,2)] = error "zip"
    | zip' [2,3] [1,2,3,4]       /= [(2,1),(3,2)] = error "zip"
    | factorial 0  /= 1    = error "factorial"
    | factorial' 0 /= 1    = error "factorial"
    | factorial 5  /= 120  = error "factorial"
    | factorial' 5 /= 120  = error "factorial"
    | compare' 2 1   /= GT = error "compare"
    | 1 `compare'` 2 /= LT = error "compare"
    | initials "Kentril" "Despair" /= "K. D." = error "initials"
    | fib 8 /= 21 = error "fib"
    | fibFast 8 /= 21                      = error "fibfast"
    | fibFast 100 /= 354224848179261915075 = error "fibfast"
    | zipWith' (+) [4,2,5,6] [2,6,2,3]   /= [6,8,7,9] = error "zipWith"
    | zipWith' (max) [4,2,5,6] [2,6,2,3] /= [4,6,5,6] = error "zipWith"
    | flip' zip [1,2,3,4,5] "hello" /= [('h',1),('e',2),('l',3),('l',4),('o',5)] = error "flip"
    | map' (+3) [1,5,3,1,6]  /= [4,8,6,4,9] = error "map"
    | (null $ map' (+3) [])  /= True        = error "map"
    | map'' (+3) [1,5,3,1,6] /= [4,8,6,4,9] = error "map"
    | (null $ map'' (+3) []) /= True        = error "map"
    | (null $ filter' (>3) []) /= True     = error "filter"
    | filter' even [1..10] /= [2,4,6,8,10] = error "filter"
    | (null $ takeWhile' (<=3) []) /= True = error "takeWhile"
    | takeWhile' (<=3) [1,2,3,4,5,6] /= [1,2,3] = error "takeWhile"
    | foldl' (-) 0 [2,3,4] /= (-9) = error "foldl"
    | foldl' (-) 0 [] /= 0         = error "foldl"
    | foldr' (-) 0 [2,3,4] /= 3    = error "foldl"
    | foldr' (-) 0 [] /= 0         = error "foldl"
    | foldl1' (-) [2,3,4] /= (-5)  = error "foldl"
    | scanl' (+) 0 [] /= [0]               = error "scanl"
    | scanl' (-) 0 [1,2,3] /= [0,-1,-3,-6] = error "scanl"
    | splitBy ',' "1,2,3,4" /= ["1","2","3","4"] = error "splitby"
    | (null $ takeFrom 2 []) /= True = error "takeFrom"
    | takeFrom (-1) ["hee", "loo", "la"] /= ["hee", "loo", "la"] = error "takeFrom"
    | takeFrom (0) ["hee", "loo", "la"] /= ["hee", "loo", "la"] = error "takeFrom"
    | takeFrom (1) ["hee", "loo", "la"] /= ["loo", "la"] = error "takeFrom"
    | takeFrom (2) ["hee", "loo", "la"] /= ["la"] = error "takeFrom"
    | (null $ takeFrom (3) []) /= True = error "takeFrom"
    | otherwise = "All ok"

testVector x
    | Vector 3 5 `vecAdd` Vector 9 2 /= Vector 12 7  = error "vecAdd"
    | Vector 3 9 `vecMul` 10         /= Vector 30 90 = error "vecMul"
    | Vector 4 9 `vecDot` Vector 9.0 2.0 
                                     /= 54.0         = error "vecDot"
    | Vector 2 9 `vecMul` (Vector 4 9 `vecDot` Vector 9 2)
                                     /= Vector 108 486 = error "vecMul or vecDot"
    | Vector3 3 5 8 `vec3Add` Vector3 9 2 8 
                                     /= Vector3 12 7 16 = error "vecAdd"
    | Vector3 3 5 8 `vec3Add` Vector3 9 2 8 `vec3Add` Vector3 0 2 3 
                                     /= Vector3 12 9 19 = error "vecAdd"
    | Vector3 3 9 7 `vec3Mul` 10     /= Vector3 30 90 70 = error "vecMul"
    | Vector3 4 9 5 `vec3Dot` Vector3 9.0 2.0 4.0 
                                     /= 74.0 = error "vecDot"
    | Vector3 2 9 3 `vec3Mul` (Vector3 4 9 5 `vec3Dot` Vector3 9 2 4)
                                     /= Vector3 148 666 222 = error "vecMul or vecDot"
    | otherwise = "Vector: All ok"

testTree x
    | T.singleton 5 /= (T.Node 5 T.EmptyTree T.EmptyTree) 
        = error "singleton"
    | T.fromList [5,3,1,2,4] /= 
        T.Node 4 (T.Node 2 (T.singleton 1) (T.singleton 3)) (T.singleton 5)
        = error "fromList"
    | T.inorder tree1 /= [1,2,3,4,5] = error "inorder"
    | T.preorder tree1 /= [4,2,1,3,5] = error "preorder"
    | T.postorder tree1 /= [1,3,2,5,4] = error "postorder"
    | otherwise = "Tree: All ok"
    where tree1 = T.fromList[5,3,1,2,4]



main = do
    putStrLn "Running tests ..."
    print $ test 1
    print $ testVector 1
    print $ testTree 1


