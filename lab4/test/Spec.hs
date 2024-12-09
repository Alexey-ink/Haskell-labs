module Main (main) where
import Test.QuickCheck 
import Lib

prop_mod :: Int -> Int -> Int -> Property
prop_mod x y m = (m /= 0) ==> (addMod x y m) `mod` m === (x + y) `mod` m    

prop_neutral :: Int -> Int -> Property
prop_neutral x m = (m /= 0) ==> addMod x 0 m === x `mod` m

prop_commutativity :: Int -> Int -> Int -> Property
prop_commutativity x y m = (m /= 0) ==> addMod x y m === addMod y x m


prop_emptyTree :: Bool
prop_emptyTree = treeDepth (Empty :: Tree Int) == 0

prop_singleNodeTree :: Bool
prop_singleNodeTree = treeDepth (Node 1 Empty Empty :: Tree Int) == 1

prop_twoBranchTree :: Tree Int -> Tree Int -> Bool
prop_twoBranchTree leftBranch rightBranch =
    let tree = Node 1 leftBranch rightBranch
    in treeDepth tree == 1 + max (treeDepth leftBranch) (treeDepth rightBranch)

prop_addNodeInTree :: Tree Int -> Int -> Bool
prop_addNodeInTree tree x =
    let newTree = Node x tree Empty
    in treeDepth newTree >= treeDepth tree


main :: IO()
main = do 
    putStrLn "Сложение по модулю: "
    quickCheck prop_mod
    putStrLn "Нейтральный элемент: "
    quickCheck prop_neutral
    putStrLn "Коммутативность: "
    quickCheck prop_commutativity
    putStrLn "\nГлубина пустого дерева равна 0: "
    quickCheck prop_emptyTree
    putStrLn "Глубина дерева с одним узлом равна 1: "
    quickCheck prop_singleNodeTree 
    putStrLn "Глубина дерева с двумя ветвями равна максимальной глубине среди ветвей плюс 1: "
    quickCheckWith stdArgs { maxSuccess = 100, maxSize = 10 } prop_twoBranchTree
    putStrLn "Добавление узла в дерево не уменьшает его глубину: "
    quickCheckWith stdArgs { maxSuccess = 100, maxSize = 10 } prop_addNodeInTree
