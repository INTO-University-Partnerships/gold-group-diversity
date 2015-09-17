--import Types
--import Lib
--import Util
import TestLib

--import Data.List (sortBy)
--import Data.Maybe (fromJust)
--import Test.QuickCheck (generate, arbitrary, Gen)

main :: IO Bool
main = do
    b <- sequence
        [ testLib
        ]
    return $ and b

{-
main :: IO Bool
main = do
    wrappedGroups <- generate (arbitrary :: Gen HomoGrpLstWrapper)
    let unwrappedGroups = unwrapHmGrpList wrappedGroups
    putStrLn $ "Current (old) diversity score: " ++ show (objFnAll unwrappedGroups)
    putStrLn $ "Number of groups:              " ++ show (length unwrappedGroups)
    putStrLn $ "Number of students:            " ++ show (length $ concatMap (\(Group _ xs) -> xs) unwrappedGroups)
    let (Group _ students) = head unwrappedGroups
    let student = head students
    --
    let ug   = fromJust $ getUserGroup student unwrappedGroups
    let xs   = fromJust $  mapM (getObjFnDelta unwrappedGroups student) $ concatMap (\(Group _ ys) -> ys) $ getGroupsExcept ug unwrappedGroups
    let xs'  = filter (\(d, _, _) -> d > 0) xs
    let xs'' = sortBy (\(d1, _, _) (d2, _, _) -> compare d2 d1) xs'
    --
    case safeHead xs'' of
        Just (score, u1, u2) -> do
            putStrLn $ "New diversity score:           " ++ show score
            putStrLn $ "Would be achieved by swapping element " ++ show u1 ++ " with " ++ show u2
        Nothing -> do
            putStrLn $ "There is no swap that can increase the diversity score"
    --
    return True
-}
