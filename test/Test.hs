import TestLib

main :: IO Bool
main = do
    b <- sequence
        [ testLib
        ]
    return $ and b
