module CommandLine
 (askQuestion, askStr, askOption)
 where 

askQuestion :: (Read a) => String -> IO a
askQuestion = askQuestion' read


askStr :: String -> IO String
askStr = askQuestion' id

askQuestion' :: (String -> a) -> String -> IO a
askQuestion' parse q = do
    putStrLn q
    parse <$> getLine 

askOption :: String -> String -> String -> IO a -> IO a -> IO a
askOption q a1 a2 ac1 ac2 = do
    ans <- askQuestion' id q
    if a1 == ans
        then ac1
        else if a2 == ans
            then ac2
            else error $ "Not one of the options for question " ++ q