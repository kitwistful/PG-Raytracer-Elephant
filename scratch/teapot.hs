

main = do
  putStrLn "I'm a little teapot."
  response <- getLine
  putStrLn ("'" ++ response ++ "' is your mom")
  