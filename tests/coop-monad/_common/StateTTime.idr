import CommonTestingStuff
import Program

export
main : HasIO io => io ()
main = do
  putStrLn beforeString
  traverse_ putStrLn $ execW program
