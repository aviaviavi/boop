import Prelude     (IO, putStrLn)
import Application (appMain)

main :: IO ()
main = do
    putStrLn "running on localhost:3000"
    appMain
