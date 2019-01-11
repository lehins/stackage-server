import Prelude
import Stackage.Database.Cron
import System.IO
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    stackageServerCron (args == ["--force-update"])
