module IRC.IO where

import Control.Monad
import Data.List
import Network
import System.IO
import Text.Printf

joinServer host port nick = do
    h  <- connectTo host (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    negotiateNick h nick
    return h -- Actually just wrapping Handle back into an IO Handle

listen h f = forever $ do
                 t <- hGetLine h
                 print t
                 f t

write :: Handle -> String -> [String] -> IO()
write h cmd body = do
    hPrintf h "%s %s\r\n"   cmd $ intercalate " " body
    printf    "> %s %s\r\n" cmd $ intercalate " " body

negotiateNick h nick = do
    write h "NICK" [nick]
    write h "USER" [nick, "0", "*", "Jimmie"]
