import Network
import System.IO
import Text.Printf 
import Text.Regex.Posix
import Data.List
import Data.Char
import IRC
import IRC.IO
import IRC.Parser

server  = "irc.ecnet.org"
port    = 6669
channel = "#testing"
nick    = "snarkles"
host    = "localhost"

numericMsgHandler :: Handle -> Int -> [String] -> IO()
numericMsgHandler h i body | i ==  376 = write h "JOIN" [channel]
                           | otherwise = return ()

msgHandler :: Handle -> ServerMessage -> IO()
msgHandler h (NOTICE)                         = return ()
msgHandler h (CMD prefix cmd params postfix)  | cmd == "PING"    = write h "PONG" [postfix]
                                             -- | cmd == "PRIVMSG" = privmsg h channel "Shut up"
msgHandler h (NUMERIC x body)                 = numericMsgHandler h x body
msgHandler h u = print u
 
main = do
    h <- joinServer server port nick 
    listen h (\ s -> msgHandler h $ parseMessage' s)
    hClose h
