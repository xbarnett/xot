module Main where
import qualified Control.Monad.IO.Class as MO
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Discord as D
import qualified Discord.Requests as D
import qualified Discord.Types as D
import qualified System.IO as I

on_start :: D.DiscordHandler ()
on_start = MO.liftIO (putStrLn "xot has started")

on_end :: IO ()
on_end = putStrLn "xot has ended"

is_bad_message :: String -> Bool
is_bad_message = all C.isSpace

send_message :: D.ChannelId -> String -> D.DiscordHandler ()
send_message channel content = if is_bad_message content then return () else do
    result <- D.restCall (D.CreateMessage channel (T.pack content))
    case result of
      Left e -> MO.liftIO (putStrLn ("message send error: " ++ show e))
      Right _ -> return ()

process_command :: D.ChannelId -> String -> D.DiscordHandler ()
process_command channel command = case func_name of
  "echo" -> send_message channel arg
  "reverse" -> send_message channel (reverse arg)
  _ -> send_message channel "xot does not understand that"
  where
    func_name :: String
    arg :: String
    (func_name, arg) = case L.findIndex C.isSpace command of
      Nothing -> (command, "")
      Just i -> (take i command, drop (i + 1) command)

process_message :: D.Message -> D.DiscordHandler ()
process_message message = if (D.userIsBot (D.messageAuthor message))
  then return ()
  else case L.stripPrefix "?" (T.unpack (D.messageContent message)) of
         Just command -> process_command (D.messageChannelId message) command
         Nothing -> return ()

on_event :: D.Event -> D.DiscordHandler ()
on_event event = case event of
  D.MessageCreate message -> process_message message
  _ -> return ()

main :: IO ()
main = do
  token <- I.withFile "secrets/token.txt" I.ReadMode T.hGetLine
  error_text <- D.runDiscord (D.def {D.discordToken = token
                                    ,D.discordOnStart = on_start
                                    ,D.discordOnEnd = on_end
                                    ,D.discordOnEvent = on_event})
  putStrLn ("catastrophic error: " ++ T.unpack error_text)
