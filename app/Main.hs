module Main where
import qualified Control.Monad.IO.Class as MO
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
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

data Action = ActionSendMessage D.ChannelId String

is_bad_message :: String -> Bool
is_bad_message = all C.isSpace

execute_action :: Action -> D.DiscordHandler ()
execute_action (ActionSendMessage channel content) = if is_bad_message content
  then return () else do
    result <- D.restCall (D.CreateMessage channel (T.pack content))
    case result of
      Left e -> MO.liftIO (putStrLn ("message send error: " ++ show e))
      Right _ -> return ()

string_func_map :: M.Map String (String -> String)
string_func_map = M.fromList [("echo", id)
                             ,("reverse", reverse)]

command_to_actions :: D.ChannelId -> String -> [Action]
command_to_actions channel command = case M.lookup func_name string_func_map of
  Nothing -> [ActionSendMessage channel "xot does not understand that"]
  Just f -> [ActionSendMessage channel (f arg)]
  where
    func_name :: String
    arg :: String
    (func_name, arg) = case L.findIndex C.isSpace command of
      Nothing -> (command, "")
      Just i -> (take i command, drop (i + 1) command)

message_to_actions :: D.Message -> [Action]
message_to_actions message = if (D.userIsBot (D.messageAuthor message))
  then []
  else case L.stripPrefix prefix content of
         Just command -> command_to_actions channel command
         Nothing -> []
  where
    channel :: D.ChannelId
    channel = D.messageChannelId message

    content :: String
    content = T.unpack (D.messageContent message)

    prefix :: String
    prefix = "?"

on_event :: D.Event -> D.DiscordHandler ()
on_event event = case event of
  D.MessageCreate message -> mapM_ execute_action (message_to_actions message)
  _ -> return ()

main :: IO ()
main = do
  token <- I.withFile "secrets/token.txt" I.ReadMode T.hGetLine
  error_text <- D.runDiscord (D.def {D.discordToken = token
                                    ,D.discordOnStart = on_start
                                    ,D.discordOnEnd = on_end
                                    ,D.discordOnEvent = on_event})
  putStrLn ("catastrophic error: " ++ T.unpack error_text)
