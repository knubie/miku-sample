{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Reader
import           System.IO.Unsafe
import           Text.Format
import           Network.Miku
import           Network.Wai.Handler.Warp (run)

import qualified Data.ByteString.Char8 as B

{-is_caps_msg_correct :: B.ByteString -> Either B.ByteString-}
is_caps_msg_correct msg =
  if msg == "hello"
  then Right msg
  else Left (B.pack "Message incorrect")

render_either message = case message of
  Right m -> html $ B.pack $ "<h1 style='color: green;'>" ++ (B.unpack m) ++ "</h1>"
  Left m -> html $ B.pack $ "<h1 style='color: red;'>" ++ (B.unpack m) ++ "</h1>"

main :: IO ()
main = do
  putStrLn "server started on port 3000..."
  run 3000 . miku $ do
  
  get "/" (text "miku power")

  get "/debug" (text . B.pack . show =<< ask)

  get "/say/:msg" $ do
    foo <- is_caps_msg_correct . snd . (!!0) <$> captures
    render_either foo

  get "/html" $ do
    view <- liftIO (B.readFile "index.html")
    html view
