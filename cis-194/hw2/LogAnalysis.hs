module LogAnalysis where

  import Log

  toInt :: String -> Int
  toInt x = read x::Int

  parseMessage :: String -> LogMessage
  parseMessage str =
    case words str of
      ("I": ts : msg) -> LogMessage Info (toInt ts) (unwords msg)
      ("W": ts : msg) -> LogMessage Warning (toInt ts) (unwords msg)
      ("E": sev : ts : msg) -> LogMessage (Error (toInt sev)) (toInt ts) (unwords msg)
      _ -> Unknown str
  
  parse :: String -> [LogMessage]
  parse str = map parseMessage (lines str)

  insert :: LogMessage -> MessageTree -> MessageTree
  insert msg@LogMessage{} Leaf = Node Leaf msg Leaf
  insert msg1@(LogMessage _ ts1 _) (Node left msg2@(LogMessage _ ts2 _) right)
    | ts1 > ts2 = Node left msg2 (insert msg1 right)
    | otherwise = Node (insert msg1 left) msg2 right
  insert (Unknown msg) tree = tree

  build :: [LogMessage] -> MessageTree
  build = foldr insert Leaf

  inOrder :: MessageTree -> [LogMessage]
  inOrder Leaf = []
  inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

  isGreaterThan50 :: LogMessage -> Bool
  isGreaterThan50 (LogMessage (Error sev) _ _) = sev > 50
  isGreaterThan50 _ = False

  getMessage :: LogMessage -> String
  getMessage (LogMessage _ _ msg) = msg

  whatWentWrong :: [LogMessage] -> [String]
  whatWentWrong xs = map getMessage (filter isGreaterThan50 xs)

  main = testWhatWentWrong parse whatWentWrong "error.log"