import System (getArgs)
import System.IO
import System.IO.Unsafe
import Network.HTTP
import Text.HTML.TagSoup
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

threadCount :: MVar Int
threadCount = unsafePerformIO (newMVar 0)

threadMax = 10

waitForChildren = do
    cs <- takeMVar children
    case cs of
        [] -> return ()
        m:ms -> do
            putMVar children ms
            takeMVar m
            waitForChildren

processCards chan = do
    sequence_ $ repeat (readChan chan >>= putStrLn)

tcIncrement = do
    tc <- takeMVar threadCount
    putMVar threadCount (tc+1)

tcDecrement = do
    tc <- takeMVar threadCount
    putMVar threadCount (tc-1)

tcCheck = do
    tc <- readMVar threadCount
    return (tc < threadMax)

forkChild io = do
    tcOkay <- tcCheck
    if tcOkay then do { tcIncrement; spawnChild; } else do {threadDelay 500;  forkChild io}
    where
    spawnChild = do
        mvar <- newEmptyMVar
        childs <- takeMVar children
        putMVar children (mvar:childs)
        forkIO (io `finally` do {putMVar mvar (); tcDecrement;})

makeURL id = "http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid="++(show id)
fetchBody u = simpleHTTP (getRequest u) >>= getResponseBody
isHeader = (==) "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_cardImage"
isCardTitle t@(TagOpen s xs) = any (\x -> ((fst x) == "id") && (isHeader (snd x))) xs
isCardTitle _ = False
getCardName (TagOpen s xs) = (snd . head . filter (((==) "alt").fst)) xs

cardName u = fmap (getCardName . head . (filter isCardTitle) . parseTags) $ fetchBody u
cardInfo id = do
    cn <- cardName $ makeURL id
    return $ (show id) ++ ":" ++ cn

writeCardInfo id chan = do
    forkChild $ writeLine chan
    where
    writeLine chan = cardInfo id >>= writeChan chan

main = do
    [s',e'] <- getArgs
    c <- newChan
    let s = (read s') :: Int
    let e = (read e') :: Int
    forkChild $ processCards c
    sequence_ [writeCardInfo x (unsafePerformIO $ dupChan c) | x <- [s..e]]
    waitForChildren
