import System.Environment (getArgs)
import System.IO
import System.IO.Unsafe
import Network.HTTP -- cabal install HTTP
import Text.HTML.TagSoup -- cabal install tagsoup
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad

-- This is a pretty nasty, hakish program to scrape the names and IDs of cards
-- from the Magic The Gathering "Gatherer" site.  This makes it much easier to
-- create a local database of cards that we have, since we can just go out and
-- look up the card directly to pull information.  The program itself is a huge
-- hack that basically spawns a bunch of threads, makes HTTP requests, and
-- parses out the resulting HTML.  It will almost certainly break as soon as
-- they make any tweeks to their site, but for now it's a quick way to get a
-- list of all the cards.

-- Shared state for tracking when child threads have finished
children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

-- Shared state for tracking how many threads we've spawned
threadCount :: MVar Int
threadCount = unsafePerformIO (newMVar 0)

-- Maximum number of threads to spawn
threadMax = 10

-- How long to wait after we've used up all our threads to try again
threadWaitTime = 500

-- Watch the shared state and wait until all children are accounted for
waitForChildren = do
    cs <- takeMVar children
    case cs of
        [] -> return ()
        m:ms -> do
            putMVar children ms
            takeMVar m
            waitForChildren

-- hackish way of printing out everything read on the channels, terminates 
-- with "cardInfo: thread blocked indefinitely in an MVar operation", but it's
-- fine for something quick 'n dirty
processCards chan = do
    sequence_ $ repeat (readChan chan >>= putStrLn)

-- utility functions to increment/decrement/check the shared state tracking the
-- number of threads we've spawned.
tcIncrement = do
    tc <- takeMVar threadCount
    putMVar threadCount (tc+1)

tcDecrement = do
    tc <- takeMVar threadCount
    putMVar threadCount (tc-1)

tcCheck = do
    tc <- readMVar threadCount
    return (tc < threadMax)

-- When we are below the maximum number of allowed threads, spawn a function
-- off into it's own thread and increment the counter
forkChild io = do
    tcOkay <- tcCheck
    if tcOkay then do { tcIncrement; spawnChild; } else do {threadDelay threadWaitTime;  forkChild io}
    where
    spawnChild = do
        mvar <- newEmptyMVar
        childs <- takeMVar children
        putMVar children (mvar:childs)
        forkIO (io `finally` do {putMVar mvar (); tcDecrement;})

-- As of 1/8/2012, the MTG gatherer site identifies cards by their ID
-- so to get a given card we just specify it's multiverse id
makeURL id = "http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid="++(show id)

-- Just do an HTTP request and get the body of the document as a string
fetchBody u = simpleHTTP (getRequest u) >>= getResponseBody

-- We're looking for an image tag, since the alt text on it is the only thing
-- that gives us the name of the card when we do a request from Network.HTTP;
-- when using an actual web browser (chromium at least...) there are some other
-- places where the card name is available, but those don't seem to be returned
-- now.  In any case, we just look for the element by it's "id", so if they go
-- changing the site layout this is prone to needing to be updated
isImg = (==) "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_cardImage"

-- Basically we check to see if this is the tag that we want by seeing if it
-- has an id attribute with a value matching isImg
isCardTitle t@(TagOpen s xs) = any (\x -> ((fst x) == "id") && (isImg (snd x))) xs
isCardTitle _ = False

-- If we're on the right tag, then look for the alt attribute and pull out it's
-- value to get the card name
getCardName (TagOpen s xs) = (snd . head . filter (((==) "alt").fst)) xs

-- Grab all of the tags, filter out everything that isn't the card title, then
-- get the name from the first (presumably only) remaining tag's alt text
cardName u = fmap (getCardName . head . (filter isCardTitle) . parseTags) $ fetchBody u

-- Take a number and return a string in the form of "XXX:Card Name"
cardInfo id = do
    cn <- cardName $ makeURL id
    return $ (show id) ++ ":" ++ cn

-- Take in an ID and a communication channel, and spawn off a thread to do an
-- HTTP request, parse the card name, then write the string onto the channel
writeCardInfo id chan = do
    forkChild $ writeLine chan
    where
    writeLine chan = cardInfo id >>= writeChan chan

main = do
    -- expect to get 2 numbers on the command line, a starting and ending id
    [s',e'] <- getArgs
    c <- newChan -- Create a new master communication channel
    let s = (read s') :: Int
    let e = (read e') :: Int

    forkChild $ processCards c -- spawn off a thread for writing out info

    -- Create a list of calls to our card fetching function, duplicating the
    -- communication channel for each, then call them in sequence
    sequence_ [writeCardInfo x (unsafePerformIO $ dupChan c) | x <- [s..e]]

    -- Don't terminate the master thread until all of the children have
    -- finished
    waitForChildren
