module Main where

import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Concurrent (threadDelay)
import System.IO (hFlush, stdout)
import System.Timeout (timeout)

data Auction = Auction
    { currentPrice   :: Double
    , lastBidTime    :: UTCTime
    , bidCount       :: Int
    }

-- Countdown function to inform the user
countdown :: Int -> IO ()
countdown 0 = return ()
countdown n = do
    putStrLn $ "You have " ++ show n ++ " seconds to bid."
    threadDelay 1000000  -- Wait for 1 second
    countdown (n - 1)

-- Countdown and input function
countdownAndGetInput :: Int -> IO (Maybe String)
countdownAndGetInput seconds = do
    let timer = countdown seconds
    result <- timeout (seconds * 1000000) getLine  -- Timeout in microseconds
    timer `seq` return result  -- Ensure countdown completes while waiting for input

-- Function to handle bidding with keyboard input and countdown
processBids :: Auction -> IO ()
processBids auction = do
    putStrLn $ "Current auction price: " ++ show (currentPrice auction) ++ "\n"
    putStr "Enter your bid amount (or type 'yes' to auto-bid +5, or 'exit' to end the auction): "
    hFlush stdout
    input <- countdownAndGetInput 10  -- 10-second timeout with countdown
    now <- getCurrentTime
    case input of
        Nothing -> do
            -- Auction ends due to inactivity
            putStrLn $ "Auction ended due to inactivity. Product sold at last bid price: " ++ show (currentPrice auction)
        Just inputLine -> case inputLine of
            "exit" -> putStrLn $ "Auction ended by user. Product sold at last bid price: " ++ show (currentPrice auction)
            "yes" -> do
                let newBid = currentPrice auction + 5
                let newAuction = auction 
                        { currentPrice = newBid
                        , lastBidTime = now
                        , bidCount = bidCount auction + 1 
                        }
                putStrLn $ "Auto-bid accepted! New price: " ++ show (currentPrice newAuction)
                processBids newAuction
            _ -> do
                let maybeBid = readMaybe inputLine :: Maybe Double
                case maybeBid of
                    Just bid ->
                        if bid > currentPrice auction
                            then do
                                let newAuction = auction 
                                        { currentPrice = bid
                                        , lastBidTime = now
                                        , bidCount = bidCount auction + 1 
                                        }
                                putStrLn $ "Bid accepted! New price: " ++ show (currentPrice newAuction)
                                processBids newAuction
                            else do
                                putStrLn "Bid must be higher than the current price. Try again."
                                processBids auction
                    Nothing -> do
                        putStrLn "Invalid input. Please enter a numeric value."
                        processBids auction

-- Helper function to safely parse a string into a number
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(val, "")] -> Just val
    _ -> Nothing

-- Main function to start the auction
main :: IO ()
main = do
    initialTime <- getCurrentTime
    let auction = Auction { currentPrice = 100.0, lastBidTime = initialTime, bidCount = 0 }
    putStrLn "Starting the auction..."
    processBids auction