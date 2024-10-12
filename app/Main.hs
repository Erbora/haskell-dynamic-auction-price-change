module Main where

import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime, getCurrentTime)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, foldM)
import System.Random (RandomGen, newStdGen, randomR)

data Auction = Auction
    { currentPrice   :: Double
    , lastBidTime    :: UTCTime
    , bidCount       :: Int
    }

-- Function to update the price based on bidding history
updatePrice :: Auction -> IO Auction
updatePrice auction = do
    now <- getCurrentTime
    let timeElapsed = diffUTCTime now (lastBidTime auction)
    let newBidCount = bidCount auction + 1
    let newPrice = calculateNewPrice (currentPrice auction) timeElapsed newBidCount
    return $ auction { currentPrice = newPrice, lastBidTime = now, bidCount = newBidCount }

-- Dynamic pricing logic
calculateNewPrice :: Double -> NominalDiffTime -> Int -> Double
calculateNewPrice price timeElapsed bidCount
    | bidCount >= 5 && timeElapsed < 10 = price * 1.05  -- Increase price by 5% if many bids quickly
    | bidCount < 5 && timeElapsed > 10 = price - 5.0  -- Decrease price by a fixed amount if no bids for over 10 seconds
    | otherwise = price  -- No change

-- Function to simulate multiple bids
simulateBids :: Int -> Auction -> IO Auction
simulateBids 0 currentAuction = return currentAuction  -- Base case for recursion
simulateBids n currentAuction = do
    updatedAuction <- updatePrice currentAuction
    putStrLn $ "New auction price after bid " ++ show (bidCount updatedAuction) ++ ": " ++ show (currentPrice updatedAuction)
    -- Wait for a short period before the next bid
    threadDelay 1000000  -- 1 second delay
    simulateBids (n - 1) updatedAuction

-- Function to gradually decrease the price
decreasePrice :: Auction -> IO Auction
decreasePrice auction' = do
    -- Generate a random decrease amount
    gen <- newStdGen
    let (randomDecrease, _) = randomR (1, 5) gen  -- Random decrease between 1 and 5
    let decreasedPrice = currentPrice auction' - (5 + randomDecrease)  -- Reduce price by random amount
    let newAuction = auction' { currentPrice = max 0 decreasedPrice }  -- Ensure price does not go below 0
    putStrLn $ "New auction price after inactivity: " ++ show (currentPrice newAuction)
    threadDelay 1000000  -- Delay for 1 second before the next decrease
    return newAuction

-- Sample main function to demonstrate the auction price update
main :: IO ()
main = do
    initialTime <- getCurrentTime
    let auction = Auction { currentPrice = 100.0, lastBidTime = initialTime, bidCount = 0 }

    -- Simulate the first 17 bids and get the final auction state
    finalAuction <- simulateBids 17 auction

    -- Now simulate inactivity for a while to see the price decrease
    putStrLn "No bids for a while..."

    -- Number of decreases (set to 5)
    let numDecreases = 5  -- Decrease the price 5 times

    -- Use a loop to decrease the price for the specified number of times
    auctionWithDecreasedPrice <- foldM (\auction' _ -> decreasePrice auction') finalAuction [1..numDecreases] 

    -- Resume bidding after the inactivity
    putStrLn "Bidding resumes..."
    
    -- Simulate more bids (set to 7)
    finalAuctionAfterBids <- simulateBids 7 auctionWithDecreasedPrice

    putStrLn $ "Final auction price after resuming bids: " ++ show (currentPrice finalAuctionAfterBids)

    return ()