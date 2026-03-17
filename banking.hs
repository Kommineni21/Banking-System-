-- Full Interactive Immutable Banking System

import Data.List (find)
import System.IO (hFlush, stdout)

-- Define a BankAccount type
data BankAccount = BankAccount {
    accountId :: Int,
    ownerName :: String,
    balance   :: Double
} deriving (Show)

type Bank = [BankAccount]

-- Deposit function
deposit :: Double -> BankAccount -> BankAccount
deposit amount acc = acc { balance = balance acc + amount }

-- Withdraw function
withdraw :: Double -> BankAccount -> BankAccount
withdraw amount acc
    | amount <= balance acc = acc { balance = balance acc - amount }
    | otherwise = acc

-- Transfer function
transfer :: Double -> BankAccount -> BankAccount -> (BankAccount, BankAccount)
transfer amount fromAcc toAcc
    | amount <= balance fromAcc =
        let newFrom = withdraw amount fromAcc
            newTo   = deposit amount toAcc
        in (newFrom, newTo)
    | otherwise = (fromAcc, toAcc)

-- Find account by ID
findAccount :: Int -> Bank -> Maybe BankAccount
findAccount accId bank = find (\acc -> accountId acc == accId) bank

-- Update account in the bank
updateAccount :: BankAccount -> Bank -> Bank
updateAccount updatedAcc = map (\acc -> if accountId acc == accountId updatedAcc then updatedAcc else acc)

-- Transfer between accounts in the bank
transferInBank :: Double -> Int -> Int -> Bank -> Bank
transferInBank amount fromId toId bank =
    case (findAccount fromId bank, findAccount toId bank) of
        (Just fromAcc, Just toAcc) ->
            let (newFrom, newTo) = transfer amount fromAcc toAcc
            in updateAccount newTo (updateAccount newFrom bank)
        _ -> bank

-- Print all accounts
printBank :: Bank -> IO ()
printBank [] = putStrLn "No accounts in the bank."
printBank bank = mapM_ print bank

-- Create a new account
createAccount :: Int -> String -> Double -> Bank -> Bank
createAccount newId name initialBalance bank =
    bank ++ [BankAccount newId name initialBalance]

-- Command loop
bankLoop :: Bank -> IO ()
bankLoop bank = do
    putStrLn "\nOptions: create, deposit, withdraw, transfer, show, exit"
    putStr "Enter command: "
    hFlush stdout
    cmd <- getLine
    case words cmd of
        ["create", accIdStr, name, balStr] ->
            let accId = read accIdStr
                bal   = read balStr
                newBank = createAccount accId name bal bank
            in do
                putStrLn "Account created."
                printBank newBank
                bankLoop newBank

        ["deposit", accIdStr, amtStr] ->
            let accId = read accIdStr
                amt   = read amtStr
            in case findAccount accId bank of
                Just acc -> do
                    let newBank = updateAccount (deposit amt acc) bank
                    putStrLn "Deposit successful."
                    printBank newBank
                    bankLoop newBank
                Nothing -> do
                    putStrLn "Account not found."
                    bankLoop bank

        ["withdraw", accIdStr, amtStr] ->
            let accId = read accIdStr
                amt   = read amtStr
            in case findAccount accId bank of
                Just acc -> do
                    let newBank = updateAccount (withdraw amt acc) bank
                    putStrLn "Withdraw successful."
                    printBank newBank
                    bankLoop newBank
                Nothing -> do
                    putStrLn "Account not found."
                    bankLoop bank

        ["transfer", fromIdStr, toIdStr, amtStr] ->
            let fromId = read fromIdStr
                toId   = read toIdStr
                amt    = read amtStr
                newBank = transferInBank amt fromId toId bank
            in do
                putStrLn "Transfer attempted."
                printBank newBank
                bankLoop newBank

        ["show"] -> do
            printBank bank
            bankLoop bank

        ["exit"] -> putStrLn "Exiting banking system."

        _ -> do
            putStrLn "Invalid command."
            bankLoop bank

-- Initial setup
main :: IO ()
main = do
    let bank = []
    putStrLn "Welcome to the Immutable Banking System!"
    bankLoop bank