import Data.List.Split 
testAddress = "John,Doe,120 jefferson st.,Riverside, NJ, 08075"

data Address = Address {
    firstName :: String ,
    lastName :: String , 
    street :: String , 
    city :: String , 
    state :: String , 
    zip :: Int 
} deriving (Eq, Show, Read)

test = Address "john" "doe" "120 jefferson st" "riverside" "nj" 08075

makeAddressFromCSV'A line = Address firstName' lastName' street' city' state' (read zip')
    where all@[firstName', lastName', street', city', state', zip'] = splitOn "," line

makeAddressFromCSV'B line =  case (splitOn "," line) of 
                                [firstName', lastName', street', city', state', zip']
                                     ->  Right (Address firstName' lastName' street' city' state' (read zip'))
                                _ -> Left ("something went wrong")
                                
                                    -- _ -> Left ("something went wrong")
main = do 
   thefile <- readFile "src/addresseseasy.csv"
   let mapped = map makeAddressFromCSV'A $ lines thefile
   mapM_ print mapped

mainB = do 
   thefile <- readFile "src/addresses.csv"
   let mapped = map makeAddressFromCSV'B $ lines thefile
   mapM_ print mapped
