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

makeAddressFromCSV_A line = Address firstName' lastName' street' city' state' (read zip')
    where all@[firstName', lastName', street', city', state', zip'] = splitOn "," line


