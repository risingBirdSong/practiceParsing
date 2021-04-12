# parsing
# practiceParsing


this is working on src/realworldhaskellCSVFinal.hs main function which will read the contents and parse using the parseCSV function 

`$ stack build` 

the following example outputs to the terminal
`$ stack runhaskell src/realworldhaskellCSVFinal.hs < src/addresses.csv`

the following pipes the parsed data to a new file 
`
stack runhaskell src/realworldhaskellCSVFinal.hs < src/addresses.csv > output.txt`