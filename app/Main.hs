module Main where

import Command 
import AgdaProc (startAgda)

main :: IO ()
main = do 
    agda <- startAgda
    test agda
    
    


