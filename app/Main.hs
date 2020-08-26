module Main where

import Player
import Password

import qualified Data.ByteString as BS

main :: IO ()
main = do
    let p = Player 1 2 3 4 5 
    let empty_pwd = Password.new BS.empty 
    let pwd = Password.new $ BS.singleton 54
    print $ p  
    print $ empty_pwd
    print $ "lol"
    print $ pwd
    let strp = show p
    print $ strp
    let p2 = read strp :: Player
    print $ p2
