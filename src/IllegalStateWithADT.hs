module IllegalStateWithADT where
-- I found this ADT example easy to explain to begginers
-- Note does a phantom type make sense? Pros/Cons?
-- Note dont understand how to use the data constructor as in the example
type Backend = Int
type Frontend = [Int]

-- This not working
--data Settings = Backend | Frontend deriving (Show)
data Configuration
    = OnlyBackend Backend 
    | OnlyFrontEnd Frontend
    | BothSettings Frontend Backend 
    deriving (Show)

runApp :: Configuration -> IO ()
runApp config = case config of
    OnlyBackend back -> print $ "Backkkkk"
    OnlyFrontEnd back -> print $ "FrontEnd"
    BothSettings front back -> print $ "Both of them"