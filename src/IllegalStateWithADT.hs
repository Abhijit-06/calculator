module IllegalStateWithADT where
-- I found this ADT example easy to explain to begginers
-- Note does a phantom type make sense? Pros/Cons?
-- Note dont understand how to use the data constructor as in the example
type Backend = Int
type Frontend = [Int]

-- this not working
--data settings = backend | frontend deriving (show)
data Configuration
    = OnlyBackend Backend 
    | OnlyFrontend Frontend
    | BothSettings Frontend Backend 
    deriving (Show)

runApp :: Configuration -> IO ()
runApp config = case config of
    OnlyBackend back -> print $ "Backkkkk"
    OnlyFrontend back -> print $ "FrontEnd"
    BothSettings front back -> print $ "Both of them"