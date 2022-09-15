module Chapter9.ControlledVisit where
import System.Directory (Permissions)
import Data.Time.Clock.System (SystemTime)
import System.FilePath ((</>))
import Control.Monad

data Info = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe SystemTime
}

getInfo :: FilePath -> IO Info
getInfo = undefined 

getUsefulContents :: FilePath -> IO [a0]
getUsefulContents = undefined

isDirectory :: Info -> Bool
isDirectory = undefined

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    fmap concat $ forM (order contents) $ \info -> do
        if isDirectory info && infoPath info /= path
            then Chapter9.ControlledVisit.traverse order (infoPath info)
            else return [info]




