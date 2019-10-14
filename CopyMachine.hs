import System.Directory
import System.Environment   
import System.FilePath (takeFileName, (</>))
import System.IO
import System.IO.Error

main = do
    (fileToCopy:destination:depth:xs) <- getArgs
    copyFiles fileToCopy (read depth) destination 

copyFiles :: FilePath -> Int -> FilePath -> IO ()
copyFiles fileToCopy depth destination = catchIOError (toTry fileToCopy depth destination) (handler fileToCopy destination)

toTry :: FilePath -> Int -> FilePath -> IO ()
toTry fileToCopy depth destination = do 
    absoluteDestination <- makeAbsolute destination
    destinationContents <- listDirectory absoluteDestination
    let destinationFolders = checkDirs destinationContents
    let fileName = takeFileName fileToCopy
    copyFile fileToCopy $ absoluteDestination </> fileName
    putStrLn $ "copied " ++ fileName ++ " to " ++ absoluteDestination
    let dirPath = absoluteDestination ++ "/"
    if (destinationFolders == Nothing) || ( depth < 1)
        then return ()
        else let (Just dirs) = destinationFolders in mapM_ (copyFiles fileToCopy (depth - 1)) (map (dirPath ++) dirs) 

handler :: FilePath -> FilePath -> IOError -> IO ()
handler _ _ e = putStrLn "\nAn exception!"
    
isDir :: FilePath -> Bool
isDir [] = True
isDir (x:xs) = isDir xs && if x == '.' then False else True

checkDirs :: [FilePath] -> Maybe [FilePath]
checkDirs [] = Nothing
checkDirs x = if filtered == [] then Nothing else Just filtered
    where filtered = filter (isDir) x