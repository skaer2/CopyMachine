import System.Directory
import System.Environment   
import System.FilePath (takeFileName, (</>))

main = do
    (fileToCopy:destination:xs) <- getArgs
    copyFiles fileToCopy destination

copyFiles :: FilePath -> FilePath -> IO ()
copyFiles fileToCopy relativeDir = do 
    dir <- makeAbsolute relativeDir
    thisDir <- listDirectory dir
    let newDirs = checkDirs thisDir
    let fileName = takeFileName fileToCopy
    copyFile fileToCopy $ dir </> fileName
    putStrLn $ "copied " ++ fileName ++ " to " ++ dir
    let dirPath = dir ++ "/"
    if newDirs == Nothing 
        then return ()
        else let (Just dirs) = newDirs in mapM_ (copyFiles fileToCopy) (map (dirPath ++) dirs) 
            
    
            
    
isDir :: FilePath -> Bool
isDir [] = True
isDir (x:xs) = isDir xs && if x == '.' then False else True

checkDirs :: [FilePath] -> Maybe [FilePath]
checkDirs [] = Nothing
checkDirs x = if filtered == [] then Nothing else Just filtered
    where filtered = filter (isDir) x