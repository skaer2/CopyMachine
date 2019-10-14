import System.Directory
import System.Environment   
import System.FilePath (takeFileName, (</>))
import System.IO
import System.IO.Error

main = do
    (fileToCopy:destination:depth:numberOfCopies:xs) <- getArgs
    initCopyMachine fileToCopy (read depth) destination  (read numberOfCopies)

initCopyMachine :: FilePath -> Int -> FilePath -> Int -> IO ()
initCopyMachine fileToCopy depth destination 0 = copyFiles fileToCopy depth destination
initCopyMachine fileToCopy depth destination numberOfCopies = catchIOError (copyFiles fileToCopy depth destination) (handler fileToCopy destination)


copyFiles :: FilePath -> Int -> FilePath -> IO ()
copyFiles fileToCopy depth destination = catchIOError (copyFiles_ fileToCopy depth destination) (handler fileToCopy destination)
    where   copyFiles_ :: FilePath -> Int -> FilePath -> IO ()
            copyFiles_ fileToCopy depth destination = do 
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

                    
copyFilesMultiCopy :: FilePath -> Int -> FilePath -> Int -> IO ()
copyFilesMultiCopy fileToCopy depth destination numberOfCopies = catchIOError (copyFiles_ fileToCopy depth destination) (handler fileToCopy destination)
    where   copyFilesMultiCopy_ :: FilePath -> Int -> FilePath -> Int -> IO ()
            copyFilesMultiCopy_ fileToCopy depth destination numberOfCopies = do 
                absoluteDestination <- makeAbsolute destination
                destinationContents <- listDirectory absoluteDestination
                let destinationFolders = checkDirs destinationContents
                let fileName = takeFileName fileToCopy
                copyFile fileToCopy $ absoluteDestination </> fileName
                putStrLn $ "copied " ++ fileName ++ " to " ++ absoluteDestination
                let dirPath = absoluteDestination ++ "/"
                if (destinationFolders == Nothing) || ( depth < 1)
                    then return ()
                    else let (Just dirs) = destinationFolders in mapM_ (copyFilesMultiCopy fileToCopy (depth - 1) numberOfCopies) (map (dirPath ++) dirs) 

--fileAlreadyExists <- doesFileExist fileToCopy
        --if fileAlreadyExists 
            --then copyFile fileToCopy $ dir </> $ fileName ++

multiCopy :: FilePath -> FilePath -> Int -> IO ()
multiCopy fileToCopy destination numberOfCopies 
multiCopy fileToCopy destination numberOfCopies = do
    absoluteDestination <- makeAbsolute destination
    fileAlreadyExists <- doesFileExist fileToCopy
        if fileAlreadyExists 
            then copyFile fileToCopy $ absoluteDestination </> $ fileName ++ numberOfCopies

handler :: FilePath -> FilePath -> IOError -> IO ()
handler _ _ e = putStrLn "\nAn exception!"
    
isDir :: FilePath -> Bool
isDir [] = True
isDir (x:xs) = isDir xs && if x == '.' then False else True

checkDirs :: [FilePath] -> Maybe [FilePath]
checkDirs [] = Nothing
checkDirs x = if filtered == [] then Nothing else Just filtered
    where filtered = filter (isDir) x