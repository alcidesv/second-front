{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module SecondFront.Workers.FileServer(
    -- | Very basic file-serving utility. It includes support for mime types 
    --   based on file extension, and for index files. 
    --   It doesn't include support for anything fancy in the URLs, in particular
    --   query strings are not parsed or used for anything
    fileServer,

    FileServerConfig(..),
    HidePattern(..),

    defaultFileServerConfig,

    basePath_FSC,
    suffixToMimeTypes_FSC,
    validIndexFileNames_FSC
    ) where 


import           Control.Lens
import qualified Control.Lens             as L

import qualified Data.ByteString          as B
import           Data.ByteString.Char8    (pack, unpack)
import           Data.List                (isInfixOf, isSuffixOf, isPrefixOf)
import           Data.Conduit             (yield)
import           Data.Foldable            (find)

import           System.Directory         (doesFileExist)
import           System.FilePath
import           System.Log.Logger

import qualified Network.URI              as U


import           SecondFront.Workers.Data
import           SecondTransfer           (CoherentWorker, PrincipalStream)

-- | File patterns to hide. This can be improved a lot, but for
--   now let's have simple patterns
data HidePattern = 
     Prefix_HP FilePath 
    |Suffix_HP FilePath 



-- | Config information for the file server. Notice that all members of this 
--   struct have associated lenses, prefer to use those
data FileServerConfig = FileServerConfig  {
    -- | The path which will be prefixed to paths present in the URL
    _basePath_FSC             :: FilePath,
    -- | A list of file extensions (including the '.') and of 
    --   mime-types
    _suffixToMimeTypes_FSC   :: [(B.ByteString, B.ByteString)],
    -- | A list of files to search for when the path looks like a directory, 
    --   i.e. it ends in '/' or is at the very top of the domain.
    _validIndexFileNames_FSC :: [FilePath],
    -- | A list of file patterns to hide. 
    _hidePatterns :: [HidePattern]
  }


L.makeLenses ''FileServerConfig

-- | Default file server config. It serves files from the current directory,
--  serves index.html and hides something starting with "_priv/"
defaultFileServerConfig :: FileServerConfig
defaultFileServerConfig = FileServerConfig {
    _basePath_FSC = "."
    ,_suffixToMimeTypes_FSC = defaultSuffixToMimeTypes
    ,_validIndexFileNames_FSC = ["index.html"]
    ,_hidePatterns = [
        Prefix_HP "_priv/"
      ]
  }


-- | Very simple and probably insecure file server. In order to harden it a bit, 
--   we ignore relative paths that contain '..' and '%' signs, but the result can 
--   still be unsafe :-(
fileServer :: FileServerConfig -> CoherentWorker
fileServer config (headers, _) = do 
    let 
        base_path = config ^. basePath_FSC
        mime_types = config ^. suffixToMimeTypes_FSC
        valid_index_file_names = config ^. validIndexFileNames_FSC
        hide_patterns = config ^. hidePatterns
        maybe_usable_uri = do 
            path <- lookup ":path" headers
            U.parseRelativeReference . unpack $ path
    case maybe_usable_uri of 
        Nothing     ->  do 
            -- Indeed this circumstance should be captured by second-transfer, that is,
            -- where there no :path pseudo-header is present
            errorM "2ndF" $ "Couln't find path between pseudo-headers" 
            send501

        Just (U.URI {U.uriPath=some_path}) -> do

            case lookup ":method" headers of 

                Just "GET"  -> do
                    -- RFC 7231 makes methods case-sensitive, so any variation of this is an anomaly 
                    -- and should be rejected

                    infoM "2ndF" $ "Requested relUri = " ++ some_path

                    -- Very basic security
                    if  (".." `isInfixOf` some_path ) || ("%" `isInfixOf` some_path) || (mostHide hide_patterns some_path) then 
                        send404
                    else do
                        let 
                            relativized_path = case head some_path of 
                                '/'       -> tail some_path 
                                -- TODO: this one is not correct, all paths coming in 
                                -- requests should be absolute.
                                _         -> some_path
                            full_path = base_path </> relativized_path
                            mime_type = getRelPathMime mime_types relativized_path

                        infoM "2ndF" $ "  full path = " ++ full_path

                        maybe_contents <- fetchFile full_path

                        case maybe_contents of 

                            Just contents -> do

                                -- Seems I can deliver something.... 
                                return (
                                    good200ResponseHeaders mime_type (B.length contents),
                                    [],
                                    ( do 
                                        yield contents
                                        -- TODO: Care to add a hash here?
                                        return []
                                    )
                                  )

                            -- Let's do a search with alternative names
                            Nothing ->  do 
                                check <- pathGoesToIndex valid_index_file_names full_path
                                case check of 
                                    Just index_fname -> do
                                        Just contents2 <- fetchFile index_fname
                                        -- When sending default contents, classify it as text/html, no 
                                        -- matter the actual extension of files in the filesystem....
                                        -- otherwise it doesn't make sense.
                                        return (
                                            good200ResponseHeaders "text/html" (B.length contents2),
                                            [],
                                            ( do 
                                                yield contents2
                                                -- TODO: Care to add a hash here?
                                                return []
                                            )
                                          )

                                    Nothing -> send404
              
                Just _      -> 
                    -- That's an incorrect method
                    return (
                        bad501ResponseHeaders,
                        [],
                        ( do 
                            yield bad501ResponseData
                            return []
                        )
                      )

                -- This actually another protocol error
                Nothing -> send501


fetchFile :: FilePath ->  IO (Maybe B.ByteString)
fetchFile full_path  = do
    exists <- doesFileExist full_path
    if exists then
      do 
        contents <- B.readFile full_path
        return $ Just contents
    else 
      do 
        return $ Nothing            


send501 :: IO PrincipalStream
send501 =  return (
        bad501ResponseHeaders,
        [],
        ( do 
            yield bad501ResponseData
            return []
        )
    )


mostHide :: [HidePattern] -> FilePath -> Bool 
mostHide hide_patterns rel_path = 
    foldl (\ a h -> a || (rel_path `hideMatches` h) ) False hide_patterns


hideMatches :: FilePath -> HidePattern -> Bool 
hideMatches rel_path (Prefix_HP p) = p `isPrefixOf` rel_path
hideMatches rel_path (Suffix_HP p) = p `isSuffixOf` rel_path

-- TODO: We need to load this list from somewhere else
defaultSuffixToMimeTypes :: [ (B.ByteString, B.ByteString) ]
defaultSuffixToMimeTypes = [
    (".js", "application/x-javascript")
    ,(".html", "text/html")
    ,(".css", "text/css")
    ,(".svg", "image/svg+xml")
    ,(".json", "application/json")
    ,(".txt", "text/plain")
    ]

getRelPathMime :: [ (B.ByteString, B.ByteString) ] -> String -> B.ByteString
getRelPathMime suffix_to_mime_list rel_path = case maybe_mime of 
    Just (_, mime_type) -> mime_type
    Nothing             -> "application/octet-stream" 
  where 
    maybe_mime  = find (\ (ext, _) -> ext `B.isSuffixOf` rel_path_bs ) suffix_to_mime_list
    rel_path_bs = pack rel_path


pathGoesToIndex :: [FilePath] -> String -> IO (Maybe String)
pathGoesToIndex valid_index_names abs_path = let 
    ends_with_slash = "/" `isSuffixOf` abs_path 
    if_exists [] = return Nothing
    if_exists (valid_index_filename:vins) = let 
        index_fname = if ends_with_slash then
            abs_path ++ valid_index_filename
          else 
            abs_path ++ "/" ++ valid_index_filename
      in do 
        b <- doesFileExist index_fname  
        if b then 
            return $ Just index_fname
          else 
            if_exists vins
  in 
    if_exists valid_index_names   



send404 :: IO PrincipalStream
send404 = return (
    bad404ResponseHeaders,
    [],
    ( do 
        yield bad404ResponseData
        return []
    )
  )
