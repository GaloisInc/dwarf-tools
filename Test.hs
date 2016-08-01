import Data.Elf
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import Data.List(isPrefixOf)
import System.Environment
import Text.Read(readMaybe)
import qualified Data.Map as Map

import DWARF.Basics
import DWARF.Addr2Line

fromElf :: ByteString -> Sections
fromElf bs = sections end
           $ Map.fromList [ (name, elfSectionData s)
                          | s <- elfSections elf
                          , let name = elfSectionName s
                          , ".debug_" `isPrefixOf` name ]
  where
  elf  = parseElf bs
  end  = case elfData elf of
           ELFDATA2LSB -> LittleEndian
           ELFDATA2MSB -> BigEndian


main :: IO ()
main =
  do args <- getArgs
     case args of
       [file,addrTxt] | Just addr <- readMaybe addrTxt ->
         do secs <- fromElf <$> BS.readFile file
            print (addr2line secs addr)

       _ -> putStrLn "USAGE: FILE ADDRESS"

