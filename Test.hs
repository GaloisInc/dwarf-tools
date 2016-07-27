-- import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(isPrefixOf)
import Data.Elf
import qualified Data.ByteString as BS
import System.Environment
import qualified Data.Serialize as S
import Text.Show.Pretty-- (pPrint,ppShow)
import Hexdump

-- import Dwarf

import DW.Basics hiding (prettyHex)
import DW.Sections
import DW.Section.Abbrev
import DW.Section.Info
import DW.Section.ARanges


main :: IO ()
main =
  do let a = "/home/diatchki/src/galua/galua-c/inplace/bin/galua-dbg"
     bs <- BS.readFile a
     let elf  = parseElf bs
         end  = case elfData elf of
                  ELFDATA2LSB -> LittleEndian
                  ELFDATA2MSB -> BigEndian
         secs = Map.fromList
                  [ (name, elfSectionData s)
                    | s <- elfSections elf
                    , let name = elfSectionName s
                    , ".debug_" `isPrefixOf` name ]

     putStrLn "Debug sections:"
     mapM_ putStrLn (Map.keys secs)

     ss <- case sections end secs of
             Left err -> fail err
             Right a  -> return a

     putStrLn "\n-- ARanges -----------\n"

     case aranges ss of
       Left err -> fail err
       Right a  -> pPrint a


     putStrLn "\n-- Info -----------\n"

     info_bytes <- case Map.lookup ".debug_info" secs of
                     Nothing -> fail "Can't find debug info"
                     Just sec_bytes -> return sec_bytes

     case dieFrom ss 959 of
       Left err -> fail err
       Right ok -> pPrint ok


