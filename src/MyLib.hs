module MyLib where

import qualified GCLParser.Parser as GCL
import qualified GCLParser.GCLDatatype as GCLD

testParsing :: IO (GCL.ParseResult GCLD.Program)
testParsing = GCL.parseGCLfile "examples/examples/minind.gcl"