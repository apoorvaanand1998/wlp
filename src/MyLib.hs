module MyLib where

import qualified GCLParser.Parser as GCL
import qualified GCLParser.GCLDatatype as GCLD
import Paths
import WLP

treeGCLfile :: Int -> FilePath -> IO (GCL.ParseResult PathTree)
treeGCLfile k = (fmap.fmap.fmap $ limitDepth k . fst . programTree) GCL.parseGCLfile

testParsing :: IO (GCL.ParseResult GCLD.Program)
testParsing = GCL.parseGCLfile "examples/examples/min.gcl"

testWLP :: IO GCLD.Expr
testWLP = do
  (Right tree) <- treeGCLfile 111 "examples/examples/min.gcl"
  return $ repBy $ verify mempty (GCLD.LitB True) tree
