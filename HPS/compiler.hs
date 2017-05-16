import qualified JuliaParser
import qualified JuliaIR
import qualified JuliaAssembler

{-# LANGUAGE ForeignFunctionInterface #-}
import Foreign
import Foreign.C.Types

handleParseError (Left x) = error "Error parsing"
handleParseError (Right x) = x

main = do
  line <- getLine
  -- putStrLn $ show $ solution (filter (/=' ') line)
  putStrLn $ show $ {-|JuliaAssembler.assemble $ reverse $ JuliaIR.schedule $ handleParseError $-} JuliaParser.juliaParse line
