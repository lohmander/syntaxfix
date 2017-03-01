module Parser
    ( parser
    ) where


import qualified Text.Megaparsec        as P
import           Text.Megaparsec.String

import           AST
import           Parser.Declaration


parser :: Parser Module
parser = do
    (Module name exports _) <- pModule
    decls <- P.some pDecl
    P.eof
    return $ Module name exports decls



