{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric,DeriveAnyClass #-}

module Astbuilder(
    mkAST
    )where
import Cmm_alex
import Cmm_happy
import Dictutils
import Data.Either
import Data.Aeson(ToJSON)
import GHC.Generics

data Type = Number | Reference Int deriving (Eq, Show, Generic, ToJSON)
data Vardecl = Vardecl String Type deriving (Eq, Show, Generic, ToJSON)
data Funcdecl = Funcdecl String [String] Statement deriving (Eq, Show, Generic, ToJSON)
type Declaration = Either Vardecl Funcdecl
    
data Statement =
    Complex [Vardecl] [Statement] |
    Ite Expression Statement (Maybe Statement) |
    While Expression Statement |
    Expsta Expression |
    Return (Maybe Expression)
    deriving (Eq, Show, Generic, ToJSON)
    
data Expression =
    ConstInt Int |
    ConstArr [Int] |
    Takeval Expression |
    Takeadr String |
    Call String [Expression] |
    Assign [Expression] Expression
    deriving (Eq, Show, Generic, ToJSON)

convexpr::TExpression->Expression    
convexpr = \case
    ComplEx [] right ->
        convexpr right
    ComplEx lefts right -> 
        Assign (map convref lefts) (convexpr right)
    CallEx (Posed _ nam) parexprs -> 
        Call nam (map convexpr parexprs)
    Retrieval ref ->
        Takeval $ convref ref
    NumLiteral (Posed _ num) ->
        ConstInt num
    StringLiteral (Posed _ arr) ->
        ConstArr arr
    
convref::Reference->Expression
convref ((Posed _ nam), Nothing) = Takeadr nam
convref ((Posed _ nam), Just adrexpr) = Call "+" [Takeadr nam, convexpr adrexpr]

convvardecl::TDeclaration->Vardecl
convvardecl = \case
    Intdecl (Posed _ nam) -> Vardecl nam Number
    Arrdecl (Posed _ nam) (Posed _ size) -> Vardecl nam (Reference size)
    _ -> error "Unexpected function declaration"

convstat::TStatement->Statement
convstat = \case
    CompSta tdecls tstats ->
        Complex (map convvardecl tdecls) (map convstat tstats)
    SelSta ifexpr tstat mestat -> 
        Ite (convexpr ifexpr) (convstat tstat) (fmap convstat mestat)
    IterSta whexpr wstat ->
        While (convexpr whexpr) (convstat wstat)
    RetSta _ mexpr ->
        Return $ fmap convexpr mexpr
    ReadSta ref ->
        Expsta (Call "<<" [convref ref])
    ExpSta texpr ->
        Expsta (convexpr texpr)
    EmpSta -> 
        Complex [] []
        
mkAST::[TDeclaration]->[Declaration]
mkAST [] = []
mkAST (t:ree) = case t of
    Intdecl (Posed _ nam) -> 
        (Left $ Vardecl nam Number):mkAST ree
    Arrdecl (Posed _ nam) (Posed _ size) -> 
        (Left $ Vardecl nam (Reference size)):mkAST ree
    Fundecl (Posed _ nam) pardecls stat ->
        (Right $ Funcdecl nam (getnams pardecls) (convstat stat)):mkAST ree
    Procdecl (Posed _ nam) pardecls stat ->
        (Right $ Funcdecl nam (getnams pardecls) (convstat stat)):mkAST ree
    where 
        getnams::[TDeclaration]->[String]
        getnams [] = []
        getnams (d:ecl) = case d of
            Intdecl (Posed _ nam) -> nam:getnams ecl
            Arrdecl (Posed _ nam) _ -> nam:getnams ecl
            _ -> error "Unexpected function declaration in function parameters"