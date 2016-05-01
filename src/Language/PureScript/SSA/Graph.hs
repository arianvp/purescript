module Language.PureScript.SSA.Graph where

type CFG = [(BlockID, Block)]

newtype BlockID = BlockID Integer deriving (Eq, Ord)

type Block = [(InstID, Inst)]

newtype InstID = InstID Integer deriving (Eq, Ord)

data Inst
  = AliasInst Value

  | ArrayInst [Value]
  | ObjectInst [(String, Value)]
  | ConstructInst -- ???

  | AccessInst String Value
  | ObjectUpdateInst Value [(String, Value)]

  -- lambdas???
  | CallInst Value [Value]

  -- cases???

  | IntArithInst (Integer -> Integer -> Integer) Value Value

  | GotoInst BlockID
  | IfInst Value BlockID BlockID

data Value
  = IntValue Integer
  | DoubleValue Double
  | StringValue String
  | CharValue Char
  | BooleanValue Bool
  | InstValue InstID
