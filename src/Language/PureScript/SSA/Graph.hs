module Language.PureScript.SSA.Graph where

newtype BlockID = BlockID Int deriving (Eq, Ord)

type Block = [(InstID, Inst)]

newtype InstID = InstID Int deriving (Eq, Ord)

data Inst
  = AliasInst Value

  | ArrayInst [Value]
  | ObjectInst [(String, Value)]
  | ConstructInst -- ???

  | AccessInst Value String
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
  | BoolValue Bool
  | InstValue InstID
