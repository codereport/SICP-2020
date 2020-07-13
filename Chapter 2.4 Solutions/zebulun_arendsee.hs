-------------------------------------------------------------------------------
-- SICP 2.4.2                                                                --
-- Implementtion of complex numbers with algebraic types                     --
-------------------------------------------------------------------------------

-- Rather than type tags, in Haskell we use algebraic types:
data Complex = Polar Double Double | Rectangular Double Double deriving(Show)

add' :: Complex -> Complex -> Complex
add' (Rectangular r1 i1) (Rectangular r2 i2) = Rectangular (r1 + r2) (i1 + i2) 
add' x y = add' (asRectangular x) (asRectangular y)

sub' :: Complex -> Complex -> Complex
sub' (Rectangular r1 i1) (Rectangular r2 i2) = Rectangular (r1 - r2) (i1 - i2) 
sub' x y = sub' (asRectangular x) (asRectangular y)

mul' :: Complex -> Complex -> Complex
mul' (Polar r1 a1) (Polar r2 a2) = Polar (r1 * r2) (a1 + a2)
mul' x y = mul' (asPolar x) (asPolar y)

div' :: Complex -> Complex -> Complex
div' (Polar r1 a1) (Polar r2 a2) = Polar (r1 / r2) (a1 - a2)
div' x y = div' (asPolar x) (asPolar y)

asRectangular :: Complex -> Complex
asRectangular x@(Rectangular _ _) = x
asRectangular (Polar angle mag) = Rectangular (mag * cos angle) (mag * sin angle)

asPolar :: Complex -> Complex
asPolar x@(Polar _ _) = x
asPolar (Rectangular x y) =
  let r = sqrt (x*x + y*y)
      a = atan (y / x)
  in Polar r a


-------------------------------------------------------------------------------
--                        Pointless Intermission                             --
-------------------------------------------------------------------------------

-- Define equality over complex numbers
instance Eq Complex where
  (Polar r1 a1) == (Polar r2 a2) = r1 == r2 && a1 == a2
  (Rectangular x1 y1) == (Rectangular x2 y2) = x1 == x2 && y1 == y2
  x == y = asPolar x == asPolar y

-- make it bind a little weaker than `.`
infixr 8 ...
(...) = (.) . (.)

{-
rule 1:  f x = g x   -->   f = x    -- eta reduction
rule 2:  f (g x) == (f . x) x       -- definition of composition
rule 3:  (f . g) == ((.) f) g       -- just rule 2 again
def1: ((.) . (.)) = ...

-- pull variables to the right and cancel them out
polar x y = asPolar (asRectangular (Polar x y))
polar x y = (asPolar . asRectangular) ((Polar x) y)             -- rule 2
polar x y = ((asPolar . asRectangular) . Polar x) y             -- rule 2
polar x   = ((asPolar . asRectangular) . Polar x)               -- rule 1
polar x   = ((.) (asPolar . asRectangular)) (Polar x)           -- rule 3
polar x   = (((.) (asPolar . asRectangular)) . Polar) x         -- rule 2
polar     = ((.) (asPolar . asRectangular)) . Polar             -- rule 1
polar     = ((.) ((.) (asPolar . asRectangular))) Polar         -- rule 3
polar     = (((.) . (.)) (asPolar . asRectangular)) Polar       -- rule 2
polar     = ((...) (asPolar . asRectangular)) Polar             -- def 1
polar     = asPolar . asRectangular ... Polar                   -- refactor to infix
-}

polar =  asPolar . asRectangular ... Polar

-- -- The following returns False !!!
-- $ polar 3 1 == Polar 3 1


-------------------------------------------------------------------------------
--                           SICP 2.4.3                                      --
-- Implementtion of complex numbers with typeclasses                         --
-------------------------------------------------------------------------------

-- The type table approach corresponds to Haskell typeclasses
--
--                                   Types
--                 Polar                            Rectangular
--           .-------------------------+---------------------------
-- real-part | real-part-polar         | real-part-rectangular
-- imag-part | imag-part-polar         | imag-part-rectangular
-- magnitude | magnitude-polar         | magnitude-rectangular
-- angle     | angle-polar             | angle-rectangular

class Complex' a where
  realPart  :: a -> Double
  imagPart  :: a -> Double
  magnitude :: a -> Double
  angle     :: a -> Double
  fromXY :: Double -> Double -> a -- convert to a from rectangular coordinates
  fromRA :: Double -> Double -> a -- convert to a from polar coordinates

add'' :: (Complex' a, Complex' b) => a -> b -> b
add'' x y = fromXY (realPart x + realPart y) (imagPart x + imagPart y)

sub'' :: (Complex' a, Complex' b) => a -> b -> b
sub'' x y = fromXY (realPart x - realPart y) (imagPart x - imagPart y)

mul'' :: (Complex' a, Complex' b) => a -> b -> b
mul'' x y = fromRA (magnitude x * magnitude y) (angle x + angle y)

div'' :: (Complex' a, Complex' b) => a -> b -> b
div'' x y = fromRA (magnitude x / magnitude y) (angle x - angle y)


-- add the polar instance
data Polar' = Polar' Double Double deriving (Show, Ord, Eq)
instance Complex' Polar' where
  realPart  (Polar' r a) = r * cos a
  imagPart  (Polar' r a) = r * sin a
  magnitude (Polar' r _) = r
  angle     (Polar' _ a) = a
  fromXY x y = Polar' (magnitude (Rectangular' x y)) (angle (Rectangular' x y))
  fromRA r a = Polar' r a

-- add the rectangular instance
data Rectangular' = Rectangular' Double Double deriving (Show, Ord, Eq)
instance Complex' Rectangular' where
  realPart  (Rectangular' x _) = x
  imagPart  (Rectangular' _ y) = y
  magnitude (Rectangular' x y) = sqrt (x*x + y*y)
  angle     (Rectangular' x y) = atan (y / x)
  fromXY x y = Rectangular' x y
  fromRA r a = Rectangular' (realPart (Polar' r a)) (imagPart (Polar' r a))

-- -- Example:
-- $ add'' (Polar' 3 1) (Rectangular' 5 6)



-------------------------------------------------------------------------------
-- SICP Ex 2.73 (sort of)                                                    --
-- Implementation of derivation and simplification with algebraic types      --
-------------------------------------------------------------------------------

data Expr
  = Var
  | Const Integer      -- who needs real numbers
  | Exponent Expr Expr
  | Product Expr Expr
  | Sum Expr Expr
  deriving(Show)

derivative :: Expr -> Expr
derivative Var = Const 1
derivative (Const a) = Const 0
derivative (Exponent x y) = Exponent (Product (derivative x) (Product y x)) (Sum y (Const (-1)))
derivative (Product x y) = Sum (Product x (derivative y)) (Product y (derivative x))
derivative (Sum x y) = Sum (derivative x) (derivative y)

simplify :: Expr -> Expr
simplify (Sum x y) = case Sum (simplify x) (simplify y) of
  (Sum (Const x) (Const y)) -> Const (x+y)
  (Sum x (Const 0)) -> x
  (Sum (Const 0) x) -> x
  x -> x
simplify (Product x y) = case Product (simplify x) (simplify y) of
  (Product (Const x) (Const y)) -> Const (x*y)
  (Product x (Const 1)) -> x
  (Product (Const 1) x) -> x
  (Product x (Const 0)) -> Const 0
  (Product (Const 0) x) -> Const 0
  x -> x
simplify (Exponent x y) = case Exponent (simplify x) (simplify y) of
  (Exponent (Const x) (Const y)) -> Const (x ^ y)
  (Exponent _ (Const 0)) -> Const 1
  (Exponent x (Const 1)) -> x
  (Exponent (Const 1) _) -> Const 1
  (Exponent (Const 0) _) -> Const 0
  x -> x
simplify x = x

prettyExpr :: Expr -> String
prettyExpr Var = "x"
prettyExpr (Const i) = show i
prettyExpr (Exponent x c) = "(" ++ prettyExpr x ++ ") ^ (" ++ prettyExpr c ++ ")"
prettyExpr (Product x y) = "(" ++ prettyExpr x ++ ") (" ++ prettyExpr y ++ ")"
prettyExpr (Sum x y) = prettyExpr x ++ " + "  ++ prettyExpr y
