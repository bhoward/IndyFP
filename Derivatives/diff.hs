f :: Double -> Double
f x = m 4 where
  m 1 = x
  m n = let mn1 = m (n - 1)
        in 4 * mn1 * (1 - mn1)

f' :: Double -> Double
f' x = let eps = 0.000001
       in (f (x + eps) - f x) / eps

-- >>> f' 0.25
-- 7.999951999848598
--

-- Based on http://www.danielbrice.net/blog/automatic-differentiation-is-trivial-in-haskell/
data Dual = Dual Double Double deriving (Read, Show, Eq)

instance Num Dual where
    (+) (Dual a1 b1) (Dual a2 b2) = Dual (a1 + a2) (b1 + b2)
    (-) (Dual a1 b1) (Dual a2 b2) = Dual (a1 - a2) (b1 - b2)
    (*) (Dual a1 b1) (Dual a2 b2) = Dual (a1 * a2) (a2 * b1 + a1 * b2)
    negate (Dual a b) = Dual (negate a) (negate b)
    abs (Dual a b)    = Dual (abs a) ((signum a) * b)
    signum (Dual a _) = Dual (signum a) 0
    fromInteger n     = Dual (fromInteger n) 0
  
instance Fractional Dual where
    (/) (Dual a1 b1) (Dual a2 b2) = Dual (a1 / a2) ((a2 * b1 - a1 * b2) / a2 ** 2)
    recip (Dual a b)              = Dual (recip a) (- b / a ** 2)
    fromRational n                = Dual (fromRational n) 0

instance Floating Dual where
    pi                = Dual pi 0
    exp (Dual a b)   = Dual (exp a) (b * exp a)
    sqrt (Dual a b)  = Dual (sqrt a) (b / (2 * sqrt a))
    log (Dual a b)   = Dual (log a) (b / a)
    sin (Dual a b)   = Dual (sin a) (b * cos a)
    cos (Dual a b)   = Dual (cos a) (- b * sin a)
    tan (Dual a b)   = Dual (tan a) (b / ((cos a) ** 2))
    asin (Dual a b)  = Dual (asin a) (b / (sqrt(1 - a ** 2)))
    acos (Dual a b)  = Dual (acos a) (- b / (sqrt(1 - a ** 2)))
    atan (Dual a b)  = Dual (atan a) (b / (1 + a ** 2))
    sinh (Dual a b)  = Dual (sinh a) (b * cosh a)
    cosh (Dual a b)  = Dual (cosh a) (b * sinh a)
    tanh (Dual a b)  = Dual (tanh a) (b * (1 - (tanh a) ** 2))
    asinh (Dual a b) = Dual (asinh a) (b / (sqrt(1 + a ** 2)))
    acosh (Dual a b) = Dual (acosh a) (b / (sqrt(a ** 2 - 1)))
    atanh (Dual a b) = Dual (atanh a) (b / (1 - a ** 2))
    (**) (Dual a1 b1) (Dual a2 b2)
        = Dual (a1 ** a2) (a1 ** a2 * (b2 * (log a1) + (a2 * b1 / a1)))
    logBase (Dual a1 b1) (Dual a2 b2)
        = Dual (logBase a1 a2) (((log a2) * b1 / a1 - (log a1) * b2 / a2) / ((log a1) ** 2))

g :: Dual -> Dual
g x = m 4 where
  m 1 = x
  m n = let mn1 = m (n - 1)
        in 4 * mn1 * (1 - mn1)

-- >>> g (Dual 0.25 1)
-- Dual 0.75 8.0
--

        