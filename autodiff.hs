data Dual = Dual Float Float deriving (Show, Eq, Read)

instance Num Dual where
    Dual a a' + Dual b b' = Dual (a+b) (a'+b')
    Dual a a' - Dual b b' = Dual (a-b) (a'-b')
    Dual a a' * Dual b b' = Dual (a*b) (a'*b + a*b')
    negate (Dual a a') = Dual (-a) (-a')
    abs (Dual a a') = Dual (abs a) (signum a * a')
    signum (Dual a a') = Dual (signum a) 0
    fromInteger i = Dual (fromInteger i) 0

instance Fractional Dual where
    fromRational a = Dual (fromRational a) 0
    Dual a a' / Dual b b' = Dual (a/b) ((a'*b - a*b')/(b**2))
    recip (Dual a a') = Dual (1/a) (a' / (a**2))

instance Floating Dual where
    pi = Dual pi 0
    exp (Dual a a') = Dual (exp a) (exp a * a')
    log (Dual a a') = Dual (log a) (a'/a)
    sqrt (Dual a a') = Dual (sqrt a) (a'/(2 * sqrt a))
    sin (Dual a a') = Dual (sin a) (cos a * a')
    cos (Dual a a') = Dual (cos a) (-sin a * a')
    tan (Dual a a') = Dual (tan a) (a'/(cos a **2))
    asin (Dual a a') = Dual (asin a) (a' / sqrt (1 - a**2))
    acos (Dual a a') = Dual (acos a) (-a' / sqrt (1 - a**2))
    atan (Dual a a') = Dual (atan a) (a' / (1 + a**2))
    sinh (Dual a a') = Dual (sinh a) (cosh a * a')
    cosh (Dual a a') = Dual (cosh a) (sinh a * a')
    tanh (Dual a a') = Dual (tanh a) (a'/(cosh a **2))
    asinh (Dual a a') = Dual (asinh a) (a' / sqrt (1 + a**2))
    acosh (Dual a a') = Dual (acosh a) (a' / sqrt (a**2 - 1))
    atanh (Dual a a') = Dual (atanh a) (a' / (1 - a**2))