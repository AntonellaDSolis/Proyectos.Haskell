type Polinomio = [Float]
type Monomio = (Float, Int)

crearPolinomio :: [Float] -> Polinomio
crearPolinomio (0:xs) = crearPolinomio xs
crearPolinomio l = l

grado :: Polinomio -> Int
grado [] = undefined
grado [x] = 0
grado (x:xs) = 1 + grado xs

evaluar :: Polinomio -> Float -> Float
evaluar [] _ = 0
evaluar (c:p) a = c * (a ^ (grado (c:p))) + evaluar p a

productoPorMonomio :: Monomio -> Polinomio -> Polinomio
productoPorMonomio (_,n) [] = replicate n 0 {- Devuelve una lista de n ceros -}
productoPorMonomio (a,n) (c:p) = (a * c) : (productoPorMonomio (a,n) p)

suma :: Polinomio -> Polinomio -> Polinomio
suma [] p2 = p2
suma p1 [] = p1
suma p1 p2 = (suma (init p1) (init p2)) ++ [(last p1) + (last p2)]

producto :: Polinomio -> Polinomio -> Polinomio
producto [] p = []
producto (c:ps) p = suma (productoPorMonomio (c,grado (c:ps)) p) (producto ps p)

evaluacionMultiple :: [Float] -> Polinomio -> Polinomio -> [Float]
evaluacionMultiple [] p q = []
evaluacionMultiple (i:is) p q | mod (length is) 2 == 0 = evaluar p i : siguiente
                              | otherwise = (evaluar q i) : siguiente
                              where siguiente = evaluacionMultiple is p q