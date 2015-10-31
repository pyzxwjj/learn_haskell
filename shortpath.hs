data Section = Section {getA :: Int, getB :: Int, getC :: Int}
type RoadSystem = [Section]
myroad :: RoadSystem
myroad = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]
data Label = A|B|C deriving (Show)
type Path = [(Label, Int)]
roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let priceA = sum $ map snd pathA
        priceB = sum $ map snd pathB
        priceAToA = priceA + a
        priceAToB = priceA + a + c
        priceBToA = priceB + b + c
        priceBToB = priceB + b
        newPathA = if priceAToA <= priceBToA then (A,a):pathA else (C,c):(B,b):pathB
        newPathB = if priceBToB <= priceAToB then (B,b):pathB else (C,c):(A,a):pathA
    in  (newPathA, newPathB)

shortPath :: RoadSystem -> Path
shortPath road =
    let (bestA, bestB) = foldl roadStep ([], []) road  
    in if sum (map snd bestA) <= sum (map snd bestB) then reverse bestA else reverse bestB
