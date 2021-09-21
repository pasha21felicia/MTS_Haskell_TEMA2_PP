{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List
import Data.Maybe

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)


{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}
data Game = Game {
    rows :: Int, 
    cols :: Int, 
    myHunter :: Position, 
    myTargets :: [Target], 
    myGateways :: [(Position, Position)], 
    myObstacles :: [Position]
    } deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}

findtargets :: [Target] -> [Position]
findtargets target = foldl (\acc x -> acc ++ [(position x)]) [] target

allGateways :: [(Position , Position)] -> [Position]
allGateways gateway = foldl (\acc x -> acc ++ [fst x] ++ [snd x]) [] gateway

gameAsString :: Game -> String
gameAsString (Game rows cols myHunter myTargets myGateways myObstacles) = let 
    allCoordinates = [ (x,y) | x <- [0..rows-1], y <- [0..cols-1]]
    in 
        init $ foldl (\acc pos ->   if (snd pos == cols-1) 
                                        then acc ++ "@" ++ "\n"
                                    else if (elem pos myObstacles)
                                        then acc ++ "@"
                                    -- else if (elem pos (findtargets myTargets) && elem pos (allGateways myGateways)
                                    --     then acc ++ "*"
                                   
                                    else if (pos == myHunter)
                                        then acc ++ "!"
                                    else if (elem pos (findtargets myTargets))
                                        then acc ++ "*"
                                    else if (elem pos (allGateways myGateways))
                                        then acc ++ "#"
                                    else acc ++ " ") "" allCoordinates




instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame rows cols = Game rows cols (1,1) [] [] obs
    where
        obs = [(0,x) | x <- [0..cols-1]] ++ [(rows-1,x) | x <- [0..cols-1]] ++ [(x, 0) | x <- [0..rows-1]] ++ [(x, cols-1) | x <- [0..rows-1]]

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
addHunter :: Position -> Game -> Game
addHunter pos (Game rows cols myHunter myTargets myGateways myObstacles) = 
    let 
        alternativeDest = fromMaybe (-1, -1) (attemptMove pos (Game rows cols myHunter myTargets myGateways myObstacles))
    in
        if (elem pos myObstacles) || (pos == myHunter) 
            then (Game rows cols myHunter myTargets myGateways myObstacles)
        else if ((fst pos) < 0 || (fst pos) > rows-1 || (snd pos) < 0 || (snd pos) > cols-1) 
            then (Game rows cols myHunter myTargets myGateways myObstacles)
        else if (elem pos (allGateways myGateways))
            then (Game rows cols alternativeDest myTargets myGateways myObstacles)
        else (Game rows cols pos myTargets myGateways myObstacles)

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}


addTarget :: Behavior -> Position -> Game -> Game
addTarget behavior pos (Game rows cols myHunter myTargets myGateways myObstacles) = 
    let newTargets = myTargets ++ [(Target pos behavior)]
    in (Game rows cols myHunter newTargets myGateways myObstacles)

{-
    *** TODO ***

    Primește o pair de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway  (start, end) (Game rows cols myHunter myTargets myGateways myObstacles) = 
    let newGateways = myGateways ++ [(start, end)]
    in (Game rows cols myHunter myTargets newGateways myObstacles)

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle pos (Game rows cols myHunter myTargets myGateways myObstacles) = 
    let newObstacles = myObstacles ++ [pos]
    in (Game rows cols myHunter myTargets myGateways newObstacles)

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pair;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}

-- allGateways :: [(Position , Position)] -> [Position]
-- allGateways gateway = foldl (\acc x -> acc ++ [fst x] ++ [snd x]) [] gateway

checkGateways :: Position -> [(Position , Position)] -> Position
checkGateways pos gateway = foldl (\acc x -> if (pos == (fst x))
                                                then (snd x)
                                            else if (pos == (snd x))
                                                then (fst x)
                                            else (1, 1)) (1, 1) gateway

attemptMove :: Position -> Game -> Maybe Position
attemptMove pos (Game rows cols myHunter myTargets myGateways myObstacles) = 
        if (elem pos myObstacles) 
            then Nothing
        else if (not (elem pos (allGateways myGateways)) && not(elem pos (findtargets myTargets))) 
            then Just pos -- spatiu gol
        else if (elem pos (allGateways myGateways))
            then Just (checkGateways pos myGateways) --pozitia la gateway
        else Nothing

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.

-}

goGeneral :: Position -> Position -> Game  -> Behavior -> Target
goGeneral sourse dest game behavior = 
    let 
        alternativeDest = fromMaybe (-1, -1) (attemptMove dest game)
        alternativeSourse = fromMaybe (-1, -1) (attemptMove sourse game)
    in
        if (elem dest (myObstacles game) && elem sourse (allGateways $ myGateways game))
            then (Target alternativeSourse behavior)
        else if (isNothing (attemptMove dest game))
            then (Target sourse behavior)
        else if (elem dest (allGateways $ myGateways game) && isJust (attemptMove dest game))
            then (Target alternativeDest behavior)
        else (Target dest behavior)

goEast :: Behavior
goEast pos game = (goGeneral pos (fst pos, snd pos + 1) game goEast)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se Target pos goSouth)află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest pos game = (goGeneral pos (fst pos, snd pos - 1) game goWest)


{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}

goNorth :: Behavior
goNorth pos game = (goGeneral pos (fst pos - 1, snd pos) game goNorth)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}




goSouth :: Behavior
goSouth pos game = 
    (goGeneral pos (fst pos + 1, snd pos) game goSouth)

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pair conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}

bounce :: Int -> Behavior
bounce direction pos game = 
    let 
        posNorth = (fst pos - direction, snd pos)
        posSouth = (fst pos + direction, snd pos)
        nextGate = fromMaybe (-1, -1) (attemptMove posSouth game)
    in 
        if (isJust (attemptMove posSouth game) && (notElem posSouth (allGateways $ myGateways game)))
            then (Target posSouth (bounce direction))
        else if (elem posSouth (allGateways $ myGateways game)) && (notElem ((fst posSouth) + 1, snd posSouth) (myObstacles game))
            then (Target posSouth (bounce direction))
        else if ((elem ((fst posSouth) + 1, snd posSouth)  (myObstacles game) )
                && (elem posSouth (allGateways $ myGateways game)))
            then (Target nextGate (bounce direction))
        else if (elem posSouth (myObstacles game))
            then (Target posNorth (bounce (negate direction)))
        else (Target pos (bounce direction))


{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}

moveTargets :: Game -> Game
moveTargets g = 
    let change = foldl (\acc x -> acc ++ [(behavior x) (position x) g]) [] (myTargets g)
        in 
            (Game (rows g) (cols g) (myHunter g) change (myGateways g) (myObstacles g))
    
{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled posHunter target = 
    let 
        targetNorth = (fst posHunter - 1, snd posHunter)
        targetSouth = (fst posHunter + 1, snd posHunter)
        targetEast = (fst posHunter, snd posHunter + 1)
        targetWest = (fst posHunter, snd posHunter - 1)
    in
        if (targetNorth == (position target) || (targetSouth == (position target)) || 
            (targetEast == (position target)) || (targetWest == (position target)))
            then True
        else False


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}

pairGatewayFirst :: [(Position, Position)] -> Position -> Position
pairGatewayFirst gateway pos = head [snd pair | pair <- gateway, fst pair == pos] 

        
pairGatewaySecond ::  [(Position, Position)] -> Position -> Position
pairGatewaySecond gateway pos = head [fst pair | pair <- gateway, snd pair == pos]

checkTargets :: [Target] -> Position -> [Target]
checkTargets targets pos = foldl (\acc x -> if ((isTargetKilled pos x) == False) then acc ++ [x] else acc) [] targets

helperAdvanceGameState :: Position -> Direction -> Game -> Game
helperAdvanceGameState posHunter direction game = 
    let 
        afterKilling = (checkTargets (myTargets game) posHunter)
        newposWest = (fst posHunter, snd posHunter - 1)
        newPosEast = (fst posHunter, snd posHunter + 1)
        newPosNorth = (fst posHunter - 1, snd posHunter)
        newPosSouth = (fst posHunter + 1, snd posHunter)
    
        listLeft =  [fst pair | pair <- (myGateways game)]
        listRight =  [snd pair | pair <- (myGateways game)]

    in
    if direction == North then
        if (elem posHunter listLeft) && (elem newPosNorth (myObstacles game))
            then (Game (rows game) (cols game) (pairGatewayFirst (myGateways game) posHunter)  afterKilling (myGateways game) (myObstacles game))
        else if (elem posHunter listRight) && (elem newPosNorth (myObstacles game))
            then (Game (rows game) (cols game) (pairGatewaySecond (myGateways game) posHunter)  afterKilling (myGateways game) (myObstacles game))
        else (Game (rows game) (cols game) posHunter afterKilling (myGateways game) (myObstacles game))
    else if direction == South then
        if (elem posHunter listLeft) && (elem newPosSouth (myObstacles game))
            then (Game (rows game) (cols game) (pairGatewayFirst (myGateways game) posHunter)  afterKilling (myGateways game) (myObstacles game))
        else if (elem posHunter listRight) && (elem newPosSouth (myObstacles game))
            then (Game (rows game) (cols game) (pairGatewaySecond (myGateways game) posHunter)  afterKilling (myGateways game) (myObstacles game))
        else (Game (rows game) (cols game) posHunter afterKilling (myGateways game) (myObstacles game))
    else if direction == West then 
        if (elem posHunter listLeft) && (elem newposWest (myObstacles game))
            then (Game (rows game) (cols game) (pairGatewayFirst (myGateways game) posHunter)  afterKilling (myGateways game) (myObstacles game))
        else if (elem posHunter listRight) && (elem newposWest (myObstacles game))
            then (Game (rows game) (cols game) (pairGatewaySecond (myGateways game) posHunter)  afterKilling (myGateways game) (myObstacles game))
        else (Game (rows game) (cols game) posHunter afterKilling (myGateways game) (myObstacles game))
    else
        if (elem posHunter listLeft) && (elem newPosEast (myObstacles game)) 
            then (Game (rows game) (cols game) (pairGatewayFirst (myGateways game) posHunter)  afterKilling (myGateways game) (myObstacles game))
        else if (elem posHunter listRight) && (elem newPosEast (myObstacles game))
            then (Game (rows game) (cols game) (pairGatewaySecond (myGateways game) posHunter)  afterKilling (myGateways game) (myObstacles game))
        else (Game (rows game) (cols game) posHunter afterKilling (myGateways game) (myObstacles game))
    
        


advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState direction value game = 
    let 
        posNorth = (fst (myHunter game) - 1, snd (myHunter game))
        posSouth = (fst (myHunter game) + 1, snd (myHunter game))
        posWest = (fst (myHunter game), snd (myHunter game) - 1)
        posEast = (fst (myHunter game), snd (myHunter game) + 1)
        
        newPosNorth = (fst posNorth - 1, snd posNorth)
        newPosSouth = (fst posSouth + 1, snd posSouth)
        newposWest = (fst posWest, snd posWest - 1)
        newPosEast = (fst posEast, snd posEast + 1)
        
        listLeft =  [fst pair | pair <- (myGateways game)]
        listRight =  [snd pair | pair <- (myGateways game)]
    in
    if (value == True && direction == North) then
        if isJust (attemptMove posNorth game) 
            then helperAdvanceGameState posNorth North (moveTargets (helperAdvanceGameState posNorth North game))
            else helperAdvanceGameState (myHunter game) North (moveTargets (helperAdvanceGameState (myHunter game) North game))
    else if (value == True && direction == South) then
        if isJust (attemptMove posSouth game) 
            then helperAdvanceGameState posSouth South (moveTargets (helperAdvanceGameState posSouth South game))
            else helperAdvanceGameState (myHunter game) South (moveTargets (helperAdvanceGameState (myHunter game) South game))
    else if (value == True && direction == West) then
        if (isJust (attemptMove posWest game)) 
            then helperAdvanceGameState posWest West (moveTargets (helperAdvanceGameState posWest West game))
            else helperAdvanceGameState (myHunter game) West (moveTargets (helperAdvanceGameState (myHunter game) West game))
    else if (value == True && direction == East) then
        if isJust (attemptMove posEast game) 
            then helperAdvanceGameState posEast East (moveTargets (helperAdvanceGameState posEast East game))
            else helperAdvanceGameState (myHunter game) East (moveTargets (helperAdvanceGameState (myHunter game) East game))
    else if (value == False && direction == North) then
        if elem posNorth listLeft
            then (Game (rows game) (cols game) (pairGatewayFirst (myGateways game) posNorth) (myTargets game) (myGateways game) (myObstacles game))
        else if elem posNorth listRight
            then (Game (rows game) (cols game)  (pairGatewaySecond(myGateways game) posNorth) (myTargets game) (myGateways game) (myObstacles game)) 
        else if notElem posNorth (myObstacles game) 
            then (Game (rows game) (cols game) posNorth (myTargets game) (myGateways game) (myObstacles game))
        else game
    else if (value == False && direction == South) then
        if elem posSouth listLeft
            then (Game (rows game) (cols game) (pairGatewayFirst (myGateways game) posSouth) (myTargets game) (myGateways game) (myObstacles game))
        else if elem posSouth listRight
            then (Game (rows game) (cols game) (pairGatewaySecond(myGateways game) posSouth) (myTargets game) (myGateways game) (myObstacles game)) 
        else if notElem posSouth (myObstacles game) 
            then (Game (rows game) (cols game) posSouth (myTargets game) (myGateways game) (myObstacles game))
        else game
    else if (value == False && direction == West) then
        if elem posWest listLeft 
            then (Game (rows game) (cols game)  (pairGatewayFirst (myGateways game) posWest) (myTargets game) (myGateways game) (myObstacles game))
        else if elem posWest listRight
            then (Game (rows game) (cols game) (pairGatewaySecond(myGateways game) posSouth) (myTargets game) (myGateways game) (myObstacles game)) 
        else if notElem posWest (myObstacles game) 
            then (Game (rows game) (cols game) posWest (myTargets game) (myGateways game) (myObstacles game))
        else game
    else 
        if elem posEast listLeft
            then (Game (rows game) (cols game)  (pairGatewayFirst (myGateways game) posEast) (myTargets game) (myGateways game) (myObstacles game))
        else if elem posEast listRight
            then (Game (rows game) (cols game) (pairGatewaySecond(myGateways game) posEast) (myTargets game) (myGateways game) (myObstacles game)) 
        else if notElem posEast (myObstacles game) 
            then (Game (rows game) (cols game) posEast (myTargets game) (myGateways game) (myObstacles game))
        else game



{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft game = 
    if (myTargets game) == [] 
        then True
        else False

    -- if (length (myTargets g) == 0)
    --     then False -- nu exista
    -- else True --exista


{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined




instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game =
        if (isGoal game == True) 
            then [] 
            else zip [North, South, East, West] [(advanceGameState North False game), (advanceGameState South False game), 
                                                    (advanceGameState East False game), (advanceGameState West False game)]

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal game  =  
        let posTargets = checkTargets (myTargets game) (myHunter game)
        in 
            if (length posTargets /= (length $ myTargets game)) then True
            else False
        

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h game = 
        let targetel = head(foldl (\acc x -> if ((isTargetKilled (myHunter game) x) == True) then acc ++ [x] else acc) [] (myTargets game))
        in
        hEuclidean (myHunter game) (position targetel)

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
