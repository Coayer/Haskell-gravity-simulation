import Graphics.Gloss
import System.Random
import System.IO.Unsafe


--Change the values below to create new simulations:

playback_rate = 1.0 --Controls simulation speed eg. 0.5 = half-speed
mass_range = (10,20) --When compiling, the program will select a mass for the dots within the range of these two values; change along with playback_rate to get a good speed
black_holes = [((0,0), (x,y), 10000) | x <- [200,-200], y <- [200,-200]] --Some 'black holes' to add to the starting state of the simulation, see below for usage

--Starting state of simulation; can combine multiple sets of dots (watch out for performance) eg. initial = generateDisc 5 ++ generateRectangle 4 ++ black_holes
--The number refers to the density of dots placed
initial = generateDisc 6 

--Used to determine the limits for where to place dots -- I tended not to change these
wX = 250 
wY = 250 

---------------------------------------------------------Don't change below---------------------------------------------------------


--((x velocity, y velocity), (x coord, y coord), mass)
type Dot = ((Float, Float), (Float, Float), Float)
type Model = [Dot]


--Used to generate a rectangle of dots
generateRectangle :: Int -> Model 
generateRectangle n = [((0, 0), (fromIntegral posX, fromIntegral posY), unsafePerformIO $ randomRIO mass_range) | posX <- [-wX,-wX + (wX `div` n)..wX], posY <- [-wY,-wY + (wY `div` n)..wY]]

--Used to generate a circle of dots
generateDisc :: Int -> Model 
generateDisc n = [((0, 0), (fromIntegral posX, fromIntegral posY), unsafePerformIO $ randomRIO mass_range) | posX <- [-wX,-wX + (wX `div` n)..wX], posY <- [-wY,-wY + (wY `div` n)..wY], inRadius (posX, posY)]
    where
        inRadius (x, y) = round (sqrt ((fromIntegral x)**2 + (fromIntegral y)**2)) <= wX


--Runs simulation
main = simulate (InWindow "Window" window (0, 0)) black 30 initial draw update
    where
        window = (wX*8, wY*8)


--Draws each dot on screen
draw :: Model -> Picture
draw model = Pictures [translate x y $ color (makeColor (toPos vX) (toPos vY) mass 1) $ circleSolid 2 | ((vX, vY), (x, y), mass) <- model]
    where 
        toPos val = if val < 0 then ratio (val * (-1)) else ratio val
        ratio val = val / 100


--Updates the current state of the simulation
update vp dt model = [updateDot (model !! n) (dt * playback_rate) model | n <- [0..((length model)-1)]]

updateDot :: Dot -> Float -> Model -> Dot
updateDot ((vX, vY), (x, y), mass) dt model = ((v vX aX dt, v vY aY dt), (newX, newY), mass)
    where
        newY = max (fromIntegral (-maxY)) (min (fromIntegral maxY) (y + s vY aY dt))
        newX = max (fromIntegral (-maxX)) (min (fromIntegral maxX) (x + s vX aX dt))
        (fX, fY) = (sum $ map fst forces, sum $ map snd forces)
        (aX, aY) = (fX/mass, fY/mass)
        forces = [forceOnDot (x, y) mass otherDot | otherDot <- model]
        s u a t = u * t + 0.5 * a * (t ** 2)
        v u a t = u + a * t
        maxX = wX * 8
        maxY = wY * 8


--Calculates the x and y components of the force experienced by each dot
forceOnDot :: (Float, Float) -> Float -> Dot -> (Float, Float)
forceOnDot pos1 m1 (_, pos2, m2)
    | pos1 == pos2 = (0.0, 0.0) 
    | fst pos1 > fst pos2 && snd pos1 > snd pos2 = (f * cos(theta), f * sin(theta))
    | fst pos1 < fst pos2 && snd pos1 > snd pos2 = ((-f) * cos(theta), (-f) * sin(theta))
    | fst pos1 > fst pos2 = (f * cos(theta), f * sin(theta))
    | snd pos1 > snd pos2 = (f * cos(theta), (-f) * sin(theta))
    | otherwise = ((-f) * cos(theta), (-f) * sin(theta))
    
    where 
        f = -(6.7 * m1 * m2)/(dist pos1 pos2)
        theta = angleBetweenDots pos1 pos2
        dist (x1, y1) (x2, y2) = sqrt ((x2 - x1)**2 + (y2 - y1)**2) 
        angleBetweenDots (x1, y1) (x2, y2) = atan ((y2 - y1) / (x2 - x1))
