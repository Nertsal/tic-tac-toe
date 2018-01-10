import Graphics.Gloss.Interface.Pure.Game

data Cell = E | X | O deriving (Eq)

instance Show Cell where
	show E = " "
	show X = "X"
	show O = "O"

data Game = GameTurn Board Cell Coords | Game Board Cell State

data State = Finished | NotFinished deriving (Eq)

type Coords = (Int, Int)

type CoordsF = (Float, Float)

type Board = [[Cell]]

emptyBoard =
	[ [E, E, E]
	, [E, E, E]
	, [E, E, E]]

sampleBoard1 =
	[ [X, X, O]
	, [X, O, E]
	, [X, O, O]]

sampleBoard2 =
	[ [X, X, O]
	, [E, O, X]
	, [O, X, O]]

sampleBoard3 =
	[ [X, E, E]
	, [E, O, E]
	, [E, E, E]]

checkRows :: Board -> State
checkRows [] = Finished
checkRows (row : rows)
	| checkRow row == NotFinished = NotFinished
	| otherwise 			  = checkRows rows

checkRow :: [Cell] -> State
checkRow [] = Finished
checkRow (cell : cells)
	| cell == E = NotFinished
	| otherwise = checkRow cells

move :: Game -> Game
move (GameTurn rows turn (x, y)) = Game newRows newTurn state
	where
		f :: Cell -> Cell
		f E = turn
		f c = c

		newRows = update y (update x f) rows

		state
			| winner /= E = Finished
			| otherwise   = checkRows newRows

		newTurn
			| checkRows newRows	== Finished 	   = E
			| rows == newRows || state == Finished = turn
			| otherwise      					   = nextTurn turn

		winner = win $ boardLines newRows

update :: Int -> (a -> a) -> [a] -> [a]
update _ _ [] = []
update i f (x : xs)
	| i < 0 = x : xs
	| i == 0 = f x : xs
	| otherwise = x : update (i - 1) f xs

win :: [[Cell]] -> Cell
win [] = E
win (xs @ (x : _) : xss)
	| lineWin xs x == E = win xss
	| otherwise 	    = lineWin xs x

lineWin :: [Cell] -> Cell -> Cell
lineWin [] cell = cell
lineWin (x : xs) cell
	| cell == E = E
	| x == cell = lineWin xs x
	| otherwise = E

boardLines :: Board -> [[Cell]]
boardLines board = rows board ++ columns board ++ [diagonal board] ++ [diagonal (reverse board)]
	where
		rows [] = []
		rows (xs : xss) = xs : rows xss

		columns [] = []
		columns ((x : xs) : []) = [x] : columns [xs]
		columns (xs : xss) = map f (zip xs (columns xss))
			where			
				f (a, b) = a : b

		diagonal [] = []
		diagonal (x : []) = x
		diagonal ((x : xs) : xss) = x : (diagonal a)
			where
				(_ : a) = columns xss

gameTurn :: Game -> Game
gameTurn game = move game

nextTurn :: Cell -> Cell
nextTurn X = O
nextTurn O = X
nextTurn E = X

printBoard :: Board -> [String]
printBoard rows = printRows (map printRow rows) (length rows)
	where
		printRow :: [Cell] -> String
		printRow row = printCells (map printCell row)

		printCell :: Cell -> String
		printCell cell = show cell

		printCells :: [String] -> String
		printCells (cell : []) = cell
		printCells (cell : cells) = cell ++ "|" ++ printCells cells

		printRows :: [String] -> Int -> [String]
		printRows (row : []) _ = [row]
		printRows (row : rows) c = row : printMinuses c : printRows rows c

		printMinuses :: Int -> String
		printMinuses 1 = "-"
		printMinuses c = "-+" ++ printMinuses (c - 1)

playerPrompt :: Cell -> IO ()
playerPrompt E = putStr "> "
playerPrompt turn = putStr (show turn ++ "> ")

printWin :: Cell -> IO ()
printWin winPos
	| winPos == X = putStrLn "X win!"
	| winPos == O = putStrLn "O win!"

run :: IO ()
run = play display bgColor fps initialWorld drawWorld handleWorld updateWorld
	where
		display :: Display
		display = InWindow "Tictactoe" (1000, 900) (0, 0)
		bgColor :: Color
		bgColor = makeColor 0 0 0 0
		fps :: Int
		fps = 60
		initialWorld :: Game
		initialWorld = Game emptyBoard X NotFinished
		updateWorld :: Float -> Game -> Game
		updateWorld _ game = game

drawWorld :: Game -> Picture
drawWorld game @ (Game board turn state) 
	= (changePicture $ pictures 
		[pictureBoard boardPaths, 
		 drawCells board 0,
		 drawTurn game])

handleWorld :: Event -> Game -> Game
handleWorld (EventKey (MouseButton RightButton) Down _ _) (Game _ _ Finished)
	= Game emptyBoard X NotFinished
handleWorld _ (game @ (Game _ _ Finished)) = game
handleWorld (EventKey (MouseButton LeftButton) Down _ mouse) (Game board turn _)
	= move (GameTurn board turn (findCursorCell mouse))
handleWorld _ game = game

findCursorCell :: CoordsF -> Coords
findCursorCell xy
	| check (-150, 150) (-50, 50) xy   = (0, 0)
	| check (-50, 150) (50, 50) xy     = (1, 0)
	| check (50, 150) (150, 50) xy     = (2, 0)
	| check (-150, 50) (-50, -50) xy   = (0, 1)
	| check (-50, 50) (50, -50) xy     = (1, 1)
	| check (50, 50) (150, -50) xy     = (2, 1)
	| check (-150, -50) (-50, -150) xy = (0, 2)
	| check (-50, -50) (50, -150) xy   = (1, 2)
	| check (50, -50) (150, -150) xy   = (2, 2)
	| otherwise 					   = (3, 3)

check :: CoordsF -> CoordsF -> CoordsF -> Bool
check (cX, cY) (cXm, cYm) (x, y) = x > cX && y < cY && x < cXm && y > cYm

changePicture :: Picture -> Picture
changePicture picture = color white picture

pictureBoard :: [Path] -> Picture
pictureBoard [] = blank
pictureBoard (path : paths) = pictures [line path, pictureBoard paths]

boardPaths :: [Path]
boardPaths = 
	[[(-150, 50), (150, 50)],
	 [(-150, -50), (150, -50)],
	 [(50, 150), (50, -150)],
	 [(-50, 150), (-50, -150)]] 

drawCells :: Board -> Int -> Picture
drawCells [] _ = blank
drawCells (row : rows) index = pictures [drawRow row (index * 3 + 1), drawCells rows (index + 1)]

drawRow :: [Cell] -> Int -> Picture
drawRow [] _ = blank
drawRow (cell : cells) index = pictures [setCell index $ drawCell cell, drawRow cells (index + 1)]

drawCell :: Cell -> Picture
drawCell X = pictureBoard pathX
drawCell O = pathO
drawCell E = blank

setCell :: Int -> Picture -> Picture
setCell 1 p = translate (-100) 100 p
setCell 2 p = translate 0 100 p
setCell 3 p = translate 100 100 p
setCell 4 p = translate (-100) 0 p
setCell 5 p = p
setCell 6 p = translate 100 0 p
setCell 7 p = translate (-100) (-100) p
setCell 8 p = translate 0 (-100) p
setCell 9 p = translate 100 (-100) p

pathX :: [Path]
pathX = [[(-40, 40), (40, -40)], [(-40, -40), (40, 40)]]

pathO :: Picture
pathO = circle 45

drawTurn :: Game -> Picture
drawTurn (Game _ E Finished)
	= scale 0.5 0.5 $ translate (-100) (-500) $ text "Draw!"
drawTurn (Game _ turn Finished)
	= scale 0.5 0.5 $ translate (-100) (-500) $ text (show turn ++ " win!")
drawTurn (Game _ turn NotFinished) 
	= scale 0.5 0.5 $ translate (-200) (-500) $ text (show turn ++ "'s turn")