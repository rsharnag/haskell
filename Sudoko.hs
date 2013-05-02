
board1 = [['2','.','.','.','.','1','.','3','8'],
          ['.','.','.','.','.','.','.','.','5'],
          ['.','7','.','.','.','6','.','.','.'],
          ['.','.','.','.','.','.','.','1','3'],
          ['.','9','8','1','.','.','2','5','7'],
          ['3','1','.','.','.','.','8','.','.'],
          ['9','.','.','8','.','.','.','2','.'],
          ['.','5','.','.','6','9','7','8','4'],
          ['4','.','.','2','5','.','.','.','.']] 
type Matrix a= [[a]]
type Board = Matrix Char
boxsize=3
allvals = "123456789"
blank c = c =='.'

--Correct board state
correct :: Board -> Bool
correct b = all nodups (rows b) && 
            all nodups (cols b) &&
            all nodups (boxes b)

rows = id
-- Check for duplicates
nodups [] = True
nodups (x:xs) = x `notElem` xs && nodups xs

-- Transpose of matrix

cols [] = replicate 9 []
cols (x:xs) = zipWith (:) x (cols xs)

-- Boxes
chop [] = []
chop l = (take boxsize l):chop (drop boxsize l) 
unchop = concat
boxes = (map unchop).unchop.(map cols).chop.(map chop)


type Choices = [Char]

initialChoices :: Board -> Matrix Choices

fillin ::Char -> Choices

fillin initialChar | initialChar == '.' = allvals
                   | otherwise = [initialChar]
                                 
initialChoices b = map (map fillin) b


-- Cartesian product
cp :: [[x]]->[[x]]
cp [] =[[]]
cp (x:xs) = [ y:ys | y<-x, ys<-cp xs]

mcp = cp . (map cp )

sudukosolver1 = filter correct.mcp.initialChoices

fixed ::[Choices]-> Choices
fixed = concat. (filter singleton)
singleton [_] = True
singleton _ = False

pruneList :: [Choices] -> [Choices]
-- pruneList t@(x:xs) | singleton x = (x:pruneList xs)
--                 | otherwise =((delete t xs): pruneList xs)
pruneList l = map (cFilter (fixed l)) l 
  where cFilter f x | singleton x = x
                  | otherwise = filter (`notElem` f) x

pruneMatrix = pruneBy rows.pruneBy cols.pruneBy boxes
pruneBy f = f.(map pruneList).f


sudukosolver2=  filter correct.mcp.pruneMatrix.initialChoices

----------------------------------------------------------------------
blocked cm = void  cm || not (safe cm)

void cm = any (any null) cm

safe cm = all (nodups.fixed) (rows cm) &&
          all (nodups.fixed) (cols cm) &&
          all (nodups.fixed) (boxes cm) 

----------------------------------------------------------------------
-- expand 
--expand:: Choices ->[Choices]
expand cm =[rows1++( row1++[c]:row2):rows2 | c<-cs]
  where (rows1,row:rows2)=break (any isCandidate) cm
        (row1,cs:row2)= break isCandidate row
        isCandidate cs= n == length cs
        n = minchoice cm
        minchoice = minimum.filter (>1). map length.concat

search cm | blocked cm = []
          | all singleton cm = [cm]
          | otherwise = (concat.map (search.pruneMatrix).expand) cm
                        
sudukosolver3= map (map head).head.search.initialChoices