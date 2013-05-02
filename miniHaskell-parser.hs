import Data.Char
type Parser symbol result = [symbol] -> [([symbol],result)]

symbolP :: (Eq a) => a -> Parser a a
symbolP s [] = []
symbolP s (x:xs)
	|s==x = [(xs,s)]
	|otherwise = []

tokenP :: (Eq a) => [a] -> Parser a [a]
tokenP s [] = []
tokenP s xs
	|s==(take (length s) xs) = [(drop (length s) xs,s)]
	|otherwise = []     

satisfyP :: (t -> Bool) -> Parser t t
satisfyP p [] = []
satisfyP p (x:xs)
		|p x = [(xs,x)]
		|otherwise = []

succeedP :: t -> Parser a t
succeedP v xs = [(xs,v)]

epsilonP :: Parser a ()
epsilonP = succeedP () 

infixr 6 <*> --Concatenation
infixr 4 <|> --Alternation

(<*>) :: Parser s a -> Parser s b -> Parser s (a,b)
(p1 <*> p2) s = [(xs'',(v1,v2))|(xs',v1) <- p1 s, (xs'',v2) <- p2 xs']

(<|>) :: Parser s a -> Parser s a -> Parser s a
(p1 <|> p2) s = p1 s ++ p2 s
(p1 <||> p2) s|p1 s==[]=p2 s
	      |otherwise=p1 s
(p <@ func) xs = [(x, func v)|(x,v) <- p xs]
-- Action part of an optional parser
                     
p <?@ (failValue,succFunc) = p <@ f
          where  f [] = failValue
                 f [x] = succFunc x

many :: Parser s a -> Parser s [a]
many p = (p <*> many p) <@ (\(x,xs) -> x:xs) <|> epsilonP <@ (\_ -> [])

(<*) :: Parser s a -> Parser s b -> Parser s a
p <* q = (p <*> q) <@ fst

(*>) :: Parser s a -> Parser s b -> Parser s b
p *> q = (p <*> q) <@ snd

(<:*>) :: Parser s a -> Parser s [a] -> Parser s [a]
p <:*> q =  (p <*> q)  <@  list
  where list (x,xs) = x:xs
        
numberP :: Parser Char Integer
numberP = ((digitP <*> many digitP) <@ (\(x,xs) -> x:xs)) <@ (read :: String -> Integer)

digitP :: Parser Char Char
digitP = (satisfyP isDigit) 

alphaP :: Parser Char Char
alphaP = (satisfyP isAlpha) 

floatP :: Parser Char Double
floatP = (wholeP <*> fractionalP) <@ (\(x,y) -> (fromInteger x) + y)

wholeP :: Parser Char Integer
wholeP s = func' (numberP s)

fractionalP :: Parser Char Double
fractionalP s = func' (fractP s)

chainr :: Parser s a -> Parser s (a->a->a) -> Parser s a
chainr p s = (many (p <*> s) <*> p) <@  uncurry (flip (foldr ap1)) 
  where ap1 (x, op) y = x `op` y 
       

fractP :: Parser Char Double
fractP = ((symbolP '.' *> (satisfyP isDigit <*> many (satisfyP isDigit))) <@ (\(x,xs) -> x:xs) <@ ("0." ++) <@ (read :: String -> Double)) <|> failP

failP s = [(s,0.0)]

func' []=[]
func' (x:xs)=[x]
listOf :: Parser s a-> Parser s b -> Parser s [a]
listOf p s = p<:*> many  (s *> p) <|> succeedP []
pack :: Parser s a -> Parser s b -> Parser s c -> Parser s b
pack s1 p s2 = s1 *> p <* s2

paranthesizedP p = pack (symbolP '(') p (symbolP ')')

--sp p = p . dropWhile (==' ')
sp  ::  Parser Char String
sp   =  greedy (satisfyP isSpace)

greedy = determ . many

determ :: Parser a b -> Parser a b
determ p xs  |  null r     =  []
             |  otherwise  =  [head r]
                     where r = p xs

-- optionally parses p; always succeeds

option :: Parser s a -> Parser s [a]
option p  =  p  <@  single <|> 
             succeedP []
               where single x = [x]
-----------------------------------------------------------

--Major Assignment --
type Fname = String
type Var = String
data Program = Prog [Fundef] Exp deriving (Show,Eq)
data Fundef = Fun String [String] Exp deriving (Show,Eq)
data Exp = I Integer | B Bool | V String | Fcall String [Exp] |  Exp `Add` Exp |   Exp `Sub` Exp | Eq  Exp Exp | Cons Exp Exp | Car Exp | Cdr Exp | Null Exp | If Exp Exp Exp | NilList deriving(Read, Show, Eq,Ord)

-- chainx :: Parser s a -> Parser s (a->a->a) -> Parser s a
-- chainx p s = (many (p <*> s) <*> p) <@ uncurry(flip(foldr ap1 ))
--   where ap1 (x, op) y |y==[]=  x `op` NilList 
--                       |otherwise = x `op` y
                                   


-- int parser
iintP :: Parser Char Exp
iintP = sp *> (wholeP) <@ I

--bool parser
bboolP :: Parser Char Exp
bboolP = sp*> ((tokenP "True" <|> tokenP "False") <@ (read::String -> Bool)) <@ B

-- string parser
--identifierP:: Parser Char [Char]
--identifierP = sp*>(alphaP <:*> many (alphaP <|> digitP))

reservedWords = ["if" ,"car","cdr","null", "then","else"]
identifierP p | identifier'' p == [] = []
  | not(snd (head (identifier'' p)) `elem` reservedWords) = identifier'' p
              | otherwise = []
identifier'' = satisfyP isAlpha <:*> identifier' 
identifier' s= [(dropWhile isAlphaNum s,takeWhile isAlphaNum s)]
stringP = identifierP<@ V

allExpP =iintP <||> bboolP

--consP :: Parser Char Exp
--consP = sp*> (chainr (sp *> (expr)) ((sp *> (symbolP ':'))<@ const(Cons)))

consbP = sp*>symbolP '[' *> chainC (sp *> (expr)) (symbolP ',')  <* symbolP ']'

chainC p s = listOf p s  <@ foldr app NilList
  where app x y= Cons x y


addSubP :: Parser Char Exp
addSubP =  chainr (sp *> expr)
        (
          (sp *> (symbolP '+')) <@ const (Add)
          <|> (sp *> (symbolP '-')) <@ const (Sub)
          <|> (sp *> (tokenP "==")) <@ const (Eq))
           
fcallP = (identifierP <*> (symbolP ' ' *>(many (sp *> expr))))<@ (\(x,y)-> Fcall x y)

cdrP= (((tokenP "cdr")<*> sp) *> expr) <@ Cdr
carP= (((tokenP "car") <*> sp) *> expr) <@(\x->Car x)
nullP= (((tokenP "null") <*>sp) *> expr) <@ (\x->Null x)


ifP=((pack (tokenP "if" <*> sp) expr (sp<*>tokenP "then"))<*>(sp*>expr)<*>((sp<*>tokenP "else"<*>sp)*>expr)) <@(\(x,(y,z))->If x y z )
-- parses (p), returns p

parenthesized :: Parser Char a -> Parser Char a
parenthesized p = pack (symbolP '(')  p (symbolP ')')

-- fact : - Int | Bool | string

fact :: Parser Char Exp
fact = iintP <|> bboolP<|>  stringP <|> consbP
       <|> fcallP <|> term
       -- ((identifierP <*> (option (sp*>listOf expr sp))<?@(V,flip Fcall))<@ \(x,f)->f x)
       <|> parenthesized expr
term:: Parser Char Exp
term= cdrP <|> carP <|>nullP <|> ifP 

opP::Parser Char Exp
opP= chainr fact (
  (sp*>symbolP '+'<*sp) <@ const (Add) 
  <|> (sp*>symbolP '-'<*sp) <@ const (Sub)
  <|> (sp*>symbolP ':' <*sp)<@ const (Cons))
fexpr:: Parser Char Exp
fexpr s= frst (expr s) 
        where frst ((x,y):xs) | x == "" = [(x,y)]
                              | head x=='~' =[(x,y)]
                              | otherwise = frst xs
              frst []=[]
expr :: Parser Char Exp
expr = chainr opP (
  (sp*>tokenP "=="<*sp) <@ const (Eq)) <* sp
--term :: Parser Char Exp
u = identifierP <*> (option (sp*>listOf iintP sp))-- <?@(V,flip Fcall))<@ \(x,f)->f x)

fundefP = sp*> ((identifierP <* sp <*> (many (identifierP<*sp)) <* (sp <*> symbolP '=' <*> sp))<*> fexpr ) <@(\((id,args),ex)-> Fun id args ex)


prog = (( many (fundefP <* (sp<*>symbolP '~'<*>sp))) <*> fexpr) <@ \(x,y)-> Prog x y 

prog' s= [ parse | (s,parse) <- prog s , s==""]
parser s| prog' s==[] = error "Parse not possible"
        | otherwise = putStrLn $ ("\n"++show(head(prog' s))++ "\n")