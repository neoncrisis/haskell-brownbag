This presentation is valid Haskell file
=======================================
```haskell
> import Control.Applicative ((<|>))
> import Data.Aeson
> import Data.ByteString.Lazy (ByteString)
> import Data.Char (toUpper, isDigit)
> import Data.Text (Text)
> import Data.List (sort)
> import Data.Time (Day, addDays)
> import GHC.Generics (Generic)
> import Test.QuickCheck
> import Test.QuickCheck.Arbitrary.Generic
> import Text.ParserCombinators.ReadP

> poem = unlines
>  [ "occasional clouds"
>  , "one gets a rest"
>  , "from moon-viewing"
>  ]
```

--------------------------
What I value about Haskell
==========================

- Sophisticated and expressive type system
- Correctness by construction
- Easy to maintain
- Highly composable
- Explicit side-effects
- Errors pushed to program boundaries
- Constant experimentation and research


--------------------------
People's beef with Haskell
==========================

- No mutable state
- Lazy evaluation
- Don't have time to "get a PhD", hurr hurr
- Dynamic languages are better, duh!
- Compiled languages aren't cool anymore
- Ew! Types are icky!
- Looks like a crazy moon-language
- **Monads**? that's a stupid word...


--------------
Haskell is Rad
==============

- Pure
- Immutable
- Composable
- It's just functions all the way down
- Lazy evaluation
- Strongly typed (with inference)
- Terse but expressive syntax
- Correct by construction guarantees
- **Monads**! actually pretty cool


-----------------------------
You've seen this stuff before
=============================

Sorting the lines in a file
`$ cat poem | sort`

Reversing the lines in a file and getting the first
`$ cat poem | rev | head`

Convert the file to all caps
`$ cat poem | tr a-z A-Z | sed -e 's/$/!!!/'`

**What do these commands do?**

- operate on received input ( **pure** )
- transform a copy of the input ( **immutable** )
- stream output as soon as they are able ( **lazy** )
- chain together to create larger effects ( **composable** )


--------------
Now in Haskell
==============
```haskell
> main = readFile "poem" >>= putStr . process
>   where
>     process t = unlines (sort (lines t))
```

**Original poem:**

occasional clouds
one gets a rest
from moon-viewing

**Program output:**

from moon-viewing
occasional clouds
one gets a rest


------
Maths!
======
```haskell
process t = unlines (sort (lines t))
```
Remember **f(g(x)) = (f⋅g)(x)** from high school algebra?
```haskell
process t = (unlines . sort . lines) t
```
And simplificiation works here too:
```haskell
process = unlines . sort . lines
```

----------------------
Other common functions
======================
```haskell
sortLines     = unlines . sort    . lines
reverseLines  = unlines . reverse . lines
firstTwoLines = unlines . take 2  . lines
```
Notice a pattern?  Let's factor it out!
```haskell
> byLines f = unlines . f . lines

> sortLines     = byLines sort
> reverseLines  = byLines reverse
> firstTwoLines = byLines (take 2)
```

-------------------------------
What about modifying the lines?
===============================
```haskell
> indent s = "    " ++ s
```
and then **obviously**:
```haskell
indentLines = byLines indent
```
**WOOPS!** Doesn't compile:

  • Couldn't match type **‘Char’** with **‘[Char]’**
    Expected type: **[String] -> [String]**
      Actual type: **[Char] -> [Char]**
  • In the first argument of ‘byLines’, namely ‘indent’
    In the expression: byLines indent


-----------------------
How did it work before?
=======================

In this code:
```haskell
sortLines     = byLines sort
reverseLines  = byLines reverse
firstTwoLines = byLines (take 2)
```
We apply `byLines` to arguments with these types:
```haskell
sort     :: [String] -> [String]
reverse  :: [String] -> [String]
(take 2) :: [String] -> [String]
```
Type type of `indent` is:
```haskell
indent :: String -> String
indent s = "    " ++ s
```
and that isn't compatible with `byLines`.

Notice that this is the first time I've shown you types!
That's because Haskell can infer the types of our program for us


------------------
The map function
==================
```haskell
map :: (a -> b) -> [a] -> [b]
```
which for our purposes is the same as:
```haskell
map :: (String -> String) -> [String] -> [String]
```
here it is in action:
```haskell
map reverse ["red", "yellow", "blue"]
["der","wolley","eulb"]

map sort ["red", "yellow", "blue"]
["der","ellowy","belu"]
```
compare:
```haskell
reverse ["red", "yellow", "blue"]
["blue","yellow","red"]

sort ["red", "yellow", "blue"]
["blue","red","yellow"]
```

--------------------
So now we can indent
====================

We can use map to affect each line:
```haskell
> indentEachLine :: String -> String
> indentEachLine = byLines (map indent)
```
We can define a new helper in terms of the original:
```haskell
> eachLine :: (String -> String) -> String -> String
> eachLine f = byLines (map f)
```
Using our new helper looks like this:
```haskell
> indentEachLine' :: String -> String
> indentEachLine' = eachLine indent
```
and we get:

    occasional clouds
    one gets a rest
    from moon-viewing


-----------------------------------------
Hold on!  Doesn't map take two arguments?
=========================================

How can we write:
```haskell
eachLine f = unlines . map f . lines
```
When we said `map`'s type is:
```haskell
map :: (a -> b) -> [a] -> [b]
```
Think of `map`'s type this way:
```haskell
map :: (a -> b) -> ([a] -> [b])
```
`map` takes a function **(a -> b)** and transforms (lifts) it into a function over lists **([a] -> [b])**


-----------------------
Showing our excitement!
=======================
```haskell
> excite :: String -> String
> excite s = map toUpper s ++ "!!!"

> exciteEachLine :: String -> String
> exciteEachLine = eachLine excite
```
This gives:

OCCASIONAL CLOUDS!!!
ONE GETS A REST!!!
FROM MOON-VIEWING!!!


-------------------------
Now let's do it by words!
=========================
```haskell
> eachWord :: (String -> String) -> String -> String
> eachWord f = unwords . map f . words

> exciteEachWord :: String -> String
> exciteEachWord = eachWord excite
```

**D'OH!** This gives us:

OCCASIONAL!!! CLOUDS!!! ONE!!! GETS!!! A!!! REST!!! FROM!!! MOON-VIEWING!!!


-----------------------------
We want by words, by lines...
=============================
```haskell
> eachWordOnEachLine :: (String -> String) -> String -> String
> eachWordOnEachLine = eachLine . eachWord

> exciteEachWordOnEachLine :: String -> String
> exciteEachWordOnEachLine = eachWordOnEachLine excite
```
Ah, got it:

OCCASIONAL!!! CLOUDS!!!
ONE!!! GETS!!! A!!! REST!!!
FROM!!! MOON-VIEWING!!!

This is the beauty of **higher order functions**


----------------
Pause for effect
================

Questions so far?


---------------
Structured data
===============
```haskell
> data List a
>     = EndOfList
>     | Elem a (List a)
```
We can interpret that as follows:
- Create a new type called `List` which holds some type `a`
- The **only** way to get a value of type `List` is with its constructors
- Define an `EndOfList` constructor which takes no arguments
- Define an `Elem` constructor which takes an item of type `a` and a value of type `List a`

we can make some values of this type:
```haskell
empty    = EndOfList
oneWord  = Elem "apple" EndOfList
twoWords = Elem "banana" (Elem "cantaloupe" EndOfList)
```

---------
Quiz Time
=========

Given these:
```haskell
empty    = EndOfList
oneWord  = Elem "apple" EndOfList
twoWords = Elem "banana" (Elem "cantaloupe" EndOfList)
```
What are these?
```haskell
mystery1 = Elem "pear" empty
mystery2 = Elem "peach" oneWord
mystery3 = Elem "pineapple" mystery3
mystery4 = Elem 42 (Elem "apple" EndOfList)
```

-----------------------
Some operations on List
=======================

Notice that we have one case for each constructor.  Here we are switching on the constructor by pattern matching the shape of the data.
```haskell
> dropOne :: List a -> List a
> dropOne (Elem _ rest) = rest
> dropOne EndOfList     = EndOfList

> justOne :: List a -> List a
> justOne (Elem a _) = Elem a EndOfList
> justOne EndOfList  = EndOfList
```

------------------
Built-in List type
==================
```haskell
data [] a = [] | a : [a] -- this is in the standard library
infixr 5 :

empty    = []
oneWord  = "apple" : []
twoWords = "banana" : "cantaloupe" : []

mystery1 = "pear" : empty
mystery2 = "peach" : oneWord
mystery3 = "pineapple" : mystery3
mystery4 = 42 : "apple" : []  -- sweet, but still won't compile

dropOne :: [a] -> [a]
dropOne (_:rest) = rest
dropOne []       = []

justOne :: [a] -> [a]
justOne (elem:_) = elem:[]
justOne []       = []
```

-------------------
A spoonful of sugar
===================
```haskell
data [] a = [] | a : [a] -- this is in the standard library
infixr 5 :

empty    = []
oneWord  = ["apple"]
twoWords = ["banana", "cantaloupe"]

mystery1 = "pear" : empty
mystery2 = "peach" : oneWord
mystery3 = "pineapple" : mystery3
mystery4 = [42, "apple"] -- sweet, but still won't compile

dropOne :: [a] -> [a]
dropOne (_:rest) = rest
dropOne []       = []

justOne :: [a] -> [a]
justOne (elem:_) = [elem]
justOne []       = []
```

------------------
Two more things...
==================

String is defined as a list of characters:
```haskell
type String = [Char]
```

Maybe is defined as follows:
```haskell
data Maybe a = Nothing | Just a
```

Use it like this:
```haskell
pickMessage :: Maybe Int -> String
pickMessage (Just n) = "Pick a number, like " ++ show n ++ "."
pickMessage Nothing  = "Pick any number you like."
```

-------------------------------
Handling nullability with Maybe
===============================

This one is a lie:
```haskell
justOne :: [a] -> [a]
justOne (a:_) = [a]
justOne []    = []
```
This one is dangerous:
```haskell
firstOne :: [a] -> a
firstOne (a:_) = a
firstOne []    = error "Oh noes!"
```
This one is just right:
```haskell
> firstOne :: [a] -> Maybe a
> firstOne (a:_) = Just a
> firstOne []    = Nothing
```

--------------------------
Let's write some real code
==========================

Find the first character after a star:
```haskell
findAfterStar :: String -> Maybe Char
findAfterStar (c:d:r) =
  if c == '*' then Just d
              else findAfterStar (d:r)
findAfterStar _ = Nothing
```


--------------------------
Let's write some real code
==========================

We can make it more generic:
```haskell
findAfterChar :: Char -> String -> Maybe Char
findAfterChar m (c:d:r) =
  if c == m then Just d
            else findAfterChar m (d:r)
findAfterChar _ _ = Nothing
```


--------------------------
Let's write some real code
==========================

This is Haskell we can go more generic still:
```haskell
findAfterElem :: Eq a => a -> [a] -> Maybe a
findAfterElem m (c:d:r) =
  if c == m then Just d
            else findAfterElem m (d:r)
findAfterElem _ _ = Nothing
```

It is common in Haskell to find that the code you just wrote is more generic than you realized just by changing the type.


------------
Intermission
============

Questions so far?

-------------------
The Maybe of it all
===================

This is how `Maybe` is defined in the standard library:
```haskell
data Maybe a
  = Nothing
  | Just a
```
It shows up in a lot of useful places:
```haskell
elemIndex   :: a -> [a] -> Maybe Int
lookup      :: k -> Map k a -> Maybe a
stripPrefix :: Text -> Text -> Maybe Text
port        :: URI -> Maybe Int
```

-------------------
Power lifting: fmap
===================

`fmap` takes a function **(a -> b)** and lifts it into a function over some structure **(f a -> f b)**.

If this sounds like `map` that's because it is!
```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
map  ::              (a -> b) -> [a] -> [b]
```
`fmap` is a generization for operating on the value within some structure without having to know what that structure is.

These are some useful instances:
```haskell
fmap :: (a -> b) -> Either e a  -> Either e b
fmap :: (a -> b) ->    Maybe a  ->    Maybe b
fmap :: (a -> b) ->    (x -> a) ->    (x -> b)
fmap :: (a -> b) ->      (x, a) ->      (x, b)
fmap :: (a -> b) ->         [a] ->         [b]
```
This isn't some language magic.  It is actually very common to define your instances of many of these type classes.

-------------
Quick example
=============
```haskell
> addAWeek :: Day -> Day
> addAWeek d = addDays 7 d

> interestingDates :: [Day]
> interestingDates = []

> anInterestingDate :: Maybe Day
> anInterestingDate = firstOne interestingDates

> aWeekLater :: Maybe Day
> aWeekLater = fmap addAWeek anInterestingDate
```

-----------------
Alternatives: <|>
=================

Given:
```haskell
data Person = Person
  { name :: String
  , year :: Int
  }

favoriteShow :: String -> Maybe String
showWithName :: String -> Maybe String
showForYear  :: Int    -> Maybe String
```
The following will pick the first function returning a value.
```haskell
pickShow :: Person -> Maybe String
pickShow p =
        favoriteShow (name p)
    <|> showWithName (name p)
    <|> showForYear  (year p)
```
Because of Haskell's lazy evaluation we don't compute a return value for a function unless the previous one returned `Nothing`.

This allows us to defined our own short-circut evaluation for our own types and functions!


--------------
In a bind: >>=
==============

Given:
```haskell
getHeader      :: String -> Request -> Maybe String
parseDate      :: String -> Maybe Date
mailboxForDate :: Date   -> Maybe Mailbox
```
We have multiple functions that might "fail" to return a value.
We want to chain them together without a pyramid of null checks.
```haskell
example :: Maybe Mailbox
example =
  getHeader "Date" request
    >>= parseDate
    >>= mailboxForDate
```
bind `(>>=)` is a generalization for sequencing compuations that all take place in the same data structure without knowing what that structure is.

It has the following type:
```haskell
(>>=) :: Monad m => (a -> m b) -> m a -> m b
```

--------------------
Still in a bind: >>=
====================

Given:
```haskell
data User

userById      :: String -> User
friendsOfUser :: User -> [User]
```
We can get the friends of friends of the users matching our given ids
```haskell
example :: [User]
example =
  fmap userById ["1", "2", "3"]
    >>= friendsOfUser
    >>= friendsOfUser
```
These are some useful instances:
```haskell
(>>=) :: (a -> Either e b)  -> Either e a  -> Either e b
(>>=) :: (a ->    Maybe b)  ->    Maybe a  ->    Maybe b
(>>=) :: (a ->    (x -> b)) ->    (x -> a) ->    (x -> b)
(>>=) :: (a ->      (x, b)) ->      (x, a) ->      (x, b)
(>>=) :: (a ->         [b]) ->         [a] ->         [b]
```

-----------
Typeclasses
===========

Lots of functionality in Haskell is defined in terms of **typeclasses**.  At their simplest these are like interfaces in other languages.  All core functionality in Haskell is built using typeclasses, allowing the user to define behavior for all syntax.
```haskell
(+)    :: Num a         => a -> a -> a
(<)    :: Ord a         => a -> a -> Bool
(==)   :: Eq a          => a -> a -> Bool
(<|>)  :: Alternative f => f a -> f a -> f a
(>>=)  :: Monad m       => m a -> (a -> m b) -> m b
fmap   :: Functor f     => (a -> b) -> f a -> f b
length :: Foldable f    => f a -> Int
```
Because most Haskell code is written in terms of these abstractions instead of concrete types, often all you have to do is implement the right typeclasses to get a lot of free functionality and interoperability.

It should be easy to see how you can take code from disparate sources and combine them to work on your custom data types.


--------------------------
Why walk when you can run?
==========================

Questions?  Got room for a little more?


--------------
Type-safe JSON
==============

We will often define wrapper types to make them incompatible with other types of strings.

By hiding these constructors we can enforce invariants on our data at construction.
```haskell
> newtype ZipCode = ZipCode String
>   deriving (Eq, Show, Generic, ToJSON, FromJSON)

> newtype State = State String
>   deriving (Eq, Show, Generic, ToJSON, FromJSON)
```
We can then use these types to create more complicated objects:
```haskell
> data Address = Address
>   { street1 :: String
>   , street2 :: Maybe String
>   , city    :: String
>   , state   :: State
>   , zipCode :: ZipCode
>   } deriving (Eq, Show, Generic)

> instance ToJSON Address
> instance FromJSON Address
```

-----------------
Type-safe JSON II
=================
```haskell
> addr = Address "123 Fake St" Nothing "Salt Lake" (State "UT") (ZipCode "84010")
```
Here's what it looks like to use it:
```haskell
> encoded :: ByteString
> encoded = encode addr
"{\"state\":\"UT\",\"street2\":null,\"zipCode\":\"84070\",\"city\":\"Salt Lake\",\"street1\":\"123 Fake St\"}"
```
And reading some json string is equally simple:
```haskell
> decoded :: Either String Address
> decoded = eitherDecode encoded
Right (Address {street1 = "123 Fake St", street2 = Nothing, city = "Salt Lake", state = State "UT", zipCode = ZipCode "84070"})
```
And reading some JSON that doesn't match:
```haskell
> decoded' :: Either String Address
> decoded' = eitherDecode "{}"
Left "Error in $: key \"street1\" not present"
```

----------
QuickCheck
==========

The QuickCheck library has an `Arbitrary` typeclass for generating random instances of your data types.

It is optimized to create data to find edge cases. So it will try empty lists, unprintable strings, etc.
```haskell
> instance Arbitrary State where
>   arbitrary = genericArbitrary

> instance Arbitrary ZipCode where
>   arbitrary = genericArbitrary

> instance Arbitrary Address where
>   arbitrary = genericArbitrary
```
Generating a random instance looks like this:
```haskell
> rand :: IO Address
> rand = generate arbitrary
Address {street1 = "F", street2 = Nothing, city = "\r\CAN$k\FSJ\1045708pvs\985735\DC4~U\DLE8{", state = State "Y*\540182L\650396\b\1048168\662791N", zipCode = ZipCode ".:\SUB\418506V?~\NUL\CAN#m<]\223900\1053305o\1024464\&7wH\f8`RF\"Q,"}
```

----------------------------------------------
QuickCheck II: This time it's not QuickCheck I
==============================================

QuickCheck uses this ability to generate random data to create test cases for you.

For example, given a function that returns a boolean:
```haskell
> symmetric :: Address -> Bool
> symmetric a = decode (encode a) == Just a
```
We can generate random tests that search for edge cases:
```haskell
> test :: IO ()
> test = quickCheck symmetric
+++ OK, passed 100 tests.
```
QuickCheck can do this with anything you can express as a boolean check:
```haskell
> assoc :: [Int] -> [Int] -> Bool
> assoc xs ys = reverse (xs++ys) == reverse xs ++ reverse ys

> test' :: IO ()
> test' = quickCheck assoc
+++ OK, passed 100 tests.
```

--------------------------
Cheap and easy concurrency
==========================

Because data in Haskell is immutable it is automatically thread safe.

This means that concurrency isn't only cheap, but it's easy!

This code is taken right out of CRA Products:
```haskell
prs <- runConcurrently $ CRAProducts
  <$> Concurrently (getProducts "Clarity" Clarity.fetchAllProducts clarity productClarity)
  <*> Concurrently (getProducts "DataX" DataX.fetchAllProducts dataX productDataX)
  <*> Concurrently (getProducts "FactorTrust" FT.fetchAllProducts factorTrust productFT)
  <*> Concurrently (getProducts "MicroBilt" MicroBilt.fetchProducts microBilt productMicroBilt)
  <*> Concurrently (getProducts "Neustar" Neustar.fetchAllProducts neustar productNeustar)
  <*> Concurrently (getProducts "Teletrack" Teletrack.fetchAllProducts teletrack productTeletrack)
  <*> Concurrently (getProducts "TransUnion" TransUnion.fetchAllProducts transunion productTransUnion)
  <*> ((<>) <$> Concurrently (getProducts "NetConnect" NetConnect.fetchAllProducts netConnectConfig productNetConnect)
            <*> Concurrently (getProducts "PreciseId" PreciseId.fetchAllProducts preciseIdConfig productPreciseId))
  <*> Concurrently (getProducts "Giact" Giact.fetchAllProducts giact productGiact)
  <*> Concurrently (getProducts "iOvation" IOvation.fetchAllProducts iOvation productIOvation)
  <*> Concurrently (getProducts "eBureau" EBureau.fetchAllProducts eBureauCfg productEBureau)
```

--------------------
Writing text parsers
====================

Say that you had to parse structured data out of some text like this:

`BIRK 281500Z 09014KT CAVOK M03/M06 Q0980 R13/910195`
```haskell
> digit :: ReadP Char
> digit = satisfy isDigit

> timestamp :: ReadP (Int, Int, Int)
> timestamp = do
>   day    <- count 2 digit
>   hour   <- count 2 digit
>   minute <- count 2 digit
>   char 'Z'
>   pure (read day, read hour, read minute)
```
Followed by the airport `BIRK`:
```haskell
> uppercase :: ReadP Char
> uppercase = choice (fmap char ['A'..'Z'])

> airport :: ReadP String
> airport = many1 uppercase
```

---------------------------------
Writing text parsers: Leveling Up
=================================

Let's get more complicated `09014KT`:
```haskell
> data WindInfo = WindInfo
>  { dir   :: Int
>  , speed :: Int
>  , unit  :: String
>  } deriving Show

> numbers :: Int -> ReadP Int
> numbers n = fmap read (count n digit)

> windInfo :: ReadP WindInfo
> windInfo = do
>   direction <- numbers 3
>   speed     <- numbers 2 <|> numbers 3
>   unit      <- string "KT" <|> string "MPS"
>   pure $ WindInfo direction speed unit
```

---------------------------------------
Writing text parsers: The final chapter
=======================================
```haskell
> data Report = Report
>   { station :: String
>   , time    :: (Int, Int, Int)
>   , wind    :: WindInfo
>   } deriving Show

> metar = do
>   code <- airport   <* char ' '
>   time <- timestamp <* char ' '
>   wind <- windInfo  <* char ' '
>   pure (Report code time wind)

> report = readP_to_S metar "BIRK 281500Z 09014KT CAVOK M03/M06 Q0980 R13/910195"
[ ( Report
      { station = "BIRK"
      , time = (28,15,0)
      , wind = WindInfo
          { dir = 90
          , speed = 14
          , unit = "KT"
          }
      }
  , "CAVOK M03/M06 Q0980 R13/910195"
  )
]
