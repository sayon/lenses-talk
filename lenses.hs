-- # Lenses, what are they?
-- 
-- First some sources (this document is a brief compilation): 
-- 
-- * [SLens package doc](https://hackage.haskell.org/package/lens)
-- * [Introduction pt. 1](http://blog.jakubarnold.cz/2014/07/14/lens-tutorial-introduction-part-1.html)
-- * [Introduction pt. 2](http://blog.jakubarnold.cz/2014/08/06/lens-tutorial-stab-traversal-part-2.html)
-- * [SLens wiki](https://github.com/ekmett/lens/wiki)
-- * [SLens in pictures](http://adit.io/posts/2013-07-22-lenses-in-pictures.html)
-- * [SLens over tea](http://artyom.me/lens-over-tea-1)
-- * [SLenses and functional references: wiki](https://en.wikibooks.org/wiki/Haskell/SLenses_and_functional_references)
-- 
--
-- What's the problem?
-- 
-- ```haskell
{-# LANGUAGE RankNTypes #-}
module Main where

data Address =  Address { country, street :: String, house:: Int } deriving Show
data Person = Person { age:: Int, name::String, address::Address } deriving Show

-- ```
-- 
-- Getters and setters from imperative world are ugly.
-- No decent composition
-- 
-- ```haskell
setStreet p s =  Person (age p) (name p) (Address ((country.address) p) s ((house.address) p ) ) 
-- ```
-- 
-- Here come the lenses. ```s``` is object, ```a``` is focus.
-- 
-- 
-- 
-- ```haskell
data NaiveSLens s a = NaiveSLens { view' :: s -> a, set'  :: a -> s -> s }

john = Person  42 "John" $ Address "France" "Rue" 42


nameSLens :: NaiveSLens Person String
nameSLens = NaiveSLens name (\ a s -> Person (age s) a (address s) )

setter =  set' nameSLens "Bob" john

-- ```
-- 
-- We use ```over``` to get out of the boring "view-apply-set" room. Looks like fmap.
-- 
-- ```haskell
data SLens1 s a = SLens1 { view1 :: s -> a, set1  :: a -> s -> s, over1:: (a->a) -> s -> s }

ageSLens :: SLens1 Person Int
ageSLens = SLens1 age
                     (\a s -> s { age = a })
                     (\f s -> s { age = f (age s) })

-- ```
-- 
-- It means however we need to provide loads of code for each lens AND make multiple lenses for each data type.
-- 
-- However thanks to ```const``` we can define ```set``` in terms of ```view``` and ```over```.
-- Const ignores the current value.
-- 
-- ```haskell

data SLens2 s a = SLens2 { view'' :: s -> a, over'':: (a->a) -> s -> s }

set'' :: SLens2 s a -> a -> s -> s
set'' ln a s = over'' ln (const a) s

-- ```
-- 
-- We could want to make ```overIO``` or other such things. Let's stick to a functor instead for the sake of making things general.
-- 
-- ` overF :: Functor f => (a -> f a) -> s -> f s`
-- 
-- 
-- Strangely enough, we can derive ```set``` and ```view``` from it (the explanation will be delayed a bit). Thanks to that we can just make a type alias
-- 
-- 
-- ```haskell
type SLens s a = Functor f => (a -> f a) -> s -> f s

-- ```
-- 
-- Note: we need ```RankNTypes``` for the following.
-- 
-- Let's step aside for a bit and look at  ```Control.Applicative.Const``` and ```Data.Functor.Identity```
-- 
-- ```haskell
newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
--```
-- Recall: 
-- type SLens s a = Functor f => (a -> f a) -> s -> f s
--```haskell
over:: SLens s a -> (a -> a) -> (s -> s)
over ln f s = runIdentity $ ln ( Identity . f ) s

-- ```
-- ```Const``` can help with the view as well (quite tricky).
--
--
-- ```haskell
newtype Const a b = Const { getConst :: a } 

instance Functor (Const a) where
        fmap _ (Const c) = Const c 


view :: SLens s a -> s -> a
view ln s = getConst $ ln Const s

set :: SLens s a -> a -> s -> s
set ln x = over ln (const x)

-- ```
--
--
-- ##Example: first component of a pair
-- 
-- ```haskell

_1 :: SLens (a,b) a 
_1 f (x,y)  = fmap (\a -> (a, y)) (f  x)

-- ```
--
-- ```
-- > let test = (1,2)
-- > set _1 0 test
-- (0, 2)
-- > view _1 test
-- 0
-- ```
--
-- ```haskell

someone = Person 24 "Bob" (Address "Romania" "Baker str" 90210 )

nameSLens':: SLens Person String
nameSLens' f person = fmap (\ newname -> person { name = newname }) (f (name person))

ageSLens' f person = fmap (\ newage -> person { age = newage }) (f (age person))


-- ```
--
-- Notice that lenses can be easily composed
-- ```
-- > ((set nameSLens' "Chewbacca") . (set ageSLens' 4242) ) someone
-- {age = 4242, name = "Chewbacca", address = Address {country = "Romania",
-- street = "Baker str", house = 90210}} 
-- ```
-- ## Generalization
-- Let's generalize our lenses' type. 
-- ```haskell
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t


-- ```
--
-- This way the structure's type does not need to be preserved. 
-- The lenses can be pushed even further. As for now they are bad when
-- dealing with recursive types (such as List)
--
-- ```haskell

data User = User String [Post] deriving Show
data Post = Post String deriving Show

posts :: SLens User [Post]
posts f (User n p) = fmap (\p' -> User n p') (f p)

title :: SLens Post String
title f (Post t) = fmap Post (f t)

users :: [User]
users = [User "john" [Post "hello", Post "world"], User "bob" [Post "foobar"]]

-- ```
-- After importing ```Control.Lens``` from scratch we can use
-- ```traverse``` to change focus from ```[a]``` to ```a```.
-- Note: ```traverse:::: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)```
--
-- ```haskell
main:: IO ()
main = putStrLn "hey"

-- ```
