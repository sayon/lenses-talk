-- # Lenses, what are they?
-- 
-- First some sources: 
-- 
-- * [Lens package doc](https://hackage.haskell.org/package/lens)
-- * [Introduction pt. 1](http://blog.jakubarnold.cz/2014/07/14/lens-tutorial-introduction-part-1.html)
-- * [Introduction pt. 2](http://blog.jakubarnold.cz/2014/08/06/lens-tutorial-stab-traversal-part-2.html)
-- * [Lens wiki](https://github.com/ekmett/lens/wiki)
-- * [Lens in pictures](http://adit.io/posts/2013-07-22-lenses-in-pictures.html)
-- * [Lens over tea](http://artyom.me/lens-over-tea-1)
-- * [Lenses and functional references: wiki](https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references)
-- 
--
-- What's the problem?
-- 
-- ```haskell
{-# LANGUAGE RankNTypes #-}
module Main where

data Address =  Address { country, street :: String, house:: Int }
data Person = Person { age:: Int, name::String, address::Address }

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
data NaiveLens s a = NaiveLens { view' :: s -> a, set'  :: a -> s -> s }

john = Person  42 "John" $ Address "France" "Rue" 42


nameLens :: NaiveLens Person String
nameLens = NaiveLens name (\ a s -> Person (age s) a (address s) )

setter =  set' nameLens "Bob" john

-- ```
-- 
-- We use ```over``` to get out of the boring "view-apply-set" room. Looks like fmap.
-- 
-- ```haskell
data Lens1 s a = Lens1 { view1 :: s -> a, set1  :: a -> s -> s, over1:: (a->a) -> s -> s }

ageLens :: Lens1 Person Int
ageLens = Lens1 age
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

data Lens2 s a = Lens2 { view'' :: s -> a, over'':: (a->a) -> s -> s }

set'' :: Lens2 s a -> a -> s -> s
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
type Lens s a = Functor f => (a -> f a) -> s -> f s

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
-- type Lens s a = Functor f => (a -> f a) -> s -> f s
--```haskell
over:: Lens s a -> (a -> a) -> (s -> s)
over ln f s = runIdentity $ ln ( Identity . f ) s

-- ```
-- ```Const``` can help with the view as well (quite tricky).
--
--
-- ```haskell
newtype Const a b = Const { getConst :: a } 

instance Functor (Const a) where
        fmap _ (Const c) = Const c 


view :: Lens s a -> s -> a
view ln s = getConst $ ln Const s

set :: Lens s a -> a -> s -> s
set ln x = over ln (const x)

-- ```
--
--
-- ##Example: first component of a pair
-- 
-- ```haskell

_1 :: Lens (a,b) a 
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

-- ```haskell
main:: IO ()
main = putStrLn "hey"

-- ```
