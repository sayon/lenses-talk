-- # Lenses, what are they?
-- 
-- First some sources (this document is a brief compilation): 
-- 
-- * [Lens package doc](https://hackage.haskell.org/package/lens)
-- * [Introduction pt. 1](http://blog.jakubarnold.cz/2014/07/14/lens-tutorial-introduction-part-1.html)
-- * [Introduction pt. 2](http://blog.jakubarnold.cz/2014/08/06/lens-tutorial-stab-traversal-part-2.html)
-- * [Lens wiki](https://github.com/ekmett/lens/wiki)
-- * [Lens in pictures](http://adit.io/posts/2013-07-22-lenses-in-pictures.html)
-- * [Lens over tea](http://artyom.me/lens-over-tea-1)
-- * [Lenses and functional references: wiki](https://en.wikibooks.org/wiki/Haskell/SLenses_and_functional_references)
-- 
--
-- What's the problem?
-- 
-- ```haskell
{-# LANGUAGE RankNTypes #-}
module Main where

import Data.Monoid

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
-- Additionally for more complex structures like Map update and view-set
-- can have different costs.
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
-- We could want to make ```overIO``` or something like that with monads. Let's stick to a functor instead for the sake of making things general.
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
--
-- ### Lenses summary:
-- * similar to ``` modify :: (a -> b) -> s -> t```. With Functor we can do
-- more cool stuff 
--actually like ultraModify :: Functor f => (a -> f b) -> s -> f t... and
--this f here enables us to do lots of cool stuff.
--
-- * If you want a lens to be like an ordinary modify, use Identity in place
--of f. This is done by over in lens.
--
-- * If you want to use the lens as a getter, use Const as f – it would store
--the a value and “carry it to the outside” for you. This is done by view
--in lens.
--
-- * You don't need any special overM to update the value using IO or
--something – just apply the lens directly. over is merely a convenient
--shortcut to wrap and unwrap Identity.
--
-- * You can create a lens from a getter and a setter, but it might be less
--than optimal (because this way modify would necessarily be a combination
--of get and set, instead of a single “dive” into the data structure).
-- The lenses can be pushed even further. As for now they are bad when
-- dealing with recursive types (such as List). Let's take a look at
-- ```Traversable``` / ```Foldable```
--
-- ## Foldable
-- Typeclass ```foldable``` generalizes list folding 
--  requires one of these to work:
--  * ```foldMap::foldMap :: Monoid m => (a -> m) -> t a -> m``` converts
--  elements to monoid and executes mconcat
--  * ```foldr:: (a -> (b -> b)) -> b -> t a -> b```
--
--  To express foldr through foldMap we use the fact of endomorphisms
--  forming a monoid ( id, (.) ).
--  The corresponding type is called Endo.
--```haskell
newtype Endo b = Endo { appEndo :: b -> b }
                

instance Monoid (Main.Endo b) where
        mempty = Main.Endo id
        mappend (Main.Endo g) (Main.Endo f) = Main.Endo (g . f)
-- ```
-- Foldable can be transformed to list with ```toList```
--
--  
--
-- ## Traversable
-- ```traversable``` crawls the structure and produces an overall effect.
-- 
-- ```haskell


-- ```
--
--
--
---- ```haskell

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
-- Note: ```traverse:: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)```
--
--
-- ```
-- > view (traverse.posts) users
-- [Post "hello",Post "world",Post "foobar"]
--
--
-- ```haskell
main:: IO ()
main = putStrLn "hey"

-- ```
