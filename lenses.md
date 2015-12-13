# Lenses, what are they?

First some sources: 

* [](https://hackage.haskell.org/package/lens)
* [](http://blog.jakubarnold.cz/2014/07/14/lens-tutorial-introduction-part-1.html)
* [](http://blog.jakubarnold.cz/2014/08/06/lens-tutorial-stab-traversal-part-2.html)
* [](https://github.com/ekmett/lens/wiki)
* [](http://adit.io/posts/2013-07-22-lenses-in-pictures.html)
* [](http://artyom.me/lens-over-tea-1)
* [](https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references)


What's the problem?

```haskell
{-# LANGUAGE RankNTypes #-}
module Main where

data Address =  Address { country, street :: String, house:: Int }
data Person = Person { age:: Int, name::String, address::Address }

```

Getters and setters from imperative world are ugly.
No decent composition

```haskell
setStreet p s =  Person (age p) (name p) (Address ((country.address) p) s ((house.address) p ) ) 
```

Here come the lenses. ```s``` is object, ```a``` is focus.



```haskell
data NaiveLens s a = NaiveLens { view' :: s -> a, set'  :: a -> s -> s }

john = Person  42 "John" $ Address "France" "Rue" 42


nameLens :: NaiveLens Person String
nameLens = NaiveLens name (\ a s -> Person (age s) a (address s) )

setter =  set' nameLens "Bob" john

```

We use ```over``` to get out of the boring "view-apply-set" room. Looks like fmap.

```haskell
data Lens1 s a = Lens1 { view :: s -> a, set  :: a -> s -> s, over1:: (a->a) -> s -> s }

ageLens :: Lens1 Person Int
ageLens = Lens1 age
                     (\a s -> s { age = a })
                     (\f s -> s { age = f (age s) })

```

It means however we need to provide loads of code for each lens AND make multiple lenses for each data type.

However thanks to 'const' we can define 'set' in terms of 'view' and 'over'.
Const ignores the current value.

```haskell

data Lens2 s a = Lens2 { view'' :: s -> a, over'':: (a->a) -> s -> s }

set'' :: Lens2 s a -> a -> s -> s
set'' ln a s = over'' ln (const a) s

```

We could want to make 'overIO' or other such things. Let's stick to a functor instead for the sake of making things general.

` overF :: Functor f => (a -> f a) -> s -> f s`


Strangely enough, we can derive 'set' and 'view' from it (the explanation will be delayed a bit). Thanks to that we can just make a type alias


```haskell
type Lens s a = Functor f => (a -> f a) -> s -> f s

```

Note: we need RankNTypes for the following.

Let's step aside for a bit and look at  `Control.Applicative.Const` and `Data.Functor.Identity`

```haskell
newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
```
Recall: 
type Lens s a = Functor f => (a -> f a) -> s -> f s
```haskell
over:: Lens s a -> (a -> a) -> (s -> s)
over ln f s = runIdentity $ ln ( Identity . f ) s

```
`Const` can help with the view as well.




```haskell


main:: IO ()
main = putStrLn "hey"

```
