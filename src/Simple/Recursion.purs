module Simple.Recursion where

import Prelude

import Data.Either (Either, either)


data Fix f = Fix (f (Fix f))

unfix :: forall f. Fix f -> f (Fix f)
unfix (Fix f) = f


type Reducer f a = f a -> a

reduce :: forall f a. Functor f => Reducer f a -> Fix f -> a
reduce f = reduce_ (const f)

type Algebra f a = Reducer f a

cata :: forall f a. Functor f => Algebra f a -> Fix f -> a
cata = reduce


bottomUp :: forall f. Functor f => (Fix f -> Fix f) -> Fix f -> Fix f
bottomUp f = reduce (f <<< Fix)


type Producer f a = a -> f a

produce :: forall f a. Functor f => Producer f a -> a -> Fix f
produce f = Fix <<< map (produce f) <<< f

type Coalgebra f a = Producer f a

ana :: forall f a. Functor f => Coalgebra f a -> a -> Fix f
ana = produce


topDown :: forall f. Functor f => (Fix f -> Fix f) -> Fix f -> Fix f
topDown f = produce (unfix <<< f)


type Reducer_ f a = Fix f -> f a -> a

reduce_ :: forall f a. Functor f => Reducer_ f a -> Fix f -> a
reduce_ f t = f t $ map (reduce_ f) $ unfix t

type RAlgebra f a = Reducer_ f a

para :: forall f a. Functor f => RAlgebra f a -> Fix f -> a
para = reduce_


type Producer_ f a = a -> f (Either (Fix f) a)

produce_ :: forall f a. Functor f => RCoalgebra f a -> a -> Fix f
produce_ f = Fix <<< map fanin <<< f where
    fanin = either id (produce_ f)

type RCoalgebra f a = Producer_ f a

apo :: forall f a. Functor f => RCoalgebra f a -> a -> Fix f
apo = produce_

