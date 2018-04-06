```purescript
module Simple.Recursion where

import Prelude

import Data.Either (Either, either)
```

The least fixed point for a functor. Let's say you have an `Expr` datatype
that looks like the following:

    data Expr
      = Float Number
      | Add Expr Expr

We may want to map into this type, but we can't since it's kind is `Type`.
We'll need to factor out the areas we're interested in mapping over:

    data Expr expr
      = Float Number
      | Add expr expr

Now we can give it a `Functor` instance.
But there's a problem: A nested `Expr` can quickly get a type signature that's
unwieldy:

    fifteen :: Expr (Expr (Expr Void))
    fifteen = Add (Add (Float 2.0) (Float 3.0)) (Add (Float 7.0) (Float 3.0))

Using `Fix`, we can get a better type signature:

    fifteen :: Fix Expr
    fifteen = add (add (float 2.0) (float 3.0)) (add (float 7.0) (float 3.0))
        where
        add x y = Fix (Add x y)
        float x = Fix (Float x)

```purescript
data Fix f = Fix (f (Fix f))

unfix :: forall f. Fix f -> f (Fix f)
unfix (Fix f) = f

refix :: forall f g. Functor g => (f ~> g) -> Fix f -> Fix g
refix nt = Fix <<< map (refix nt) <<< nt <<< unfix
```

A `Reducer` is a function that takes a type like `Expr` above and folds it into
another type.
Using `reduce`, we can for example get a neat textual representation of our
adding language:

    pprint :: Expr String -> String
    pprint = case _ of
        Float n -> show n
        Add e1 e2 = "(" <> e1 <> " + " <> e2 <> ")"
    
    fifteenString = reduce pprint fifteen -- = "((2.0 + 3.0) + (7.0 + 3.0))"

The traditional name for a `Reducer` is `Algebra`, and `reduce` is a catamorphism
-- shortened to `cata` typically.
```purescript
type Reducer f a = f a -> a

reduce :: forall f a. Functor f => Reducer f a -> Fix f -> a
reduce f = reduce_ (const f)

type Algebra f a = Reducer f a

cata :: forall f a. Functor f => Algebra f a -> Fix f -> a
cata = reduce
```

Apply a given function to an expression tree from the leaves to the root.
```purescript
bottomUp :: forall f. Functor f => (Fix f -> Fix f) -> Fix f -> Fix f
bottomUp f = reduce (f <<< Fix)
```

A `Producer` is the dual to a `Reducer`: instead of tearing down a structure,
it builds one up.
The traditional name is `Coalgebra`, and associated with anamorphisms (shortened
to `ana`).

```purescript
type Producer f a = a -> f a

produce :: forall f a. Functor f => Producer f a -> a -> Fix f
produce f = Fix <<< map (produce f) <<< f

type Coalgebra f a = Producer f a

ana :: forall f a. Functor f => Coalgebra f a -> a -> Fix f
ana = produce
```

Apply a given function to an expression tree from the root to the leaves.
```purescript
topDown :: forall f. Functor f => (Fix f -> Fix f) -> Fix f -> Fix f
topDown f = produce (unfix <<< f)
```

A `Reducer_` is a reducer with context (hence the extra symbol).
It's like a normal `Reducer` but has access to the original structure.
Hence, a `Reducer` is exactly a `Reducer_` that forgets the context, so this
type is strictly more powerful.

`Reducer_`s are called `RAlgebra`s, and they correspond to paramorphisms (`para`).

```purescript
type Reducer_ f a = Fix f -> f a -> a

reduce_ :: forall f a. Functor f => Reducer_ f a -> Fix f -> a
reduce_ f t = f t $ map (reduce_ f) $ unfix t

type RAlgebra f a = Reducer_ f a

para :: forall f a. Functor f => RAlgebra f a -> Fix f -> a
para = reduce_
```

A `Producer_` is the dual to a `Reducer_`.
If the `Producer_` outputs a `Left _`, then the production will stop.
These are typically called `RCoalgebra`s, and are tied with apomorphisms (`apo`).

```purescript
type Producer_ f a = a -> f (Either (Fix f) a)

produce_ :: forall f a. Functor f => RCoalgebra f a -> a -> Fix f
produce_ f = Fix <<< map fanin <<< f where
    fanin = either id (produce_ f)

type RCoalgebra f a = Producer_ f a

apo :: forall f a. Functor f => RCoalgebra f a -> a -> Fix f
apo = produce_
```