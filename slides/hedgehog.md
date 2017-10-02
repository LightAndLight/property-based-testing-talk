# Hedgehog

##

<img src="./img/hedgehog_github.png" />

##

```haskell
import Hedgehog
```

##

```haskell
import Hedgehog
import qualified Hedgehog.Gen as Gen
```

##

```haskell
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
```

##

```haskell
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

reverse_involutive :: Property
reverse_involutive =
  _
```

##

```haskell
data PropertyT m a

property :: PropertyT IO () -> Property
```

##

```haskell
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

reverse_involutive :: Property
reverse_involutive =
  property $ do
    _
```

<div class="notes">
Now we need to encode the property
</div>

##

```haskell
data GenT m a
type Gen = GenT Identity

class Monad m => MonadGen m where ...
instance Monad m => MonadGen (GenT m) where ...
```

##

```haskell
bool :: MonadGen m => m Bool
element :: MonadGen m => [a] -> m a
ascii :: MonadGen m => m Char
choice :: MonadGen m => [m a] -> m a
filter :: MonadGen m => (a -> Bool) -> m a -> m a
int :: MonadGen m => Range Int -> m Int
list :: MonadGen m => Range Int -> m a -> m [a]
```

##

```haskell
Gen.list _ Gen.ascii
```

##

```haskell
data Range a

singleton :: a -> Range a
constant :: a -> a -> Range a
linear :: Integral a => a -> a -> Range a
exponential :: Integral a => a -> a -> Range a
```

##

```haskell
Gen.list (Range.constant 0 100) Gen.ascii
  :: MonadGen m => m [Char]
```

<div class="notes">
Now we have a generator for lists of ascii characters, but
</div>

##

```haskell
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

reverse_involutive :: Property
reverse_involutive =
  property $ do
    _
```

<div class="notes">
I said our properties are written in the propertyT monad?
</div>

##

```haskell
forAll
  :: (Monad m, Show a, HasCallStack)
  => Gen a
  -> PropertyT m a
```
<div class="notes">
So we use forall to "sample" from the generator
</div>
##

```haskell
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

reverse_involutive :: Property
reverse_involutive =
  property $ do
    _
```

```haskell
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

reverse_involutive :: Property
reverse_involutive =
  property $ do
                  Gen.list (Range.constant 0 100) Gen.ascii
    _
```

##

```haskell
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

reverse_involutive :: Property
reverse_involutive =
  property $ do
         forAll $ Gen.list (Range.constant 0 100) Gen.ascii
    _
```

##

```haskell
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

reverse_involutive :: Property
reverse_involutive =
  property $ do
    l <- forAll $ Gen.list (Range.constant 0 100) Gen.ascii
    _
```

<div class="notes">
Now we have to test that the property holds
</div>

##

```haskell
class Monad m => MonadTest m where ...
instance Monad m => MonadTest (PropertyT m) where ...

success :: MonadTest m => m ()
failure :: (MonadTest m, HasCallStack) => m a
assert :: (MonadTest m, HasCallStack) => Bool -> m ()
(===) :: (Eq a, MonadTest m, HasCallStack) => a -> a -> m ()

footnote :: (MonadTest m, HasCallStack) => String -> m ()
annotate :: (MonadTest m, HasCallStack) => String -> m ()
```

##

```haskell
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

reverse_involutive :: Property
reverse_involutive =
  property $ do
    l <- forAll $ Gen.list (Range.linear 0 100) Gen.ascii
    _ 
```

##

```haskell
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

reverse_involutive :: Property
reverse_involutive =
  property $ do
    l <- forAll $ Gen.list (Range.linear 0 100) Gen.ascii
    reverse 
```

##

```haskell
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

reverse_involutive :: Property
reverse_involutive =
  property $ do
    l <- forAll $ Gen.list (Range.linear 0 100) Gen.ascii
    reverse (reverse l)
```

##

```haskell
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

reverse_involutive :: Property
reverse_involutive =
  property $ do
    l <- forAll $ Gen.list (Range.linear 0 100) Gen.ascii
    reverse (reverse l) ===
```

##

```haskell
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

reverse_involutive :: Property
reverse_involutive =
  property $ do
    l <- forAll $ Gen.list (Range.linear 0 100) Gen.ascii
    reverse (reverse l) === l
```

##

```haskell
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

reverse_involutive :: Property
reverse_involutive =
  property $ do
    l <- forAll $ Gen.list (Range.linear 0 100) Gen.ascii
    reverse (reverse l) === l
    
main = _
```

##

```haskell
check :: MonadIO m => Property -> m Bool
checkParallel :: MonadIO m => Group -> m Bool
checkSequential :: MonadIO m => Group -> m Bool
```

<div class="notes">
Test groups aren't particularly novel, so I'm just going to skip over it
You can group and name your tests.
</div>

##

```haskell
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

reverse_involutive :: Property
reverse_involutive =
  property $ do
    l <- forAll $ Gen.list (Range.linear 0 100) Gen.ascii
    reverse (reverse l) === l
    
main = _
```

##

```haskell
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

reverse_involutive :: Property
reverse_involutive =
  property $ do
    l <- forAll $ Gen.list (Range.linear 0 100) Gen.ascii
    reverse (reverse l) === l
    
main = check
```

##

```haskell
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

reverse_involutive :: Property
reverse_involutive =
  property $ do
    l <- forAll $ Gen.list (Range.linear 0 100) Gen.ascii
    reverse (reverse l) === l
    
main = check reverse_involutive
```

##

<img src="./img/hedgehog_tests_passed.png" />

##

```haskell
reverse :: [a] -> [a]
reverse [x] = [x, x]
reverse xs = P.reverse xs 
```

##

<img src="./img/hedgehog_tests_failed.png" />

## Generators

- Seed
- Size
- Shrink Tree

##

```haskell
newtype GenT m a
  = GenT { unGen :: Size -> Seed -> Tree (MaybeT m) a }
```

##

```haskell
Gen a ~ Size -> Seed -> Tree Maybe a
```

<div class="notes">
- Tree: each node in the tree is wrapped in a Maybe
- The tree might have a node, and that node may have children, which
  may have nodes, etc.
- It's not necessary to understand how this works, but it's helpful to
  at least be aware of what's going on
</div>

##

```haskell
class Monad m => MonadGen m where
  liftGen :: Gen a -> m a
  shrinkGen :: (a -> [a]) -> m a -> m a
  pruneGen :: m a -> m a
  scaleGen :: (Size -> Size) -> m a -> m a
  freezeGen :: m a -> m (a, m a)
```

<div class="notes">
You'll never need to write an instance of this class, and you'll never
even need to use the class members.

Hedgehog.Gen exports these same functions without the -Gen suffix
</div>

##

```haskell
lift :: MonadGen m => Gen a -> m a
shrink :: MonadGen m => (a -> [a]) -> m a -> m a
prune :: MonadGen m => m a -> m a
scale :: MonadGen m => (Size -> Size) -> m a -> m a
freeze :: MonadGen m => m a -> m (a, m a)
```

## Seed

```haskell
recheck :: MonadIO m => Size -> Seed -> Property -> m ()
```

## Size

```haskell
newtype Size = Size { unSize :: Int }
  deriving (Eq, Ord, Show, Num, ...)

scale :: MonadGen m => (Size -> Size) -> m a -> m a
small :: MonadGen m => m a -> m a
resize :: MonadGen m => Size -> m a -> m a
sized :: MonadGen m => (Size -> m a) -> m a
```

<div class="notes">
- What it is 
- What the functions do
</div>

##

```haskell
data BinTree a
  = Tip a
  | Bin (BinTree a) a (BinTree a)
```

##

```haskell
genBinTree :: MonadGen m => m a -> m (BinTree a)
genBinTree gen =
  Gen.choice
    [ Tip <$> gen
    , Bin <$>
      genBinTree gen <*>
      gen <*>
      genBinTree gen
    ]
```

##

```haskell
genBinTree :: MonadGen m => m a -> m (BinTree a)
genBinTree gen =
  let
    rec = Gen.scale (/ 2) (genBinTree gen)
  in
    Gen.sized $ \size ->
      if size < 1
      then Tip <$> gen
      else
        Gen.choice
          [ Tip <$> gen
          , Bin <$> rec <*> gen <*> rec
          ]
```

##

```haskell
recursive
  :: MonadGen m => ([m a] -> m a) -> [m a] -> [m a] -> m a
```

##

```haskell
genBinTree :: MonadGen m => m a -> m (BinTree a)
genBinTree gen =
  Gen.recursive
    Gen.choice
    [ Tip <$> gen ] 
    [ Bin <$>
      genBinTree gen <*>
      gen <*>
      genBinTree gen
    ]
```

##

```haskell
singleton :: a -> Range a
constant :: a -> a -> Range a
linear :: Integral a => a -> a -> Range a
exponential :: Integral a => a -> a -> Range a
```


## Shrinking

##

<img src="./img/hedgehog_tests_failed.png" />

##

Finding less complex failure cases

<div class="notes">
- Generator may generate large, complex input that fails
- Changes are there is a less complex input that also fails
- Shrinking is the process of finding that simpler failure case
</div>

##

```haskell
10? 9? 8? … 1? 0?
```

##
```haskell
[3, 6, 5] ?

[3, 6] [6, 3] [6, 5] [5, 6] [3, 5] [5, 3] ?

[3] [6] [5] ?

[] ?
```

##
```haskell
Just 'a'?

Nothing?
```
##

```haskell
a -> [a]
```

##

```haskell
shrink :: MonadGen m => (a -> [a]) -> m a -> m a
prune :: MonadGen m => m a -> m a
```

##

```haskell
list :: MonadGen m => Range Int -> m a -> m [a]
```

##

```haskell
printTree :: (MonadIO m, Show a) => Gen a -> m ()
```

##

```haskell
>>> Gen.printTree $ Gen.list (Range.constant 0 3) Gen.bool

[True,True,True]
 ├╼[]
 ├╼[True,True]
 │  ├╼[]
 │  ├╼[True]
 │  │  ├╼[]
 │  │  └╼[False]
 │  ├╼[True]
 │  │  ├╼[]
 │  │  └╼[False]
 │  ├╼[False,True]
 ...
```

##

```haskell
genBinTree :: MonadGen m => m a -> m (BinTree a)
genBinTree gen =
  Gen.recursive
    Gen.choice
    [ Tip <$> gen ] 
    [ Bin <$>
      genBinTree gen <*>
      gen <*>
      genBinTree gen
    ]
```

<div class="notes">
Shrinking behaviour is basically correct by construction
</div>

##

```haskell
>>> Gen.printTree $ Gen.resize 3 (genBinTree Gen.bool)

Bin (Tip True) False (Tip False)
 ├╼Tip True
 │  └╼Tip False
 └╼Bin (Tip False) False (Tip False)
```

##

Shrink (mostly) for free!

## Property-based testing is great

## Property-based testing with Hedgehog is awesome
