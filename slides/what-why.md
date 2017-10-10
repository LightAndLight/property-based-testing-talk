# What is property based testing?

##

"Testing the mathematical properties of a system"

##

What is a mathematical property?

## 

"A generalised relationship between the input and output of an operation"

<div class="notes">
So what does this mean in the context of programming?
</div>

##

```
reverse(reverse(l))            = l

delete(key, delete(key, d))    = delete(key, d)

x + y                          = y + x
```

<div class="notes">
Here's a couple of properties about common functions
</div>

##

```
>>> listOf(5, 'a')
[ 'a', 'a', 'a', 'a', 'a' ]
```

<div class="notes">
You're not limited to things that have names.

Take this function-

</div>
##

`length(listOf(n, item)) = n`

<div class="notes">
Makes sense right?
</div>

##

These are all very natural behaviours

##

Made explicit using mathematical notation

## 

Mathematics will help you write better software

<div class="notes">
- I want to take this time to have a little spiel about why
  you shouldn't switch off now that I've mentioned maths
- Maths is important for writing good code, no matter your field
</div>

##

Nicer APIs

<div class="notes">
- We already understand many "mathematical" concepts at a visceral level
- If I gave you a function and said "this reverses items in the list",
  you would expect it to be involutive, regardless of if you knew that word
- By keeping mathematical properties in mind, you can develop APIs
  that "just feel right", because you're tapping into people subsconscious
  expectations of how it should work
- This is the case for some of my favourite "haskelly" concepts, such
  as FRP and parser combinators- they have mathematical formalisations,
  which is why they're so wonderful
- What I'm really trying to describe is called "Denotation driven design",
  but that talk's already been done by Conal
</div>

##

Another way to think about your code

<div class="notes">
- The process of thinking about properties of your code require a deep understanding
  of what that code does
- You will likely find and fix bugs before the testing stage because you're being
  so critical
- Sometimes you find that your code almost satisfies some property, 
  and changing it to actually satisfy that property brings it more input
  line with what you intended
</div>

##

Easier and more robust testing

<div class="notes">
- Mathematics is relatively unambiguous
- Easier to get a computer to interpret and check your descriptions
- We're going to write mathematical properties and check them mechanically
- But there are tools that you can use to actually *prove* that these
  properties hold in your programs.
</div>

##

Property-based __testing__

<div class="notes">
So that's the property part, what about the testing part? 
</div>

## 

`reverse(reverse(l)) = l`

<div class="notes">
How would you test this property?
</div>

##

```python
assert(reverse(reverse([1,2,3])) == [1,2,3])
assert(reverse(reverse([1])) == [1])
assert(reverse(reverse([])) == [])
```

##

```python
def reverse_involutive(l):
  assert(reverse(reverse(l)) == l)
  
reverse_involutive([1,2,3])
reverse_involutive([1])
reverse_involutive([])
```

<div class="notes">
But that's error prone, you realise you could miss edge cases etc.
</div>

##

```python
def generate_random_lists(how_many):
  ...
  
def reverse_involutive(l):
  assert(reverse(reverse(l)) == l)

for l in generate_random_lists(100):
  reverse_involutive(l)
```

##

"This could be a library..."

<div class="notes">
You don't just need to specialise it to lists of characters in Python.

You go off and write a really nice library that does this...

...then port it to every language known to man

This happened like 20 years ago but with Haskell instead of python
</div>

##

QuickCheck!

<div class="notes">
That library's called quickcheck
</div>

## Property Testing Concepts

## Size

<div class="notes">
A generator needs to know "how big of a thing" to generate

- You might want to generate a large or small thing
- Or you might want to generate something of a consistent size no
  matter what
</div>

## Shrinking

##

Finding less complex failure cases

<div class="notes">
- Generator may generate large, complex input that fails
- Changes are there is a less complex input that also fails
- Shrinking is the process of finding that simpler failure case
</div>

##

```haskell
10
 └╼9
   └╼8
     └╼7
       └╼...
          └╼0
```

##
```haskell
[3, 6, 5]
 ├╼[3, 6]
 │  ├╼[3]
 │  │  └╼ []
 │  └╼[6]
 │     └╼ []
 ├╼[6, 3]
 │  ├╼[6]
 │  │  └╼ []
 │  └╼[3]
 │     └╼ []
...
```

##
```haskell
Just 'a'
 └╼Nothing
```
## `a -> [a]`

##

Getting good shrinking in QuickCheck takes a bit of effort

