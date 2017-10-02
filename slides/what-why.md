# What is property based testing?

##

"Testing the mathematical properties of a system"

##

What are mathematical properties?

## 

"General relationships between the inputs and outputs of operations"

<div class="notes">
So what does this mean in the context of programming?
</div>

##

`reverse(reverse(l)) = l`

<div class="notes">
Consider a function that reverses a list

It is its own inverse

This means it is involutive
</div>

##

`delete(key, delete(key, d)) = delete(key, d)`

<div class="notes">
A function that deletes a key from a dictionary

Deleting a key from a dictionary once should have the same result
as deleting it multiple times

It is idempotent
</div>

##

`2 + 3 = 3 + 2`

<div class="notes">
Adding numbers

The order in which you add them doesn't matter

This is commutativity
</div>

##

`>>> listOf(5, 'a')`
`[ 'a', 'a', 'a', 'a', 'a' ]`

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

These are all behaviours that we take for granted

##

Made explicit using mathematical notation

<div class="notes">
Some are a bit averse at the thought of approaching software dev
from a more mathematical perspective...
</div>

## 

Mathematics will help you write better software

##

More intuitive APIs

<div class="notes">
- By keeping "common sense" properties in mind, you'll develop more
  intuitive APIs
- You can leverage mathematical properties to make APIs easier to understand
  because people are already so intimately familiar with these concepts
</div>

##

More guarantees

<div class="notes">
- Consumers of your libraries will have more confidence because you've given
  them guarantees about how your system behaves
</div>

##

Another way to think about your code

<div class="notes">
- The process of thinking about properties of your code require a deep understanding
  of what that code does
- You will likely find and fix bugs before the testing stage because you're being
  so analytical
</div>

##

Easier to test

<div class="notes">
The real meat of this talk:

- Mathematics is relatively unambiguous
- Easier to get a computer to interpret and check your descriptions
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

##

Hedgehog!!!

<div class="notes">
But I'm going to talk about Hedgehog, which is a more modern version
</div>
