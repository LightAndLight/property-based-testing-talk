# What is property based testing?

##

"Testing the mathematical properties of a system"

##

Your systems __do__ have mathematical properties

##

`reverse(reverse(l)) = l`

<div class="notes">
- A function that reverses a list
- It is involutive
- An operation that is its own inverse
</div>

##

`delete("a", delete("a", d)) = delete("a", d)`

<div class="notes">
- A function that deletes a key from a dictionary
- It is idempotent
- Running the operation many times gives the same result
  as running it once
</div>

##

`2 + 3 = 3 + 2`

<div class="notes">
- Adding numbers
- It is commutative
- The operation doesn't care about the order of its arguments
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
This property seems pretty obvious.

So a "property" in this talk will basically mean...
</div>

##

Mathematical descriptions of a system's behaviour

<div class="notes">
Some of you might... flinch? at the thought of approaching software dev
from a more mathematical perspective...
</div>

##

"...Too formal!"

##

"...Not practical!"

<div class="notes">
A more formal approach will have at least two benefits

(These are my favourite two, feel free to think of more)
</div>

##

1. Better code

<div class="notes">
- By keeping "common sense" properties in mind, you'll develop more
  intuitive APIs
- Consumers of your libraries will have more confidence because you've given
  them guarantees about how your system behaves
- "Does this function care about the order of its arguments?"
- "Should it?"
- Gives you a new way to check if you're on the right track
- It's kind of like design patterns, except people started thinking about item
  thousands of years ago
</div>

##

2. Easy to automate testing

<div class="notes">
The real meat of this talk:

- Mathematics is relatively unambiguous
- Easier to get a computer to interpret and check your descriptions
</div>

##

"This function will never return null except on Tuesdays"

<div class="notes">
Good luck getting your repl to understand that!
</div>

## Property-based __testing__

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

##

```python
def generate_random_lists(how_many):
  ...
  
def reverse_involutive(l):
  assert(reverse(reverse(l)) == l)

for l in generate_random_lists(100):
  reverse_involutive(l)
```
