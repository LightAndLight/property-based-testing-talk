# hpython

##

- AST
- Parser
- Pretty printer

##

AST should be syntactically correct by construction

##

Transform the grammar into an AST, right?

##

Nope

##

Python has a few "tiers" of grammar

<div class="notes">
Python language reference not great for language tools developers

There's like 3 levels of grammar-
The grammar that they use to generate their parser
The "pretend grammar", which is extra rules
Then some extra special rules on top of that that you have to have to
  discover accidentally

It's scattered all across the language reference

If you implemented python from scratch by reading the reference, you'd
end up with something better than python3, by accident
</div>

##

How could I possibly determine what is or isn't valid Python?

<div class="notes">
Rather than fretting, and poring though the language reference trying to make
sure I was doing everything properly

I decided to let the computer figure that out for me
</div>

##

```haskell
prop_ast_is_valid_python :: Property
prop_ast_is_valid_python =
  property $ do
    program <- forAll genPythonProgram
    let printed = printPythonProgram expr
    res <- liftIO $ checkSyntax program
    case res of
      SyntaxError -> failure
      SyntaxCorrect -> success
```

<div class="notes">
I can generate random python programs and feed them into `python3`,
and check if they cause a syntax error.

Then I just continued implementing the official grammar, and leaned on
the automatic shrinking to tell me what I did wrong
</div>

##

Shrinking!

<div class="notes">
The shrinking catches edge cases like flies.

If I messed up, or there were hidden syntax rules, hedgehog gave me really
specific counter examples that I could easily find the solution for
</div>

##

<img src="img/hedgehog_await.png" />

##

```python
*A for A in A
```

##

```python
lambda *:A
```

##

This would have been a lot slower with QuickCheck

##

I wouldn't have even considered doing it without some kind of property testing

##

It has saved many man-hours of test-writing and debugging

## Property testing is great!

## Hedgehog is great!

## Thanks!

