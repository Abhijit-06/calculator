# Functional patterns

In the following repository you will find some notes on usefull and recommended practices for common functional programming patterns. [Haskell](https://wiki.haskell.org/Haskell) has been used for its compact syntax and its [Hindley-Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) type system so bear in mind that for other language implementations the actual details of the pattern implementation could differ and make use of specific language characterstics.

# Table of Contents
1. [Newtype](#Newtype)
2. [Smart Constructor](#smart-constructor)
3. [Evidence](#evidence)
4. [Illegal states unrepresentable](#illegal-states-unrepresentable)
5. [Phantom types](ohantom-types)
6. [MonadFail sugar](monadfail-sugar)
7. [Polymorphisation](polymorphisation)
8. [Bidirectional parsing](bidirectional-parsing)
9. [Recursive go](recursive-go)

## Newtype

| Pattern | Newtype |
| ------ | ------ |
| Description | Lightweight data wrapper |
|  When to use  | When you want primitive types represent semantically different entities and make use of the type system |
| Benefits | Improves maintainability, increases code readability, static analysing, type inference... |
| Costs | Additional wrapping and unwrapping, boilerplate |


## Smart Constructor

| Pattern | Smart Constructor |
| ------ | ------ |
| Description | Provide and idomatic way to construct values |
|  When to use  | Restricted data type values, complex constructors, avoiding runtime errors, illegal state unrepresentable |
| Benefits | Improves maintainability, structured code, concern separation, control errors in data inputs |
| Costs | Extra code, force implementation details / design |

## Evidence

| Pattern | Evidence |
| ------ | ------ |
| Description | Validation pattern |
|  When to use  | Data needs to be validated, illegal state unrepresentable |
| Benefits | Robust code, easier to refactor, more context / info on date, detailed error messages |
| Costs | More code and different mindset (more FPy) |

## Illegal states unrepresentable

| Pattern | Illegal states unrepresentable |
| ------ | ------ |
| Description | Applied domain driven design for your types |
|  When to use  |Restrict domain values, for the domain layer, adaptors and validations |
| Benefits | Correctness, domain is more accurate, less tests, harder to introduce bugs |
| Costs | More code and harder to introduce logical changes |

## Phantom types

| Pattern | Phantom types |
| ------ | ------ |
| Description | Additional type-level information available during compile-time |
|  When to use  |To increase code safety and avoid type duplicatoin (similar data types) |
| Benefits | Compile-time guarantees, flexebility and ergonomics |
| Costs | Advanced concept, can get out of hand easily |

## MonadFail sugar

| Pattern | MonadFail sugar |
| ------ | ------ |
| Description | Elegant syntax for pattern-matching on nested or multiple different parts of the data |
|  When to use  |When doing pattern-matching a lot and when a particular failure reason is not importan |
| Benefits | Clean syntax, no runtime errors by avoiding partial functions |
| Costs | No detailed error messages, harder to reason |

## Polymorphisation

| Pattern | Polymorphisation |
| ------ | ------ |
| Description | Assigning a more general type to a function reduces the chances of writing an incorrect implementation or providing incorrect inputs |
|  When to use  | To make illegal state unrepresnetable, avoid input error, easier logic reasoning |
| Benefits |Increase code reusability |
| Costs |Type signatures may look more complicated, can be confusing |

## Bidirectional parsing

| Pattern | Bidirectional parsing |
| ------ | ------ |
| Description |Matching only a limited set with exhaustiveness checking and inversing matching function automatically |
|  When to use  | For any bidirectional conversion by implementing only one direction and getting the inverse conversion for free |
| Benefits | Automatic code correctness |
| Costs |Extra code, less manual control|


## Recursive go

| Pattern |  Recursive go |
| ------ | ------ |
| Description | Moving recursion over data types into the separate function|
|  When to use  | Recursion with internal state |
| Benefits | Clean code, possible performance improvements |
| Costs |Extra code|

### Sources and extra reading
* [Haskell mini patterns by Kowainik](https://kowainik.github.io/posts/haskell-mini-patterns)
* [SICP on linear recursion and iteration](https://mitpress.mit.edu/sites/default/files/sicp/full-text/sicp/book/node15.html)
* [Phantom types in rust](https://bluishcoder.co.nz/2013/08/15/phantom_types_in_rust.html)
* [Designing with types: Making illegal states unrepresentable](https://fsharpforfunandprofit.com/posts/designing-with-types-making-illegal-states-unrepresentable/)
* [DDD](https://fsharpforfunandprofit.com/ddd/)
* 