# Syntaxfix

## Progress

### Language

- [ ] Arithmetic
  - [x] Addition
  
    ```javascript
    1 + 2
    ```
    
  - [x] Subtraction
    
    ```javascript
    2 - 1
    ```
    
  - [x] Multiplication
  
    ```javascript
    2 * 2
    ```
    
  - [x] Divison
  
    ```javascript
    10 / 2
    ```
    
  - [ ] Modulos
    
    ```javascript
    11 % 2
    ```
    
  - [ ] Exponent
  
    ```haskell
    10 ^ 2
    ```
    
- [ ] Functions
  - [x] Top level definiton with N params
    
    ```haskell
    greet name = "Hello " + name
    
    greetMomAndDad mom dad =
        "Hello {{mom}} and {{dad}}!"
    ```
    
  - [x] `where`-notation (local variables)
  
    ```haskell
    greet name =
        greeting
      where
        greeting = "Hello {{name}}!"
    ```
    
  - [ ] Pattern matching
  
    ```haskell
    greet "Gunnar" = "I don't like you."
    greet name     = "Hello {{name}}!"
    ```
    
  - [ ] Type annotation
  
    ```haskell
    greet :: String -> String
    greet name = "Hello {{name}}!"
    ```
    
  - [ ] Lambda
  
    ```haskell
    x y -> x ^ y
    ```
    
  - [ ] `this`-bound lambda
  
    ```haskell
    x y => x ^ y
    ```
    
  - [ ] Add pipe (`|>`) operator
  
    ```haskell
    [1, 2, 3]
        |> map (x -> x ^ 2)
        |> reduce (+)
    ```
    
  - [ ] Add prototype pull (`..`) operator
  
    ```haskell
    replace "Hej" "Hello" .. "Hej Gunnar!"
    ```
    
- [ ] Constants
  - [ ] Top level definition
  
    ```haskell
    const HOWDY = "Hello"
    
    const
        COW = 1
        CAT = 2
    ```
    
  - [ ] Inferred type
- [ ] Mutables
  - [ ] Top level definition
  
    ```haskell
    mutable howdy = "Hello"
    
    mutable
        cow = 1
        cat = 2
    ```
    
  - [ ] Inferred type
- [ ] Classes
  - [ ] Top level definition
  
    ```haskell
    class MyComponent extends Component
        
        render =
            el "div" {}
                [ el "span" {} [ "hello" ]
                , el "span" {} [ "there" ]
                ]
    ```
  - [ ] Method definition (see functions)
  - [ ] Static property definition
  - [ ] Constructor
- [ ] Modules
  - [x] Import JS modules
  
    ```haskell
    fromjs react             import (default as React) Component
    fromjs utils/preferences import get set remove
    ```
    
  - [ ] Import Syntaxfix modules
  
    ```haskell
    from Utils/Math import randBetween uniqueRand
    ```
    
  - [ ] Import type info
  - [ ] Define module name (enforce consistency with filename)
  
    ```haskell
    -- Test.sf
    module Test exports something
    ```
    
  - [x] Specify exports
  
    ```haskell
    module MyModule exports greet meet
    ```
- [ ] Types
  - [ ] Support for primitive types (String, Int, Float, Bool, Void)
  - [ ] Support for complex types (List, Dict)
  - [ ] Support for `Any` type
  - [ ] Inference of variable types
  - [ ] Solve all typed expressions
  
### CLI

TBD
