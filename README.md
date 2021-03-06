# Syntaxfix

## Progress

### Language

- [x] Arithmetic
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
    
  - [x] Modulos
    
    ```javascript
    11 % 2
    ```
    
  - [x] Exponent
  
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
    
  - [x] Lambda
  
    ```haskell
    \x y -> x ^ y
    ```
    
  - [ ] `this`-bound lambda
  
    ```haskell
    \x y => x ^ y
    ```
    
  - [ ] Add pipe (`|>`) operator
  
    ```haskell
    [1, 2, 3]
        |> map (x -> x ^ 2)
        |> reduce (+)
    ```
    
  - [ ] Add instance method pull (`..`) operator
  
    ```haskell
    replace "Hej" "Hello" .. "Hej Gunnar!"
    ```
    
- [ ] Constants
  - [x] Top level definition
  
    ```haskell
    const greeting = "Hello"
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
  
    Unlike ES modules, modules is Syntaxfix are referenced relative to the **Main module**. That is, if you're writing a module that has the file path (relative to your project root) *./Utils/Screen.sf* and you want to import *./Utils/Math.sf*, you still write as follows.
  
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
    
  - [x] Specify runnable module
  
    ```haskell
    module Main runs main
    ```
    
- [ ] Types
  - [ ] Support for primitive types (String, Int, Float, Bool, Void)
  - [ ] Support for complex types (List, Dict)
  - [ ] Support for `Any` type
  - [ ] Inference of variable types
  - [ ] Solve all typed expressions
  
### CLI

TBD
