# INF222-V24, Obligatory 1

## Submission meta-info

* Student's name: Helene Solhaug
* Student's UiB user handle: hesol8564
* Last digit: 4
* Concrete syntax choice (based on the last digit):
    * Pascal-like (begin ... end)
    * English-like syntax for arithmetic expressions

## Abstract syntax in Haskell

```haskell
module Hellang where

data Program = Program [Statement]

data Statement
  = VariableDeclaration Identifier (Maybe Expression)
  | ArrayDeclaration Identifier
  | Conditional Expression [Statement] [(Expression, [Statement])] (Maybe [Statement])
  | Loop Expression [Statement] [LoopControl]
  | FunctionDeclaration Identifier [Parameter] [Statement]
  | ClassDeclaration Identifier [ClassElement] [Identifier]
  | Block [Statement]

data Expression
  = Literal Value
  | Variable Identifier
  | BinaryOperation Op Expression Expression
  | UnaryOperation Op Expression
  | FunctionCall Identifier [Expression]
  | ArrayAccess Expression Expression

data Parameter = Parameter Identifier (Maybe Expression)

data ClassElement
  = FieldDeclaration Identifier
  | MethodDeclaration Identifier [Parameter] [Statement]
  | PropertyDeclaration Identifier (Maybe [Statement]) (Maybe [Statement])

data LoopControl
  = Break
  | Continue
  | Return (Maybe Expression)

type Identifier = String
type Value = Int | Bool | String
data Op = Add | Subtract | Multiply | Divide | And | Or | Not | Equal | LessThan | GreaterThan
```

## Abstract syntax in Zephyr ASDL
```
module Hellang

type program = [statement]

type statement =
    VariableDeclaration(identifier name, expression? value)
  | ArrayDeclaration(identifier name)
  | Conditional(expression condition, [statement] then_branch, [(expression, [statement])] elif_branches, [statement]? else_branch)
  | Loop(expression condition, [statement] body, [loop_control] controls)
  | FunctionDeclaration(identifier name, [parameter] params, [statement] body)
  | ClassDeclaration(identifier name, [class_element] elements, [identifier] bases)
  | Block([statement] statements)

type expression =
    Literal(value val)
  | Variable(identifier name)
  | BinaryOperation(op operator, expression left, expression right)
  | UnaryOperation(op operator, expression operand)
  | FunctionCall(identifier name, [expression] args)
  | ArrayAccess(expression array, expression index)

type parameter = Parameter(identifier name, expression? default_value)

type class_element =
    FieldDeclaration(identifier name)
  | MethodDeclaration(identifier name, [parameter] params, [statement] body)
  | PropertyDeclaration(identifier name, [statement]? getter, [statement]? setter)

type loop_control = Break | Continue | Return(expression? value)

type identifier = string
type value = int | bool | string
type op = Add | Subtract | Multiply | Divide | And | Or | Not | Equal | LessThan | GreaterThan

```

## Concrete syntax: grammar

```
Program = { Statement } ;

Statement =
    VariableDeclaration
  | ArrayDeclaration
  | Conditional
  | Loop
  | FunctionDeclaration
  | ClassDeclaration
  | Block ;

VariableDeclaration = "var", Identifier, [ ":=", Expression ], ";" ;
ArrayDeclaration = "array", Identifier, ";" ;
Conditional = "if", "(", Expression, ")", "then", Block, { "else if", "(", Expression, ")", "then", Block }, [ "else", Block ] ;
Loop = "while", "(", Expression, ")", "do", "begin", { Statement }, { LoopControl }, "end" ;
FunctionDeclaration = "function", Identifier, "(", [ Parameters ], ")", "begin", Block, "end" ;
ClassDeclaration = "class", Identifier, [ "extends", Identifiers ], "begin", { ClassBodyElement }, "end" ;
Block = "begin", { Statement }, "end" ;

Expression =
    Literal
  | Variable
  | BinaryExpression
  | UnaryExpression
  | FunctionCall
  | ArrayAccess ;

Literal = IntegerLiteral | BooleanLiteral | StringLiteral ;
Variable = Identifier ;
BinaryExpression = Expression, Operator, Expression ;
UnaryExpression = "not", Expression ;
FunctionCall = Identifier, "(", [ Expressions ], ")" ;
ArrayAccess = Identifier, "[", Expression, "]" ;

Parameters = Parameter, { ",", Parameter } ;
Parameter = Identifier, [ ":=", Expression ] ;

ClassBodyElement =
    FieldDeclaration
  | MethodDeclaration
  | PropertyDeclaration ;

FieldDeclaration = "var", Identifier, ";" ;
MethodDeclaration = "function", Identifier, "(", [ Parameters ], ")", "begin", Block, "end" ;
PropertyDeclaration = "property", Identifier, "begin", [ Getter ], [ Setter ], "end" ;

Getter = "get", "begin", Block, "end" ;
Setter = "set", "begin", Block, "end" ;

LoopControl = "break", ";" | "continue", ";" | "return", [ Expression ], ";" ;

Operator = "plus" | "minus" | "times" | "divided by" | "and" | "or" | "equal to" | "less than" | "greater than" ;

Identifier = letter, { letter | digit | "_" } ;
IntegerLiteral = digit, { digit } ;
BooleanLiteral = "true" | "false" ;
StringLiteral = "\"", { any character except "\"" }, "\"" ;

letter = "a" ... "z" | "A" ... "Z" ;
digit = "0" ... "9" ;
```

## Sample program

```
var x := 10;
array numbers;

function sum(a, b: Integer): Integer;
begin
  var result := a plus b;
  return result;
end;

class Greeter
begin
  var message: String;

  function greet(name: String);
  begin
    message := "Hello, " plus name;
  end;

  property greeting
  begin
    get
    begin
      return message;
    end;
    set
    begin
      message := value;
    end;
  end;
end;

if x equal to 10 then
begin
  var y := sum(x, 20);
  greet("World");
end
else
begin
  x := 0;
end;

while x less than 100 do
begin
  x := x plus 2;
  if x equal to 50 then
  begin
    break;
  end;
end;

```

## Reflections on language design, orthogonality

(your answers here)


