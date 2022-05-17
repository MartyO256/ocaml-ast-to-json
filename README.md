# OCaml AST to JSON

A CLI utility for converting a subset of the OCaml AST to JSON.

## Prerequisites

Install the OCaml package manager ([`opam`](http://opam.ocaml.org/)) by following the instructions at [http://opam.ocaml.org/doc/Install.html](http://opam.ocaml.org/doc/Install.html).
Then, install [`dune`](https://github.com/ocaml/dune) globally.

## Installation from the source

To build the OCaml AST to JSON binary and install it in its own `opam` switch:

```
git clone https://github.com/MartyO256/ocaml-ast-to-json.git
cd ocaml-ast-to-json
opam switch create . --empty
opam install .
```

The `parse-ocaml-to-json` executable is then accessible from the newly created `opam` switch.

## Usage

```
parse-ocaml-to-json <file> [--output-file <output>]
  --output-file Set the output file name
  -help  Display this list of options
  --help  Display this list of options
```

If the output file is not specified, then the output is printed instead.

## Table of Contents

- [OCaml AST to JSON](#ocaml-ast-to-json)
  - [Prerequisites](#prerequisites)
  - [Installation from the source](#installation-from-the-source)
  - [Usage](#usage)
  - [Table of Contents](#table-of-contents)
  - [Specification](#specification)
    - [`structure`](#structure)
    - [`structure-item`](#structure-item)
    - [`value_binding`](#value_binding)
    - [`identifier`](#identifier)
    - [`constant`](#constant)
    - [`type`](#type)
    - [`parameter`](#parameter)
    - [`argument`](#argument)
    - [`expresion`](#expresion)
    - [`branch`](#branch)
    - [`pattern`](#pattern)

## Specification

The output JSON has the format described by the following nodes.
The root node is a `structure`.

### `structure`

[`Parsetree.structure`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEstructure)

```idl
interface Structure {
    kind: "structure"
    items: [StructureItem]
}
```

### `structure-item`

[`Parsetree.structure_item`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEstructure_item)

```idl
interface StructureItem {
    kind: "structure-item"
}
```

[`Parsetree.Pstr_eval`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#Pstr_eval)

```idl
interface StructureItemEvaluation <: StructureItem {
    variant: "evaluation"
    expression: Expression
    attributes: [Attribute]
}
```

[`Parsetree.Pstr_type`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#Pstr_type)

```idl
interface StructureItemTypeDeclarations <: StructureItem {
    variant: "type-declarations"
    declarations: [TypeDeclaration]
}
```

[`Parsetree.Pstr_value`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#Pstr_value)

```idl
interface StructureItemValueDeclarations <: StructureItem {
    variant: "value-declaration"
    bindings: [ValueBinding]
}
```

[`Parsetree.Pstr_exception`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#Pstr_exception)

```idl
interface StructureItemExceptionDeclarations <: StructureItem {
    variant: "exception-declaration"
    declaration: TypeException
}
```

### `value_binding`

```idl
interface ValueBindings {
    kind: "value-bindings"
    recursive: Boolean
    bindins: [ValueBinding]
}
```

[`Parsetree.value_binding`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEvalue_binding)

```idl
interface ValueBinding {
    kind: "value-binding"
    pattern: Pattern
    expression: Expression
}
```

### `identifier`

[`Longident.t`](https://v2.ocaml.org/api/compilerlibref/Longident.html#TYPEt)

```idl
interface Identifier {
    kind: "identifier"
}
```

[`Longident.Lident`](https://v2.ocaml.org/api/compilerlibref/Longident.html#TYPEELTt.Lident)

```idl
interface LongIdentifier <: Identifier {
    variant: "long-identifier"
    value: String
}
```

[`Longident.Ldot`](https://v2.ocaml.org/api/compilerlibref/Longident.html#TYPEELTt.Ldot)

```idl
interface DotIdentifier <: Identifier {
    variant: "dot-identifier"
    head: Identifier
    tail: String
}
```

[`Longident.Lapply`](https://v2.ocaml.org/api/compilerlibref/Longident.html#TYPEELTt.Lapply)

```idl
interface ApplyIdentifier <: Identifier {
    variant: "apply-identifier"
    head: Identifier
    tail: Identifier
}
```

### `constant`

[`Parsetree.constant`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEconstant)

```idl
interface Constant {
    kind: "constant"
}
```

[`Parsetree.Pconst_char`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTconstant.Pconst_char)

```idl
interface CharacterConstant <: Constant {
    variant: "character"
    value: Char
}
```

[`Parsetree.Pconst_string`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTconstant.Pconst_string)

```idl
interface StringConstant <: Constant {
    variant: "string"
    value: String
}
```

[`Parsetree.Pconst_integer`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTconstant.Pconst_integer)

```idl
interface IntegerConstant <: Constant {
    variant: "integer"
    value: Int
}
```

[`Parsetree.Pconst_float`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTconstant.Pconst_float)

```idl
interface FloatConstant <: Constant {
    variant: "float"
    value: Float
}
```

### `type`

[`Parsetree.core_type_desc`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEcore_type_desc)

```idl
interface Type {
    kind: "type"
}
```

```idl
interface LabelledType <: Type {
    variant: "labelled"
    label: String
    type: Type
}
```

```idl
interface OptionalType <: Type {
    variant: "optional"
    label: String
    type: Type
}
```

[`Parsetree.Ptyp_arrow`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTcore_type_desc.Ptyp_arrow)

```idl
interface ArrowType <: Type {
    variant: "arrow"
    left: Type
    right: Type
}
```

[`Parsetree.Ptyp_alias`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTcore_type_desc.Ptyp_alias)

```idl
interface TypeAlias <: Type {
    variant: "alias"
    type: Type
    alias: String
}
```

[`Parsetree.Ptyp_any`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTcore_type_desc.Ptyp_any)

```idl
interface AnyType <: Type {
    variant: "any"
}
```

[`Parsetree.Ptyp_var`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTcore_type_desc.Ptyp_var)

```idl
interface TypeVar <: Type {
    variant: "variable"
    identifier: String
}
```

[`Parsetree.Ptyp_tuple`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTcore_type_desc.Ptyp_tuple)

```idl
interface TupleType <: Type {
    variant: "tuple"
    items: [Type]
}
```

[`Parsetree.Ptyp_constr`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTcore_type_desc.Ptyp_constr)

```idl
interface ConstructedType <: Type {
    variant: "constructed"
    constructor: Identifier
    arguments: [Type]
}
```

### `parameter`

```idl
interface Parameter {
    kind: "parameter"
}
```

```idl
interface BaseParameter <: Parameter {
    variant: "base"
    pattern: Pattern
}
```

```idl
interface OptionalParameter <: Parameter {
    variant: "optional"
    label: String
    pattern: Pattern?
    default: Expression?
}
```

```idl
interface LabelledParameter <: Parameter {
    variant: "labelled"
    label: String
    pattern: Pattern?
}
```

### `argument`

```idl
interface Argument {
    kind: "argument"
}
```

```idl
interface BaseArgument <: Argument {
    variant: "base"
    pattern: Pattern
}
```

```idl
interface OptionalArgument <: Argument {
    variant: "optional"
    label: String
    expression: Expression
}
```

```idl
interface LabelledArgument <: Argument {
    variant: "labelled"
    label: String
    expression: Expression
}
```

### `expresion`

[`Parsetree.expression_desc`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEexpression_desc)

```idl
interface Expression {
    kind: "expression"
}
```

[`Parsetree.Pexp_fun`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTexpression_desc.Pexp_fun)

```idl
interface FunExpression <: Expression {
    variant: "fun"
    parameter: Parameter
    body: Expression
}
```

[`Parsetree.Pexp_function`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTexpression_desc.Pexp_function)

```idl
interface FunctionExpression <: Expression {
    variant: "function"
    branches: [Branch]
}
```

[`Parsetree.Pexp_match`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTexpression_desc.Pexp_match)

```idl
interface MatchExpression <: Expression {
    variant: "pattern-matching"
    branches: [Branch]
}
```

[`Parsetree.Pexp_try`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTexpression_desc.Pexp_try)

```idl
interface TryExpression <: Expression {
    variant: "try-with"
    body: Expression
    catch: [Branch]
}
```

[`Parsetree.Pexp_let`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTexpression_desc.Pexp_let)

```idl
interface LetExpression <: Expression {
    variant: "let"
    bindings: [ValueBinding]
    body: Expression
}
```

[`Parsetree.Pexp_apply`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTexpression_desc.Pexp_apply)

```idl
interface ApplyExpression <: Expression {
    variant: "apply"
    head: Expression
    arguments: [Argument]
}
```

[`Parsetree.Pexp_setfield`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTexpression_desc.Pexp_setfield)

```idl
interface MutateExpression <: Expression {
    variant: "mutation"
    target: Expression
    field: Identifier
    replacement: Expression
}
```

[`Parsetree.Pexp_ifthenelse`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTexpression_desc.Pexp_ifthenelse)

```idl
interface IfThenExpression <: Expression {
    variant: "if-then"
    condition: Expression
    branch: Expression
}
```

```idl
interface IfThenElseExpression <: Expression {
    variant: "if-then-else"
    condition: Expression
    branch1: Expression
    branch2: Expression
}
```

[`Parsetree.Pexp_sequence`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTexpression_desc.Pexp_sequence)

```idl
interface SequenceExpression <: Expression {
    variant: "sequenceing"
    statements: [Expression]
}
```

[`Parsetree.Pexp_field`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTexpression_desc.Pexp_field)

```idl
interface FieldAccessExpression <: Expression {
    variant: "field-access"
    record: Expression
    field: Identifier
}
```

[`Parsetree.Pexp_construct`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTexpression_desc.Pexp_construct)

```idl
interface ConstructExpression <: Expression {
    variant: "construct"
    constructor: Identifier
    argument: Expression?
}
```

[`Parsetree.Pexp_ident`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTexpression_desc.Pexp_ident)

```idl
interface IdentifierExpression <: Expression {
    variant: "identifier"
    value: Identifier
}
```

[`Parsetree.Pexp_constant`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTexpression_desc.Pexp_constant)

```idl
interface ConstantExpression <: Expression {
    variant: "constant"
    value: Constant
}
```

[`Parsetree.Pexp_tuple`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTexpression_desc.Pexp_tuple)

```idl
interface TupleExpression <: Expression {
    variant: "tuple"
    items: [Expression]
}
```

[`Parsetree.Pexp_constraint`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTexpression_desc.Pexp_constraint)

```idl
interface ConstraintExpression <: Expression {
    variant: "constraint"
    expression: Expression
    constraint: Type
}
```

[`Parsetree.Pexp_coerce`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTexpression_desc.Pexp_coerce)

```idl
interface CoerceExpression <: Expression {
    variant: "coercion"
    expression: Expression
    from: Type?
    to: Type
}
```

[`Parsetree.Pexp_record`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTexpression_desc.Pexp_record)

```idl
interface RecordExpression <: Expression {
    variant: "record"
    original: Expression?
    fields: [{ field: Identifier, value: Expression }]
}
```

### `branch`

```idl
interface Branch {
    kind: "pattern-matching-branch"
    pattern: Pattern
    branch: Expression
    guard: Expression?
}
```

### `pattern`

[`Parsetree.pattern_desc`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEpattern_desc)

```idl
interface Pattern {
    kind: "pattern"
}
```

[`Parsetree.Ppat_alias`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTpattern_desc.Ppat_alias)

```idl
interface AliasPattern <: Pattern {
    variant: "alias"
    pattern: Pattern
    alias: Identifier
}
```

[`Parsetree.Ppat_or`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTpattern_desc.Ppat_or)

```idl
interface OrPattern <: Pattern {
    variant: "or-pattern"
    patterns: [Pattern]
}
```

[`Parsetree.Ppat_variant`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTpattern_desc.Ppat_variant)

```idl
interface VariantPattern <: Pattern {
    variant: "polymorphic-variant-pattern"
    tag: String
    argument: Pattern
}
```

[`Parsetree.Ppat_construct`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTpattern_desc.Ppat_construct)

```idl
interface ConstructorPattern <: Pattern {
    variant: "constructor-pattern"
    identifier: Identifier
    argument: Pattern?
}
```

[`Parsetree.Ppat_any`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTpattern_desc.Ppat_any)

```idl
interface AnyPattern <: Pattern {
    variant: "any"
}
```

[`Parsetree.Ppat_var`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTpattern_desc.Ppat_var)

```idl
interface VarPattern <: Pattern {
    variant: "variable-pattern"
    identiifer: Identifier
}
```

[`Parsetree.Ppat_record`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTpattern_desc.Ppat_record)

```idl
interface RecordPattern <: Pattern {
    variant: "record"
    close: Boolean
    record: [{ label: Identifier, pattern: Pattern }]
}
```

[`Parsetree.Ppat_tuple`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTpattern_desc.Ppat_tuple)

```idl
interface TuplePattern <: Pattern {
    variant: "tuple"
    patterns: [Pattern]
}
```

[`Parsetree.Ppat_constant`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTpattern_desc.Ppat_constant)

```idl
interface ConstantPattern <: Pattern {
    variant: "constant"
    value: Constant
}
```

[`Parsetree.Ppat_interval`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTpattern_desc.Ppat_interval)

```idl
interface IntervalPattern <: Pattern {
    variant: "interval"
    left: Constant
    right: Constant
}
```

[`Parsetree.Ppat_constraint`](https://v2.ocaml.org/api/compilerlibref/Parsetree.html#TYPEELTpattern_desc.Ppat_constraint)

```idl
interface ConstraintPattern <: Pattern {
    variant: "constraint"
    left: Pattern
    right: Type
}
```
