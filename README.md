# Spice

A ReScript PPX, which generates the (polymorphic) variant (de)serializer.

`Spice` is originated from
* The `Spice melange` in the novel, Dune
* A flavor for the (polymorphic) variant

> This PPX is highly influenced by [Decco](https://github.com/reasonml-labs/decco) and developed with forking the source codes of Decco.

## Motivation

1. Parse the string instead of the array

To parse the JSON data, [Decco](https://github.com/reasonml-labs/decco) is heavily used in the projects. There's a restriction to parse the JSON string into a variant. With the Decco, the data should be formed in an array. It is obvious at some point. But generally, we face the string data which needs to be parsed into the variant in most use cases.

2. Parse/stringify the Unicode string

There are many cases to parse and stringify the string data against (polymorphic) variants. Furthermore, the Unicode string needs to be handled with a variant. Currently, pattern matching is not working for the Unicode string in ReScript, the Spice is using `if ... then ... else` to compare the Unicode string.

Example
```rescript
@spice
type t = | @spice.as(`하나`) One | @spice.as(`second`) Two

// automatically generated
let t_encode = ...

// automatically generated
let t_decode = ...

let encoded = One->t_encode // `하나`

let decoded = `second`->t_decode // Belt.Result.Ok(Two)
```

## Install

```
yarn add -D @greenlabs/ppx-spice
```

```
// bsconfig.json
"ppx-flags": [
  ...,
  "@greenlabs/ppx-spice/ppx"
],
```

## Development

1. Install dependencies

```
cd src
esy install
```

2. Build

```
(make sure in /src directory)
esy build
```

3. Test

```
cd test

(install dependencies)
yarn

(build --watch)
yarn res:clean && yarn res:watch

(run test --watch)
yarn test:watch
```
