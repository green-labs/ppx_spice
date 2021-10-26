# Spice

A ReScript PPX which generates the (polymorphic) variant (de)serializer.

> This ppx is highly influenced by [Decco](https://github.com/reasonml-labs/decco) and developed with forking the source codes of Decco.

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
