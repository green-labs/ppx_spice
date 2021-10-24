# Spice

A ReScript PPX which generates the (polymorphic) variant (de)serializer.

Example
```rescript
@spice
type t = | @spice.as(`first`) One | @spice.as(`second`) Two

// automatically generated
let t_encode = ...

// automatically generated
let t_decode = ...

let encoded = Sample2.One->t_encode // `first`->Js.Json.string

let decoded = `second`->Js.Json.string->t_decode // Result.Ok(Two)
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
