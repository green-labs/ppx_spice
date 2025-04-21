# CHANGELOG

## 0.2.6

- Fix compilation issue for records with a single field.

## 0.2.5

- Flat JSON decoders are now generated for records, with correct handling of optional and option fields. https://github.com/green-labs/ppx_spice/pull/88

## 0.2.4

- Fix critical performance issues for records with a large number of fields. https://github.com/green-labs/ppx_spice/pull/87

## 0.2.3

- Support the compiler v12-alpha.4 https://github.com/green-labs/ppx_spice/pull/82
- Support bigint type https://github.com/green-labs/ppx_spice/pull/83

## 0.2.2

- Support Null https://github.com/green-labs/ppx_spice/pull/80

## 0.2.1

- a190663 Utilize Js.Json.Boolean(bool) instead oif Js.Json.True, False https://github.com/green-labs/ppx_spice/pull/58
- a190663 Add support of uncurried mode for interface(*.resi) https://github.com/green-labs/ppx_spice/pull/58
- Support the compiler v11-rc.5 https://github.com/green-labs/ppx_spice/pull/61
- Add the feature of encoding/decoding between the number and (polymorphic)variant with `@spice.as` https://github.com/green-labs/ppx_spice/pull/64
- Fix generating encode, decode function when `@spice.as` with number https://github.com/green-labs/ppx_spice/pull/74

## 0.2.0

### Minor Changes

- 9ce55bf: Compat the untagged variant

## 0.2.0-next.0

### Minor Changes

- Compat the untagged variant

## 0.1.15

### Patch Changes

- 0e738ef: Support cli arg for uncurried mode

## 0.1.15-next.0

### Patch Changes

- Support cli arg for uncurried mode

# 0.1.14

- Support both `ns.optional` and `res.optional` for backward compatability

# 0.1.13

- Rename the attribute used for optional records from `ns.optional` to `res.optional`.

# 0.1.12

- Fix build error where `@spice.encode`, `@spice.decode` are used

# 0.1.11

- Build the executable with static linking for Linux with musl

# 0.1.10

- Build the executable with static linking for Linux

# 0.1.9

- Clean up npm publish files

# 0.1.8

- Fix type error where using tuple constructor type, such as `array<int>` for optional field in the record. https://github.com/green-labs/ppx_spice/pull/32

# 0.1.7

#### :rocket: New Feature

- Add support for the optional field record
- Add Windows platform support
