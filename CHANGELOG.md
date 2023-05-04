# CHANGELOG

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
