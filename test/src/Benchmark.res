// Benchmark file with representative types to test ppx performance
// Trimmed version covering key patterns: records, variants, polyvariants, tuples

// ============================================================================
// Simple Records (4-5 fields)
// ============================================================================

@spice
type user = {
  id: int,
  name: string,
  email: string,
  active: bool,
}

@spice
type timestamp = {
  seconds: int,
  nanos: int,
}

@spice
type metadata = {
  createdAt: timestamp,
  updatedAt: timestamp,
  version: int,
}

// ============================================================================
// Records with Optional Fields
// ============================================================================

@spice
type profile = {
  userId: int,
  displayName: string,
  bio: option<string>,
  avatarUrl: option<string>,
  website: option<string>,
}

// ============================================================================
// Nested Records
// ============================================================================

@spice
type order = {
  id: int,
  customer: user,
  total: float,
  metadata: metadata,
}

// ============================================================================
// Variants
// ============================================================================

@spice
type status =
  | Active
  | Inactive
  | Pending

// ============================================================================
// Variants with Payloads
// ============================================================================

@spice
type notification =
  | Email(string)
  | SMS(string)
  | Push(string, string)

// ============================================================================
// Polyvariants
// ============================================================================

@spice
type color = [
  | #Red
  | #Green
  | #Blue
  | #Yellow
]

// ============================================================================
// Polyvariants with @spice.as
// ============================================================================

@spice
type logLevel = [
  | @spice.as("debug") #Debug
  | @spice.as("info") #Info
  | @spice.as("warn") #Warn
  | @spice.as("error") #Error
]

// ============================================================================
// Generic Types
// ============================================================================

@spice
type wrapper<'a> = {
  data: 'a,
  timestamp: int,
}

@spice
type response<'a> = {
  success: bool,
  data: option<'a>,
  error: option<string>,
}

// ============================================================================
// Records with @spice.key
// ============================================================================

@spice
type apiUser = {
  @spice.key("user_id") userId: int,
  @spice.key("first_name") firstName: string,
  @spice.key("last_name") lastName: string,
}

// ============================================================================
// Records with @spice.default
// ============================================================================

@spice
type configWithDefaults = {
  host: string,
  @spice.default(3000) port: int,
  @spice.default(false) debug: bool,
}

// ============================================================================
// Medium Record (10 fields - tests O(n²) scaling)
// ============================================================================

@spice
type mediumRecord = {
  field1: string,
  field2: string,
  field3: int,
  field4: int,
  field5: float,
  field6: float,
  field7: bool,
  field8: bool,
  field9: option<string>,
  field10: option<int>,
}

// ============================================================================
// Large Record (20 fields - noticeable O(n²) impact, 400 pattern nodes)
// ============================================================================

@spice
type largeRecord = {
  a1: string,
  a2: string,
  a3: string,
  a4: string,
  b1: int,
  b2: int,
  b3: int,
  b4: int,
  c1: float,
  c2: float,
  c3: float,
  c4: float,
  d1: bool,
  d2: bool,
  d3: bool,
  d4: bool,
  e1: option<string>,
  e2: option<int>,
  e3: option<bool>,
  e4: option<float>,
}

// ============================================================================
// Very Large Record (50 fields - stress test for O(n²) complexity)
// ============================================================================

@spice
type veryLargeRecord = {
  // String fields (10)
  s1: string,
  s2: string,
  s3: string,
  s4: string,
  s5: string,
  s6: string,
  s7: string,
  s8: string,
  s9: string,
  s10: string,
  // Int fields (10)
  i1: int,
  i2: int,
  i3: int,
  i4: int,
  i5: int,
  i6: int,
  i7: int,
  i8: int,
  i9: int,
  i10: int,
  // Float fields (10)
  f1: float,
  f2: float,
  f3: float,
  f4: float,
  f5: float,
  f6: float,
  f7: float,
  f8: float,
  f9: float,
  f10: float,
  // Bool fields (10)
  b1: bool,
  b2: bool,
  b3: bool,
  b4: bool,
  b5: bool,
  b6: bool,
  b7: bool,
  b8: bool,
  b9: bool,
  b10: bool,
  // Optional fields (10)
  o1: option<string>,
  o2: option<int>,
  o3: option<float>,
  o4: option<bool>,
  o5: option<string>,
  o6: option<int>,
  o7: option<float>,
  o8: option<bool>,
  o9: option<string>,
  o10: option<int>,
}

// ============================================================================
// Tuples
// ============================================================================

@spice
type point2D = (float, float)

@spice
type point3D = (float, float, float)

@spice
type rgba = (int, int, int, float)

// ============================================================================
// Large Tuple (10 elements - tests O(n²) in tuple.ml)
// ============================================================================

@spice
type tuple10 = (string, string, int, int, float, float, bool, bool, string, int)

// ============================================================================
// Larger Tuple (15 elements - more O(n²) stress)
// ============================================================================

@spice
type tuple15 = (
  string, string, string, string, string,
  int, int, int, int, int,
  float, float, float, float, float,
)

// ============================================================================
// Very Large Tuple (20 elements - significant O(n²) in tuple.ml)
// ============================================================================

@spice
type tuple20 = (
  string, string, string, string, string,
  string, string, string, string, string,
  int, int, int, int, int,
  int, int, int, int, int,
)

// ============================================================================
// Variants with Many Constructors (tests variant decoder switch)
// ============================================================================

@spice
type largeVariant =
  | V1
  | V2
  | V3
  | V4
  | V5
  | V6
  | V7
  | V8
  | V9
  | V10
  | V11
  | V12
  | V13
  | V14
  | V15

// ============================================================================
// Variant with Large Payload (tests O(n²) in variants.ml generate_arg_decoder)
// ============================================================================

@spice
type variantWithLargePayload =
  | NoPayload
  | SmallPayload(string, int)
  | MediumPayload(string, string, int, int, float, float)
  | LargePayload(
      string, string, string, string,
      int, int, int, int,
      float, float, float, float,
    )

// ============================================================================
// Variant with Very Large Payload (20 args - matches record test)
// ============================================================================

@spice
type variantWithHugePayload =
  | Empty
  | Huge(
      string, string, string, string, string,
      int, int, int, int, int,
      float, float, float, float, float,
      bool, bool, bool, bool, bool,
    )

// ============================================================================
// Polyvariants with Many Constructors
// ============================================================================

@spice
type largePolyvariant = [
  | #A1 | #A2 | #A3 | #A4 | #A5
  | #B1 | #B2 | #B3 | #B4 | #B5
  | #C1 | #C2 | #C3 | #C4 | #C5
]

// ============================================================================
// Polyvariant with Large Payload (tests O(n²) in polyvariants.ml)
// ============================================================================

@spice
type polyvariantWithPayload = [
  | #None
  | #Small(string, int)
  | #Medium(string, string, int, int, float)
  | #Large(string, string, string, int, int, int, float, float, float, bool)
]

// ============================================================================
// Polyvariant with Very Large Payload (20 args)
// ============================================================================

@spice
type polyvariantHugePayload = [
  | #Empty
  | #Huge(
      string, string, string, string, string,
      int, int, int, int, int,
      float, float, float, float, float,
      bool, bool, bool, bool, bool,
    )
]

// ============================================================================
// Type aliases
// ============================================================================

@spice
type userId = int

@spice
type jsonBlob = JSON.t
