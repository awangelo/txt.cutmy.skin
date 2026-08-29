import Text

/-!
Unit tests, run with `lake test`.
-/

namespace Test.RateLimit

open _root_.RateLimit

private def assertEq [BEq α] [ToString α] (name : String) (got expected : α) : IO Unit := do
  unless got == expected do
    throw <| IO.userError s!"{name}: expected {expected}, got {got}"

private def stateOf : Decision → State
  | .allowed s => s
  | .denied _ s => s

/-- Map equality without a `ToString State` instance: compare with `==`, report via `toList`. -/
private def assertStateEq (name : String) (got expected : State) : IO Unit := do
  unless got == expected do
    throw <| IO.userError s!"{name}: expected {expected.toList}, got {got.toList}"

private def expectAllowed (name : String) (d : Decision) (k : State → IO Unit) : IO Unit :=
  match d with
  | .allowed s => k s
  | .denied _ _ => throw <| IO.userError s!"{name}: expected allowed, got denied"

private def expectDenied (name : String) (d : Decision) (k : Nat → State → IO Unit) : IO Unit :=
  match d with
  | .denied r s => k r s
  | .allowed _ => throw <| IO.userError s!"{name}: expected denied, got allowed"

/-! ceilSeconds -/

private def ceilSecondsTests : IO Unit := do
  assertEq "ceilSeconds/0" (ceilSeconds 0) 0
  assertEq "ceilSeconds/1" (ceilSeconds 1) 1
  assertEq "ceilSeconds/999" (ceilSeconds 999) 1
  assertEq "ceilSeconds/1000" (ceilSeconds 1000) 1
  assertEq "ceilSeconds/1001" (ceilSeconds 1001) 2
  assertEq "ceilSeconds/600000" (ceilSeconds 600000) 600

/-! keepRecent -/

private def keepRecentTests : IO Unit := do
  -- strict boundary: t = now - window is expired, +1 is live
  assertEq "keepRecent/boundary-expired" (keepRecent [0] 600000) []
  assertEq "keepRecent/boundary-live" (keepRecent [1] 600000) [1]
  -- order preserved
  assertEq "keepRecent/order" (keepRecent [700000, 600000, 100] 700000) [700000, 600000]
  assertEq "keepRecent/empty" (keepRecent [] 0) []
  assertEq "keepRecent/all-expired" (keepRecent [3, 2, 1] 700000) []
  -- window override
  assertEq "keepRecent/custom-window" (keepRecent [50, 10] 100 (window := 60)) [50]

/-! removeExpired -/

private def removeExpiredTests : IO Unit := do
  let mixed : State := (∅ : State)
    |>.insert "stale" [100, 50]
    |>.insert "half" [900, 10]
    |>.insert "dead" [500]
  let out := removeExpired mixed 600500
  assertEq "removeExpired/drops-dead-ips" out.size 1
  assertEq "removeExpired/keeps-live-only" (out.getD "half" []) [900]
  -- every kept timestamp is live (GC never drops or keeps wrong entries)
  let keptLive := out.toList.all fun (_, ts) =>
    ts.all fun t => t + windowMs > 600500
  assertEq "removeExpired/kept-are-live" keptLive true
  -- all-stale map becomes empty
  let stale : State := (∅ : State) |>.insert "a" [1] |>.insert "b" [2]
  assertEq "removeExpired/all-stale" (removeExpired stale 700000).size 0

/-! check -/

private def checkTests : IO Unit := do
  -- fresh IP allowed, attempt recorded
  expectAllowed "check/fresh" (check (∅ : State) "ip" 1000) fun s =>
    assertEq "check/fresh-recorded" (s.getD "ip" []) [1000]

  -- three allowed attempts, newest first
  let s3 := (List.range 3).foldl (init := ∅) fun s i =>
    stateOf (check s "ip" (i + 1))
  assertEq "check/recorded-newest-first" (s3.getD "ip" []) [3, 2, 1]

  -- 4th attempt inside the window denied, correct retry, state untouched
  expectDenied "check/4th" (check s3 "ip" 4) fun r s => do
    assertEq "check/4th-retry" r 600
    assertStateEq "check/4th-readonly" s s3

  -- retry counts down to the oldest live entry, not the newest
  let sSpread : State := (∅ : State) |>.insert "ip" [700000, 699000, 698000]
  expectDenied "check/retry-oldest" (check sSpread "ip" 700001) fun r _ =>
    assertEq "check/retry-oldest-value" r 598

  -- oldest entry expires exactly at the boundary, capacity freed
  expectAllowed "check/boundary" (check s3 "ip" 600001) fun s =>
    assertEq "check/boundary-freed" (s.getD "ip" []) [600001, 3, 2]

  -- stale entries of other IPs never affect a decision, and a plain
  -- check (no GC) does not purge them
  let sOther : State := (∅ : State) |>.insert "other" [1]
  expectAllowed "check/stale-other" (check sOther "fresh" 700000) fun s =>
    assertStateEq "check/stale-other-untouched" s (sOther.insert "fresh" [700000])

  -- limit override
  let s1 := stateOf (check (∅ : State) "ip" 1000)
  expectDenied "check/limit-1" (check s1 "ip" 1001 (limit := 1)) fun r _ =>
    assertEq "check/limit-1-retry" r 600

  -- GC at the cap: stale IPs dropped, live ones kept
  let mixed : State := (∅ : State)
    |>.insert "stale1" [1] |>.insert "stale2" [2] |>.insert "live" [700000, 600000]
  expectAllowed "check/gc-at-cap" (check mixed "live" 700001 (maxIps := 2)) fun s => do
    assertEq "check/gc-drops-stale" s.size 1
    assertEq "check/gc-keeps-live" (s.getD "live" []) [700001, 700000, 600000]

  -- soft cap: a fully live map stays above maxIps, nothing evicted
  let allLive := (List.range 4).foldl (init := (∅ : State)) fun s i =>
    s.insert s!"ip{i}" [900, 800, 700]
  expectDenied "check/soft-cap" (check allLive "ip0" 1000 (maxIps := 2)) fun _ s =>
    assertEq "check/soft-cap-keeps-all" s.size 4

  -- stored list never exceeds the limit
  let sMany := (List.range 5).foldl (init := ∅) fun s i =>
    stateOf (check s "ip" (1000 * i))
  assertEq "check/list-capped" (sMany.getD "ip" []).length uploadLimit

def all : IO Unit := do
  ceilSecondsTests
  keepRecentTests
  removeExpiredTests
  checkTests

end Test.RateLimit
