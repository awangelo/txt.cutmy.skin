import Std.Data.HashMap

/-!
Sliding-window rate limiter: 3 upload attempts per 10 minutes per IP.
-/

namespace RateLimit

/--
IP → upload timestamps, newest first.
-/
abbrev State := Std.HashMap String (List Nat)

def windowMs : Nat := 10 * 60 * 1000 -- 10 minutes

def uploadLimit : Nat := 3

inductive Decision where
  | allowed (state : State)
  | denied (retryAfterSeconds : Nat) (state : State)

def keepRecent (timestamps : List Nat) (nowMs : Nat) (window : Nat := windowMs)
    : List Nat :=
  timestamps.filter fun t => t + window > nowMs

/--
Drops expired timestamps and IPs with no timestamps left.
-/
def removeExpired (state : State) (nowMs : Nat) (window : Nat := windowMs)
    : State :=
  state.filterMap fun _ timestamps =>
    let recent := keepRecent timestamps nowMs window
    if recent.isEmpty then none else some recent

def ceilSeconds (ms : Nat) : Nat := (ms + 999) / 1000

/--
Checks one attempt from `ip`: when allowed, the attempt is recorded,
when denied, `retryAfterSeconds` says how long to wait and the state is
returned unchanged. When the map exceeds `maxIps` IPs, expired ones
are dropped first.
-/
def check (state : State) (ip : String) (nowMs : Nat) (window : Nat := windowMs)
    (limit : Nat := uploadLimit) (maxIps : Nat := 1024) : Decision :=
  let state := if state.size > maxIps then removeExpired state nowMs window else state
  let recent := keepRecent (state.getD ip []) nowMs window
  if recent.length < limit then
    .allowed (state.insert ip (nowMs :: recent))
  else
    .denied (ceilSeconds (recent.getLastD nowMs + window - nowMs)) state

end RateLimit
