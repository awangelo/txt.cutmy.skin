import Test

def main : IO UInt32 := do
  try
    Test.RateLimit.all
    IO.println "all tests passed"
    return 0
  catch e =>
    IO.println s!"FAILED: {e}"
    return 1
