# kill-bool

Makes sense of your booleans: strong type them!

## Example

```haskell
type TT = TBool "missing" "present"

isPresent :: TT
isPresent = mkIs $ Proxy @"present"

is (Proxy @"missing") isPresent == False
```

