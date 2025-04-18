###  Print source code after transformation
```
node_modules/rescript/cli/bsc -dparsetree -ppx "../ppx -uncurried" -reprint-source src/Records.res
```