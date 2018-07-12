# YDC Calculator 

These files involve overloading basic integer operators to perform arbitrary
precision integer arithmetic in the style of dc(1). The class bigint will intermix
arbitrarily with simple integer arithmetic.
Will follow the same style and functions as stated in the following command
```
man -s 1 dc
```
Performs the following dc commands: 
```
+-*/%^cdfpq
```
## To Run 

```
make
ydc
```

## Files
Major implementations in these files: 
ubigint.cpp
ubigint.h
bigint.cpp
bigint.h
main.cpp
Makefile
