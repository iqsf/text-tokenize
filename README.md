# TextTokenize

## Introduction
Library for parsing text.

## Short examples
First example:
```
tokenize defaultTokenizeProps "qqq wwwwww  eeeeeeee rrrrrr"
```

This code generates the following result:

```
[TokenAtom "qqq",TokenAtom "wwwwww",TokenAtom "eeeeeeee",TokenAtom "rrrrrr"]
```

Second example:
```
putStrLn $ show $ tokenize (TokenizeBlock [ ("{","}")
                                          , ("/*","*/")
                                          ] Nothing True
                           ) 
                           "void func1 (var p, var t) { /* asasa */}"
```

This code generates the following result:

```
[ TokenBlock TBBody "void func1 (var p, var t) "
, TokenBlock TBDelm "{"
, TokenBlock TBBody " "
, TokenBlock TBDelm "/*"
, TokenBlock TBBody " asasa "
, TokenBlock TBDelm "*/"
, TokenBlock TBDelm "}"
]
```



