## Test environments
* local OS X install, R 3.3.2
* ubuntu 12.04 (on travis-ci), R 3.3.2
* win-builder (release and devel)

## R CMD check results
There were no ERRORs or WARNINGs.

win-builder check has 1 note:

```
New submission
```


Fixed 2 notes in previous submission.

```
Possibly mis-spelled words in DESCRIPTION:
   dplyr (3:45, 7:18, 8:35, 8:63, 10:61)
   extdplyr (7:62, 8:44)
   tidyr (3:55, 9:6)
```

Fixed description file for title case and single quotes. 

```
Non-standard file/directory found at top level:
  'examples'
```

This is fixed by adding the folder to `.Rbuildignore`.
