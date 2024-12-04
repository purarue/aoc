The advent of code graveyard

Perhaps one of these years I will finally accomplish my goal of solving it in distinct languages...

```
>>>PMARK
perl -E 'print "`"x3, "\n"'
tree -d $(fd '20??' --max-depth 1 --type d --format '{/}' | sort -n) | sed -e '$d' -e '/^\s*$/d'
perl -E 'print "`"x3, "\n"'
```
