#!/bin/bash

(cd shootout/go && make)
(cd shootout/java && make)
(cd shootout/mpl-cc && make)

NOW=$(date '+%y%m%d-%H%M%S')

# all of the extra arguments get sent to scripts/runcmds.py with $@ below
{ \
  shootout/java/gencmds 7; \
  shootout/go/gencmds 7; \
  shootout/mpl-cc/gencmds 7; \
} \
| scripts/runcmds.py --repeat 1 --output results/sort-$NOW $@
