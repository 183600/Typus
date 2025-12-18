#!/bin/bash
# Script to run Typus commands with correct locale settings
unset LC_ALL
unset LANG
unset LC_CTYPE
unset LC_MESSAGES
unset LC_COLLATE
unset LC_MONETARY
unset LC_NUMERIC
unset LC_TIME
export LC_ALL=C
export LANG=C
export LC_CTYPE=C
export LC_MESSAGES=C
export LC_COLLATE=C
export LC_MONETARY=C
export LC_NUMERIC=C
export LC_TIME=C
exec "$@"