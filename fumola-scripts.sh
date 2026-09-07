#!/bin/sh
#
# Every script in scripts/ is a Fumola example: runnable with no arguments,
# and expected to exit 0. Build and publish tooling lives in tools/ instead --
# assemble-site.sh landed here once and failed CI asking for its arguments.

for script in scripts/*.sh; do
    echo =====================
    echo Running Fumola script
    echo $script
    echo ====================

    $script || exit -1
done
