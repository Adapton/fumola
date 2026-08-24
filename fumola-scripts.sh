#!/bin/sh

for script in scripts/*.sh; do
    echo =====================
    echo Running Fumola script
    echo $script
    echo ====================

    $script || exit -1
done
