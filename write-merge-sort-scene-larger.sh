{
    time \
    stdbuf -oL -eL \
    cargo run  -- \
    eval 'import M "fumola/examples/mergeSort/mergeSort"; M.generateSceneFullDemand(10, 44)' \
    --import $(find fumola -name "*.fumola")
} 2>&1 | tee write-merge-sort-scene.output
