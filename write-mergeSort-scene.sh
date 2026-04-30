{
    time \
    stdbuf -oL -eL \
    cargo run  -- \
    eval 'import M "fumola/examples/mergeSort/mergeSort"; M.testGenerateSceneFullDemand()' \
    --import $(find fumola -name "*.fumola")
} 2>&1 | tee write-mergeSort-scene.output
