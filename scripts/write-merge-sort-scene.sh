{
    time \
    stdbuf -oL -eL \
    cargo run  -- \
    eval 'import M "fumola/examples/mergeSort/mergeSort"; let output = M.generateSceneFullDemand(10, 4, null); output.sceneData.objects.size()' \
    --import $(find fumola -name "*.fumola")
} 2>&1 | tee write-merge-sort-scene.output
