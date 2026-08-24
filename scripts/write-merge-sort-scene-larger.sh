{
    time \
    stdbuf -oL -eL \
    cargo run --release  -- \
    eval 'import M "fumola/examples/mergeSort/mergeSort"; let out = M.generateSceneFullDemand(10, 44, null); out.sceneData.objects.size()' \
    --import $(find fumola -name "*.fumola")
} 2>&1 | tee write-merge-sort-scene-larger.output
