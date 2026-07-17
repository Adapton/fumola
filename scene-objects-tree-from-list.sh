{
    time \
    stdbuf -oL -eL \
    cargo run  -- \
    eval 'import L "fumola/collections/levelTree"; L.Scene.testSceneObjectsFromList()' \
    --import $(find fumola -name "*.fumola")
} 2>&1 | tee $0.output
