{
    time \
    stdbuf -oL -eL \
    cargo run  -- \
    eval 'import M "fumola/collections/levelTree"; M.Scene.writeRealignScene(10, 16, 3, #inductive, `inductive)' \
    --import $(find fumola -name "*.fumola")
} 2>&1 | tee write-realign-scene.output
