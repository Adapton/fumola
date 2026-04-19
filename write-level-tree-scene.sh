{
    time \
    stdbuf -oL -eL \
    cargo run  -- \
    eval 'import M "fumola/collections/levelTree"; M.Viz.writeSceneFileHelper(`levelTreeScene, 10, 137)' \
    --import $(find fumola -name "*.fumola")
} 2>&1 | tee write-level-tree-scene.output
