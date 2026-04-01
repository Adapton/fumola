{
    time \
    stdbuf -oL -eL \
    cargo run  -- \
    eval "import M \"fumola/examples/mergeSort/mergeSort\"; M.runAll()" \
    --import $(find fumola -name "*.fumola")
} 2>&1 | tee mergeSort-example.output
