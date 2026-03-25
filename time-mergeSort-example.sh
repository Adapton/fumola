{
    time \
    stdbuf -oL -eL \
    cargo run --release -- \
    eval "import M \"fumola/examples/mergeSort/mergeSort\"; M.run()" \
    --import $(find fumola -name "*.fumola")
} 2>&1 | tee time-mergeSort-example.output
