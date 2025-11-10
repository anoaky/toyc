#!/bin/zsh

while getopts "o" opt; do
    case $opt in 
        o)
            showreport=1
            ;;
    esac
done
cargo run --example tests
python tests/gen_report.py > tests/reports/report.html
if [[ ! -z "$showreport" ]]; then
    open tests/reports/report.html
fi