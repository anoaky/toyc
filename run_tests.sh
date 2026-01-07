#!/bin/zsh
usage () {
    echo "Usage: run_tests.sh [options]\nOptions:"
    echo "\t-o: Run tests and open the generated report in your default browser"
    echo "\t-h: Display this information and exit"
}
while getopts "ho" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
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