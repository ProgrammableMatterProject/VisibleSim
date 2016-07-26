#!/bin/bash

usage() {
    echo "Usage: $0 <path-to-blockCode-binary>"
    echo "Example: $0 ../applicationsBin/bbCycle"
    exit 1
}

print_result() {
    if [ $2 == true ]; then
        echo -e "$1: \033[33;32mPASS\x1B[0m"
    else
        echo -e "$1: \033[33;31mFAILED\x1B[0m"
    fi

    exit 0
}

# Check parameters
[ $# -eq 0 ] && usage

# Check BlockCode
if [ ! -x "$1" ]; then
    echo "error: invalid BlockCode $1"
    exit 1
fi

# Save work directory
bcDir="$(dirname $1)"
bcID="$(basename $bcDir)"
control="$bcDir/.controlConf.xml"

# Check that a control output has been generated
if [ ! -r "$control" ]; then
    echo "error: $control does not exist. Please export a control xml file with VisibleSim's -g option first and try again."
    while true; do
        read -p "Do you wish to export it now? (y/n)" yn    
        case $yn in
            [Yy]* ) (./$1 -t -g -c $bcDir/config.xml &> /dev/null; mv .confCheck.xml $control); break;;
            [Nn]* ) exit 1;;
            * ) echo "Please answer yes or no.";;
        esac
    done
fi

# Execute BlockCode in test mode
(./$1 -t -g -c $bcDir/config.xml &> /dev/null)

# Check simulation status (FAIL if != EXIT_SUCCESS (0))
status=$?

if [ $status != 0 ]; then
    print_result "$bcID" false
fi

# Compare output and expected output
DIFF=$(echo `diff ".confCheck.xml" "$control"`)

# Clean outputs
rm -f ".confCheck.xml"

# Interpret diff result
if [ "$DIFF" == "" ]; then
    print_result "$bcID" true
else
    print_result "$bcID" false
fi
