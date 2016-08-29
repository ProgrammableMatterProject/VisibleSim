#!/bin/bash

usage() {
    echo "Usage: $0 <test-ID> <path-to-blockCode-binary> <VisibleSim-arguments>"
    echo "Example: $0 bbCycle ../applicationsBin/bbCycle -c config123.xml -s 9000000"
    echo "Test-ID can be used to distinguish between 2 control XML files from the same directory"
    exit 1
}

print_result() {
    size=${#1}

    # Pretty printing
    echo -ne "$1:"
    if [ $size -lt 7 ]; then
        echo -ne "\t"
    fi
    
    if [ $2 == true ]; then
        echo -e "\t\t\t[\033[33;32mPASS\x1B[0m]"
    else
        echo -e "\t\t\t[\033[33;31mFAILED\x1B[0m]"
    fi
   
    exit 0
}

# Check parameters and parse potential arguments
[ $# -lt 2 ] && usage

# Check BlockCode
if [ ! -x "$2" ]; then
    echo "error: invalid BlockCode $2"
    exit 1
fi

# Save Test ID
testID="$1"
shift                           # Shift past Test-ID

# Save work directory
bcDir="$(dirname $1)"
bcID="$(basename $bcDir)"
check="$bcDir/.confCheck.xml"
control="$bcDir/.controlConf_$testID.xml"

# Check that a control output has been generated
if [ ! -r "$control" ]; then
    echo "warning: $control does not exist. Please export a control xml file with VisibleSim's -g option first and try again."
    while true; do
        read -n 1 -rep $'Do you wish to export it now? (y/n) ' yn
        case $yn in
            [Yy]* ) (cd "$bcDir" && ./"$@" -t -g &>/dev/null; mv "$check" "$control" 2>/dev/null); break;;
            [Nn]* ) print_result "$testID" false;;
            * ) echo "Please answer yes or no.";;
        esac
    done
fi

# Execute BlockCode in test mode
(cd "$bcDir" && ./"$@" -t -g &>/dev/null)

# Check simulation status (FAIL if != EXIT_SUCCESS (0))
status=$?

if [ $status != 0 ]; then
    print_result "$testID" false
fi

# Compare output and expected output
DIFF=$(echo `diff "$check" "$control" 2> /dev/null`)

# Clean outputs
rm -f $check

# Interpret diff result (test succeeds if both output and control are identical)
if [ "$DIFF" == "" ]; then
    print_result "$testID" true
else
    print_result "$testID" false
fi
