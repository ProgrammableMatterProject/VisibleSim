#!/bin/sh


usage () {
    cat <<EOF
Usage: runBlockCode.sh CODEBLOCKNAME [-c CONFIG]
Example: runBlockCode.sh bbcycle -c config.xml
If no config is provided, a configuration file with name config.xml with be expected by default.
EOF
}

if test $# -ge 1; then
    if ! [ -d "./applicationsBin/$1" ]; then
        echo "error: CodeBlock directory $1 does not exist." >&2
        exit 1
    fi

    if ! [ -f "./applicationsBin/$1/$1" ]; then
        echo "error: Code Block file $1 does not exist, please make sure the enclosing directory" \
             "and code block have identical names." >&2
        exit 1
    fi
    
    (cd ./applicationsBin/$1; ./"$@")
else
    usage
fi

exit 0

