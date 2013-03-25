#!/bin/bash

function usage() {
    cat <<EOF
Usage: $0 file.pier
EOF
}

if [[ $# -ne 1 ]]; then
    usage
    exit 1
elif [[ ! -f $1 ]]; then
    echo "$1 not a regular file"
    usage
    exit 1
fi

input="$1"

echo "GutembergConsole generateStandaloneLaTeXFromPier: '${input}'. WorldState addDeferredUIMessage: [ SmalltalkImage current snapshot: true andQuit: true ]." | ./vm.sh Pharo.image eval
echo "GutembergConsole generateStandaloneHTMLFromPier: '${input}'. WorldState addDeferredUIMessage: [ SmalltalkImage current snapshot: true andQuit: true ]." | ./vm.sh Pharo.image eval

pdflatex ${input}.tex
pdflatex ${input}.tex
