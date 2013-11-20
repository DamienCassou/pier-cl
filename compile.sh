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
vm="${PHARO_VM:-./pharo}"

"$vm" Pharo.image eval <<SMALLTALK
PRExporter
    generateSBALaTeXChapterFromPier: '${input}';
    generateStandaloneHTMLFromPier: '${input}';
    generateGitHubMarkdownFromPier: '${input}'.

WorldState addDeferredUIMessage: [ SmalltalkImage current snapshot: false andQuit: true ].
SMALLTALK

latexmk "${input}"
