#!/bin/bash

if [ -d "$HOME/.cabal/bin" ] ; then
    PATH="$HOME/.cabal/bin:$PATH"
fi

# --template=templates/template.latex 
pandoc -s 0*.md --from markdown+auto_identifiers -V colorlinks --number-sections --pdf-engine=xelatex --variable mainfont="Linux Libertine O" --dpi=300 --filter ./bin/render.hs -o tidal-by-example.pdf
