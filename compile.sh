#!/bin/bash

if [ -d "$HOME/.cabal/bin" ] ; then
    PATH="$HOME/.cabal/bin:$PATH"
fi

pandoc -s 0*.md --template=templates/template.latex --from markdown+auto_identifiers -V colorlinks --number-sections --pdf-engine=xelatex --variable mainfont="Brill" --dpi=300 --filter render.hs -o tidal-by-example.pdf
