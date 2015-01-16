#!/bin/sh

name="dotty-types"
src="/home/sam/Documents/git/dotty/src/dotty/tools/"
rootclass="Type"

./scaladiagrams --source $src --root $rootclass > $name.dot && dot $name.dot -Tpdf > $name.pdf

