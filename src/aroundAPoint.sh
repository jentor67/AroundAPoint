#!/usr/bin/bash
strings=(
  vectorModule
  startParameters
  gravityModule
  aroundAPoint
)

FILES=""
for i in "${strings[@]}"; do
	FILES+="$i".f95" "
done

gfortran -ffree-form -c $FILES

FILES="${FILES//f95/o}"

gfortran $FILES
