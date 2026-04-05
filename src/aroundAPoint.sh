#!/usr/bin/bash
strings=(
  constantsModule
  vectorModule
  startParametersModule
  gravityModule
  aroundAPoint
)

FILES=""
for i in "${strings[@]}"; do
	FILES+="$i".f95" "
done

#gfortran -ffree-form -c $FILES
gfortran -std=f2003 $FILES

FILES="${FILES//f95/o}"

gfortran $FILES
