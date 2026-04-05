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

echo $FILES
#gfortran -ffree-form -c $FILES
gfortran -std=f2008 $FILES

FILES="${FILES//f95/o}"

echo $FILES

gfortran $FILES
