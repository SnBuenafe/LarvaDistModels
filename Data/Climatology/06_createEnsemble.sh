#!/bin/bash

# FUNCTION: Create ensemble mean

while getopts i:v:e:o: flag
do
	case "${flag}" in
		i) indir=${OPTARG};; # input directory
	 	v) var=${OPTARG};;
		e) expt=${OPTARG};;
		o) ensopt=${OPTARG};; #ensemble mean or median
	esac
done

# Define directories
mkdir ensemble

files=($(ls ${indir}/*$var*$model*$expt*))

if [[ "$ensopt" == "mean" ]]
	then
	cdo -ensmean $files "ensemble/${var}_ensemble.nc"
else
	cdo -ensmedian $files "ensemble/${var}_ensemble.nc"
fi