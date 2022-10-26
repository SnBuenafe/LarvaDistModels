#!/bin/bash

# FUNCTION: Create ensemble mean

while getopts i:v:e:o:n:x: flag
do
	case "${flag}" in
		i) indir=${OPTARG};; # input directory
	 	v) var=${OPTARG};;
		e) expt=${OPTARG};;
		o) ensopt=${OPTARG};; #ensemble mean or median
		n) yearmin=${OPTARG};;
		x) yearmax=${OPTARG};;
	esac
done

# Define directories
mkdir ensemble

files=($(ls ${indir}/*$var*$model*$expt*$yearmin*$yearmax*))

if [[ "$ensopt" == "mean" ]]
	then
	cdo -ensmean $files "ensemble/${var}_${expt}_${yearmin}_${yearmax}_ensemble.nc"
else
	cdo -ensmedian $files "ensemble/${var}_${expt}_${yearmin}_${yearmax}_ensemble.nc"
fi