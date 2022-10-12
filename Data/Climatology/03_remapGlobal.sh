#!/bin/bash

# FUNCTION: Remap to global 1x1 degree grid

while getopts i:m:v:e: flag
do
	case "${flag}" in
		i) inp=${OPTARG};; # inp file
		m) model=${OPTARG};;
	 	v) var=${OPTARG};;
		e) expt=${OPTARG};;
	esac
done

# Define directories

mkdir remapped

cdo -remapbil,global_1 $inp "remapped/${var}_${model}_${expt}_remapped.nc"