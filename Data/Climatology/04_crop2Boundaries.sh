#!/bin/bash

# FUNCTION: Crop to specific boundaries

while getopts m:v:e:tn:tx:nn:nx: flag
do
	case "${flag}" in
		m) model=${OPTARG};;
	 	v) var=${OPTARG};;
		e) expt=${OPTARG};;
		tn) latmin=${OPTARG};;
		tx) latmax=${OPTARG};;
		nn) lonmin=${OPTARG};;
		nx) lonmax=${OPTARG};;
	esac
done

# Print boundaries
echo "Boundaries: ";
echo "Latitudes: ${latmin} to ${latmax}";
echo "Longitudes: ${lonmin} to ${lonmax}";

# Define directories

mkdir crop

cdo -sellonlatbox,$lonmin,$lonmax,$latmin,$latmax "remapped/${var}_${model}_${expt}_remapped.nc" "crop/${var}_${model}_${expt}_cropped.nc"