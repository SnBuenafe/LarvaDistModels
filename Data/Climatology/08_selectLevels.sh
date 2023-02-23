#!/bin/bash

# FUNCTION: Extract required levels and calculate the weighted vertical mean

while getopts i:m:v:e:f:l: flag
do
	case "${flag}" in
		i) indir=${OPTARG};;
		m) model=${OPTARG};;
	 	v) var=${OPTARG};;
		e) expt=${OPTARG};;
	esac
done

# Define directories

mkdir selectlevels

selleveldir="selectlevels/"

curr_files=($(ls ${indir}*$var*$model*$expt*))
num_files=${#curr_files[@]}

# Loop through all these files
for((i=0; i<=num_files-1; i++)); do
	curr_file=${curr_files[i]}

	if [[ $model == "FGOALS-f3-L" || $model == "FGOALS-g3" || $model == "MPI-ESM1-2-HR" ]] # invert latitude for some models
		then
		cdo select,levrange=0,35 $curr_file tmp1.nc
		cdo -invertlat tmp1.nc tmpfile.nc
		rm tmp1.nc
		echo "Inverted latitudes."
	elif [[ $model == "NorESM2-LM" ]]
		then
		cdo -sellevel,1027.22 $curr_file tmpfile.nc
	else
		cdo select,levrange=0,35 $curr_file tmpfile.nc
	fi

	new_name=${curr_file/$indir/$selleveldir} # Change directory

	cdo -vertmean tmpfile.nc ${new_name}
	echo "Calculated weighted vertical mean"

	rm tmpfile.nc

done