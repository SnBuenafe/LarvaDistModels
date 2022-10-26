#!/bin/bash

# FUNCTION: Get the water-column-integrated values by taking the median across all levels (i.e., depths)

mkdir tmpsplit
mkdir integrated

# Take inputs from user

while getopts m:v:e: flag
do
    case "${flag}" in
        m) model=${OPTARG};;
		v) var=${OPTARG};; # tos, o2os, phos, ...
		e) expt=${OPTARG};; # ssp126, ssp245, ssp585, historical, ...
    esac
done

# Define directories
indir="merged/"
finaldir="integrated/"
splitdir="tmpsplit/"

# Find file according to variable, model, and experiment
curr_file=($(ls ${indir}*$var*$model*$expt*))

# 1. Set missing values to a really high value so as not to affect the median: e.g., -9999
tmpName1="timpfile1.nc"
cdo setmissval,-9999 $curr_file $tmpName1

# 2. Split the merged file into their unique levels
prefix="tmpsplit/split_"
cdo -splitlevel $tmpName1 $prefix

# Clean up
rm $tmpName1

# 3. Get the median of different levels across grids
vertical_name=${curr_files/_cropped_merged/_vertical} # Change name
vertical_name=${vertical_name/$indir/$finaldir} # Change directory

cdo enspctl,50 tmpsplit/split_* $vertical_name

# clean split files
rm tmpsplit/*