#!/bin/bash

# Take inputs from user

while getopts m: flag
do
    case "${flag}" in
        m) model=${OPTARG};;
    esac
done

echo "User inputs:";
echo "Model: $model";

# Define directories

indir="merged/"
finaldir="final2/"
splitdir="tmpsplit/"

# defining arrays
declare -a ExpArray=("ssp126" "ssp245" "ssp585")

for e in ${ExpArray[@]}; do
    echo $e

    curr_files=($(ls ${indir}*thetao*$model*$e*))
    num_files=${#curr_files[@]}

    for((i=0; i<=num_files-1; i++)); do

        curr_file=${curr_files[i]}

        out_name1="tmpfile1.nc"
        cdo setmisstonn $curr_file $out_name1 # set missing values to nearest neighborhood

        # Split the merged file into their unique levels
        prefix="tmpsplit/split_"
        cdo -splitlevel $out_name1 $prefix

        # Clean up
        rm $out_name1
    done
    #split_files=($(ls ${splitdir}))
    
    vertical_name=${curr_files/_cropped_merged/_vertical} # Change name
    vertical_name=${vertical_name/$indir/$finaldir} # Change directory

    cdo enspctl,50 tmpsplit/split_* $vertical_name

    # clean split files
    rm tmpsplit/*
done