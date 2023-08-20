#!/bin/bash

# FUNCTION: Create 1 file for each model/experiment in the yearly frequency

mkdir crop
mkdir merged

# Take inputs from user

while getopts m:g:v:n:e:a:x:o:i: flag
do
    case "${flag}" in
        m) model=${OPTARG};;
        g) gridname=${OPTARG};;
        v) var=${OPTARG};; # tos, o2os, phos, ...
        n) grid=${OPTARG};; # gn or gr
        e) expt=${OPTARG};; # ssp126, ssp245, ssp585, historical, ...
        a) latmin=${OPTARG};; # minimum latitude for defining cropped area
        x) latmax=${OPTARG};; # maximum latitude for defining cropped area
        o) lonmin=${OPTARG};; # minimum longitude for defining cropped area
        i) lonmax=${OPTARG};; # maximum longitude for defining cropped area
    esac
done

# Print inputs
echo "User inputs:";
echo "Model: $model";
echo "Grid to be selected: $gridname";
echo "Variable name: $var";
echo "Grid type: $grid";
echo "Experiment: $expt";
echo "Boundaries defined are: xmin: $lonmin, xmax: $lonmax, ymin: $latmin, and ymax: $latmax";

# Define directories
indir="raw/"
outdir="crop/"
mergedir="merged/"
finaldir="final/"

# Find the files per model, scenario, and variable
curr_files=($(ls ${indir}*$var*$model*$expt*))
num_files=${#curr_files[@]}

for((i=0; i<=num_files-1; i++)); do
    curr_file=${curr_files[i]}

    # 1. Select grid
    tmpName1="tmpfile1.nc"
    cdo -selgrid,$gridname $curr_file $tmpName1

    # 2. Remap to a global grid
    tmpName2="tmpfile2.nc"
    cdo -remapbil,global_1 $tmpName1 $tmpName2

    # 3. Crop the data if necessary
    if [[ "$lonmin" == "NA" ]]
        then
        echo "No boundaries indicated."
        tmpName3="tmpfile2.nc"
    else
        tmpName3="tmpfile3.nc"
        cdo -sellonlatbox,$lonmin,$lonmax,$latmin,$latmax $tmpName2 $tmpName3 # crop the data
    fi

    # 4. Select the years we want
    tmpName4="tmpfile4.nc"
    if [[ $expt == "historical" ]]
        then
        cdo -L selyear,1850/2014 -selname,$var $tmpName3 $tmpName4
    else
        cdo -O -L selyear,2015/2100 -selname,$var $tmpName3 $tmpName4
    fi

    # 5. Change frequency from monthly to yearly
    if [[ "$grid" == "gn" ]]
        then
        annual_name=${curr_file/_gn_/_cropped_} # Change name
    else # if grid = "gr"
        annual_name=${curr_file/_gr_/_cropped_}
    fi

    annual_name=${annual_name/Omon/Oyr}
    annual_name=${annual_name/$indir/$outdir} # Change directory
    cdo -yearmean $tmpName4 $annual_name # Change frequency from monthly to yearly

    # clean up the temporary files
    rm $tmpName1 $tmpName2 $tmpName3 $tmpName4
done

# 6. Merge files into one file for each model
merged_name=${annual_name::${#annual_name}-16} # Remove dates
merged_name=${merged_name/_cropped_/_cropped_merged.nc} # Change name
merged_name=${merged_name/$outdir/$mergedir} # Change directory
cdo -O -mergetime $outdir*$var*$model*$expt* $merged_name
echo $outdir*$var*$model*$expt*