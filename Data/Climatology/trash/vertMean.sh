#!/bin/bash

# Run the code for all scenarios for all models
declare -a ExpArray=("historical")
# Set a temporary name file
tmpName="temporaryFile.nc"

#########################################
# chl
model="GFDL-CM4"
#bash cropClimate.sh -m $model -g lonlat -v chl -n gr -e historical -a NA -x NA -o NA -i NA

indir="merged/"
finaldir="integrated/"
curr_file=($(ls ${indir}*chl*$model*historical*))

cdo -showlevel $curr_file # find the levels

# So... first we select levels that are <= 100m
cdo -sellevel,2.5,10,20,32.5,51.25,75,100 $curr_file $tmpName

vertical_name=${curr_file/_cropped_merged/_vertical} # Change name
vertical_name=${vertical_name/$indir/$finaldir} # Change directory

# Get the vertical mean for all these levels
cdo -vertmean $tmpName $vertical_name