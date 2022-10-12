#!/bin/bash

# FUNCTION: Change frequency from monthly to annual

while getopts i:m:v:e: flag
do
	case "${flag}" in
		i) indir=${OPTARG};; # input directory
		m) model=${OPTARG};;
	 	v) var=${OPTARG};;
		e) expt=${OPTARG};;
	esac
done

# Define directories
mkdir newfreq

file=($(ls ${indir}/*$var*$model*$expt*))

cdo -yearmean $file "newfreq/${var}_${model}_${expt}_annual.nc"