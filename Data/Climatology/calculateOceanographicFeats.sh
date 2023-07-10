#!/bin/bash

declare -a season_list=("jan-mar" "apr-jun" "jul-sept" "oct-dec")

########################## ZONAL VELOCITY (uo) ##########################

# declare -a a_list=("ACCESS-ESM1-5" "BCC-CSM2-MR" "CMCC-CM2-SR5" "CMCC-ESM2" "FGOALS-f3-L" "FGOALS-g3" "GFDL-CM4" "GISS-E2-1-G" "GISS-E2-1-H" "IPSL-CM5A2-INCA" "MCM-UA-1-0" "MIROC-ES2L" "MIROC6" "MPI-ESM1-2-HR" "MRI-ESM2-0" "NorESM2-LM")

# var="uo"
# expt="historical"
# yearmin=1956
# yearmax=1981

# for t in ${a_list[@]}; do
# 	# Calculate the vertical mean and invert latitudes for some models
# 	bash 08_selectLevels.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -v $var -e $expt

# 	# Merge the files into 1 file per model per scenario
# 	if [[ $t == "GISS-E2-1-G" || $t == "GISS-E2-1-H" || $t == "MCM-UA-1-0" ]]
# 		then
# 		bash 01_mergeFiles.sh -i "selectlevels/" -m $t -g lonlat -v $var -e $expt
# 	else
# 		bash 01_mergeFiles.sh -i "selectlevels/" -m $t -g curvilinear -v $var -e $expt
# 	fi

# 	rm selectlevels/* # Free up space

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

########################## MERIODONAL VELOCITY (vo) ##########################

declare -a a_list=("ACCESS-ESM1-5" "BCC-CSM2-MR" "CMCC-CM2-SR5" "CMCC-ESM2" "FGOALS-f3-L" "FGOALS-g3" "GFDL-CM4" "GISS-E2-1-G" "GISS-E2-1-H" "IPSL-CM5A2-INCA" "MCM-UA-1-0" "MIROC-ES2L" "MIROC6" "MPI-ESM1-2-HR" "MRI-ESM2-0" "NorESM2-LM")

var="vo"
expt="historical"
yearmin=1956
yearmax=1981

for t in ${a_list[@]}; do
	# Calculate the vertical mean and invert latitudes for some models
	bash 08_selectLevels.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -v $var -e $expt

	# Merge the files into 1 file per model per scenario
	if [[ $t == "GISS-E2-1-G" || $t == "GISS-E2-1-H" || $t == "MCM-UA-1-0" ]]
		then
		bash 01_mergeFiles.sh -i "selectlevels/" -m $t -g lonlat -v $var -e $expt
	else
		bash 01_mergeFiles.sh -i "selectlevels/" -m $t -g curvilinear -v $var -e $expt
	fi

	rm selectlevels/* # Free up space

	# Select years we want
	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

	# Remap to 1x1 degree grid
	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
	rm selectyears/* # Free up space

	# Change frequency
	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

	echo $t
done

# Make the seasonal ensembles
for s in ${season_list[@]}; do
	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
done