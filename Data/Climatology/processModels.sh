#!/bin/bash

########################## TEMPERATURE ##########################

# declare -a tmp_list=("BCC-CSM2-MR" "CMCC-CM2-SR5" "CMCC-ESM2" "FGOALS-f3-L" "FGOALS-g3" "MIROC6" "MIROC-ES2L" "MRI-ESM2-0" "NorESM2-LM")
# var="tos"
# expt="historical"
# yearmin=1956
# yearmax=1984

# for t in ${tmp_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# Make the ensemble
# bash 06_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax

# expt="ssp585"

# Present (2017-2026)
# yearmin=2017
# yearmax=2026
# for t in ${tmp_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# bash 06_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax

# Mid-century (2046-2055)
# yearmin=2046
# yearmax=2055

# for t in ${tmp_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# Removed this step because it has already been done above.

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# bash 06_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax

# End of the century (2091-2100)
# yearmin=2091
# yearmax=2100

# for t in ${tmp_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# Removed this step because it has already been done above.

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# bash 06_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax

########################## pH ##########################

# declare -a ph_list=("CMCC-ESM2" "MIROC-ES2L" "NorESM2-LM")
# var="phos"
# expt="historical"
# yearmin=1956
# yearmax=1984

# for t in ${ph_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the ensemble
# bash 06_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax

# expt="ssp585"

# Present (2017-2026)
# yearmin=2017
# yearmax=2026
# for t in ${ph_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# bash 06_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax

# Mid-century (2046-2055)
# yearmin=2046
# yearmax=2055
# for t in ${ph_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# Removed this step because it has already been done above.

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# bash 06_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax

# End of the century (2091-2100)
# yearmin=2091
# yearmax=2100
# for t in ${ph_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# Removed this step because it has already been done above.

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# bash 06_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax

########################## OXYGEN ##########################

# declare -a o2_list=("CMCC-ESM2" "MIROC-ES2L" "MRI-ESM2-0" "NorESM2-LM")
# var="o2os"
# expt="historical"
# yearmin=1956
# yearmax=1984

# for t in ${o2_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the ensemble
# bash 06_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax

# expt="ssp585"

# Present
# yearmin=2017
# yearmax=2026

# for t in ${o2_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	if [[ $t == "MRI-ESM2-0" ]]
# 		then
# 		bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g lonlat -v $var -e $expt
# 	else
# 		bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt
# 	fi

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the ensemble
# bash 06_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax

# Mid-century
# yearmin=2046
# yearmax=2055

# for t in ${o2_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# Removing this because it has already been done above

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# #Make the ensemble
# bash 06_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax

# End of the century
# yearmin=2091
# yearmax=2100

# for t in ${o2_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# Removing this because it has already been done above

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the ensemble
# bash 06_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax

########################## CHLOROPHYLL ##########################

declare -a ch_list=("CMCC-ESM2" "MIROC-ES2L" "MRI-ESM2-0" "NorESM2-LM")
var="chlos"
# expt="historical"
# yearmin=1956
# yearmax=1984

# for t in ${ch_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	if [[ $t == "MRI-ESM2-0" ]]
# 		then
# 		bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g lonlat -v $var -e $expt
# 	else
# 		bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt
# 	fi

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the ensemble
# bash 06_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax

expt="ssp585"

# Present
# yearmin=2017
# yearmax=2026

# for t in ${ch_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	if [[ $t == "MRI-ESM2-0" ]]
# 		then
# 		bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g lonlat -v $var -e $expt
# 	else
# 		bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt
# 	fi

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the ensemble
# bash 06_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax

# Mid-century
# yearmin=2046
# yearmax=2055

# for t in ${ch_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# Removing this line because it has already been done above

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the ensemble
# bash 06_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax

# End of the century
yearmin=2091
yearmax=2100

for t in ${ch_list[@]}; do
	# Merge the files into 1 file per model per scenario
	# Removing this line because it has already been done above

	# Select years we want
	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

	# Remap to 1x1 degree grid
	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
	rm selectyears/* # Free up space

	# Change frequency
	bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

	echo $t
done

# Make the ensemble
bash 06_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax
