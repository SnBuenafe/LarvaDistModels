#!/bin/bash

############# TEMPERATURE

# declare -a tmp_list=("BCC-CSM2-MR" "CMCC-CM2-SR5" "CMCC-ESM2" "FGOALS-f3-L" "FGOALS-g3" "MIROC6" "MIROC-ES2L" "MRI-ESM2-0" "NorESM2-LM")
var="tos"
expt="historical"

# for t in ${tmp_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f 1956 -l 1984

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $expt 

# 	echo $t
# done

# Make the ensemble
bash 06_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean"


############# pH

# declare -a ph_list=("CMCC-ESM2" "MIROC-ES2L" "NorESM2-LM")
var="phos"
expt="historical"

# for t in ${ph_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f 1956 -l 1984

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $expt 

# 	echo $t
# done

# # Make the ensemble
bash 06_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean"

############# OXYGEN

# declare -a o2_list=("CMCC-ESM2" "MIROC-ES2L" "MRI-ESM2-0" "NorESM2-LM")
var="o2os"
expt="historical"

# for t in ${o2_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f 1956 -l 1984

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $expt 

# 	echo $t
# done

# # Make the ensemble
bash 06_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean"

############# CHLOROPHYLL

# declare -a ch_list=("CMCC-ESM2" "MIROC-ES2L" "MRI-ESM2-0" "NorESM2-LM")
var="chlos"
expt="historical"

# for t in ${ch_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f 1956 -l 1984

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $expt 

# 	echo $t
# done

# Make the ensemble
bash 06_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean"
