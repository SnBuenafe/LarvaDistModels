#!/bin/bash

# Run the code for all scenarios for all models
declare -a ExpArray=("ssp585" "historical")

######################################### COMPLETED
# tos
#declare -a tmp_list=("BCC-CSM2-MR" "CMCC-CM2-SR5" "CMCC-ESM2" "FGOALS-f3-L" "FGOALS-g3" "MIROC6" "MIROC-ES2L" "MRI-ESM2-0" "NorESM2-LM")

#for e in ${ExpArray[@]}; do
#    for t in ${tmp_list[@]}; do
#        bash cropClimate.sh -m "$t" -g curvilinear -v tos -n gn -e "$e" -a NA -x NA -o NA -i NA
#    done
#done

######################################### COMPLETED
# phos
# For natural grids (gn)
#declare -a ph_list=("CMCC-ESM2" "MIROC-ES2L")

# for e in ${ExpArray[@]}; do
#     for p in ${ph_list[@]}; do
#         bash cropClimate.sh -m "$p" -g curvilinear -v phos -n gn -e "$e" -a NA -x NA -o NA -i NA
#     done
# done

# For regular grids (gr)
# for e in ${ExpArray[@]}; do
#     bash cropClimate.sh -m "MRI-ESM2-0" -g lonlat -v phos -n gr -e "$e" -a NA -x NA -o NA -i NA
# done

# for e in ${ExpArray[@]}; do
#     bash cropClimate.sh -m "NorESM2-LM" -g curvilinear -v phos -n gr -e "$e" -a NA -x NA -o NA -i NA
# done

######################################### COMPLETED
# o2os
# For natural grids (gn)
# declare -a o2_list=("CMCC-ESM2" "MIROC-ES2L" "NorESM2-LM")

# for e in ${ExpArray[@]}; do
#     for o in ${o2_list[@]}; do
#         bash cropClimate.sh -m "$o" -g curvilinear -v o2os -n gn -e "$e" -a NA -x NA -o NA -i NA
#     done
# done

# # For MRI-ESM2-0
# bash cropClimate.sh -m "MRI-ESM2-0" -g curvilinear -v o2os -n gn -e "historical" -a NA -x NA -o NA -i NA
# bash cropClimate.sh -m "MRI-ESM2-0" -g lonlat -v o2os -n gr -e "ssp585" -a NA -x NA -o NA -i NA

######################################### COMPLETED
# chlos
# For natural grids (gn)
# declare -a ch_list=("CMCC-ESM2" "MIROC-ES2L" "NorESM2-LM")

# for e in ${ExpArray[@]}; do
#     for c in ${ch_list[@]}; do
#         bash cropClimate.sh -m "$c" -g curvilinear -v chlos -n gn -e "$e" -a NA -x NA -o NA -i NA
#     done
# done

# # For regualr grids
for e in ${ExpArray[@]}; do
    bash cropClimate.sh -m 'MRI-ESM2-0' -g lonlat -v chlos -n gr -e "$e" -a NA -x NA -o NA -i NA
done