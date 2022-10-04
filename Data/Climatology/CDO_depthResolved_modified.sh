#!/bin/bash

cd ~/Documents/GitHub/LarvaDistModels/Data/Climatology

# Take inputs from user

while getopts m:g:l:t:d:u: flag
do
    case "${flag}" in
        m) model=${OPTARG};;
        g) gridname=${OPTARG};;
        v) var=${OPTARG};; # tos, o2os, phos, ...
        u) units=${OPTARG};; # depth units
        n) grid=${OPTARG};; # gn or gr
    esac
done

echo "User inputs:";
echo "Model: $model";
echo "Grid to be selected: $gridname";
echo "Variable name: $var";
echo "Depth units: $units";
echo "Grid type: $grid";

# Define directories

indir="raw/"
outdir="crop/"
mergedir="merged/"
finaldir="final/"

# Define latitudinal boundaries (longitude min and max are already defined)
#latmin=-79
#latmax=-50
#lonmin=-20
#lonmax=50

# defining arrays
declare -a ExpArray=("ssp585", "historical")

for e in ${ExpArray[@]}; do
    echo $e
        curr_files=($(ls ${indir}*$var*$model*$e*))
        num_files=${#curr_files[@]}

        for((i=0; i<=num_files-1; i++)); do
            curr_file=${curr_files[i]}
            #cdo -sinfov $curr_file # Print details

            out_name1="tmpfile1.nc"
            cdo -selgrid,$gridname $curr_file $out_name1

            #if [[ "$model" == "AWI-CM-1-1-MR" ]] # For models with unstructured grids (possibly those < 100km in res?)
            #    then
            #    cdo remapcon,global_1 $out_name1 temporary.nc
            #    out_name1="temporary.nc"
            #else
            #    :
            #fi

            out_name2="tmpfile2.nc"
            cdo -remapbil,global_1 $out_name1 $out_name2 # Remapping to 1x1 global grid

            #out_name3="tmpfile3.nc"
            #cdo -sellonlatbox,$lonmin,$lonmax,$latmin,$latmax $out_name2 $out_name3 # crop the data

            out_name4="tmpfile4.nc"
            cdo -L selyear,2015/2100 -selname,$var $out_name2 $out_name4

            if [[ "grid" == "gn" ]]
                then
                annual_name=${curr_file/_gn_/_cropped_} # Change name
            else # if grid = "gr"
                annual_name=${curr_file/_gr_/_cropped_}
            fi

            annual_name=${annual_name/Omon/Oyr}
            annual_name=${annual_name/$indir/$outdir} # Change directory
            cdo -yearmean $out_name4 $annual_name # Change frequency from monthly to yearly

            # Clean up
            rm $out_name1 $out_name2 $out_name3 $out_name4
        done
    
    # Merge files into just one file for each model
    merged_name=${annual_name::${#annual_name}-16} # Remove dates
    merged_name=${merged_name/_cropped_/_cropped_merged.nc} # Change name
    merged_name=${merged_name/$outdir/$mergedir} # Change directory
    cdo -O -mergetime $outdir*$var*$model*$e* $merged_name
    echo $outdir*$var*$model*$e*

    if [[ "$var" == "tos" ]] || [[ "$var" == "o2os" ]] || [[ "$var" == "phos" ]]
        then 
            out_name5="tmpfile5.nc"
            cdo setmisstonn $merged_name $out_name5 # set missing valueto nearest neighborhood value

            # Copy files from merged folder
            final_name=${merged_name/_cropped_merged/_surface} # Change name
            final_name=${final_name/$mergedir/$finaldir} # Change directory
            cp $out_name5 $final_name

            # Clean up
            rm $out_name5
    else
        out_name5="tmpfile5.nc"
        cdo setmisstonn $merged_name $out_name5 # set missing valueto nearest neighborhood value
        
        # Split the merged file into their unique levels
        prefix="tmpsplit/split_"
        cdo -splitlevel $out_name5 $prefix
        
        # loop through all split files
        curr_tmpfiles=($(ls tmpsplit)) 
        num_tmpfiles=${#curr_tmpfiles[@]}

        for((i=0; i<=num_tmpfiles-1; i++)); do
            
            file=${curr_tmpfiles[i]}
            curr_tmpfile=${curr_tmpfiles[i]:6:6}

            if [[ $units == "m" ]]
                then epi=200
                meso=1000
            else
                epi=20000
                meso=100000
            fi

            if [ $(echo "$curr_tmpfile <= $epi" | bc) -eq 1 ];
                then mv tmpsplit/$file epipelagic/ # Move directory
            elif [ $(echo "$curr_tmpfile > $epi && $curr_tmpfile <= $meso" | bc) -eq 1 ];
                then mv tmpsplit/$file mesopelagic/ # Move directory
            else
                mv tmpsplit/$file bathypelagic/ # Move directory
            fi
        done

        # get ensemble mean for epipelagic, mesopelagic, and bathypelagic
        epipelagic_name=${merged_name/_cropped_merged.nc/_epipelagic.nc}
        epipelagic_name=${epipelagic_name/$mergedir/$finaldir} # Change directory
        cdo -ensmean epipelagic/* $epipelagic_name

        mesopelagic_name=${merged_name/_cropped_merged.nc/_mesopelagic.nc}
        mesopelagic_name=${mesopelagic_name/$mergedir/$finaldir} # Change directory
        cdo -ensmean mesopelagic/* $mesopelagic_name

        bathypelagic_name=${merged_name/_cropped_merged.nc/_bathypelagic.nc}
        bathypelagic_name=${bathypelagic_name/$mergedir/$finaldir} # Change directory
        cdo -ensmean bathypelagic/* $bathypelagic_name

        # rm tmpfiles in epipelagic, mesopelagic, and bathypelagic folders
        rm epipelagic/* mesopelagic/* bathypelagic/* $out_name5
    fi
    
done
