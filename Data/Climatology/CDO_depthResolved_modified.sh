#!/bin/bash

cd ~/Documents/GitHub/LarvaDistModels/Data/Climatology

# Take inputs from user

while getopts m:g:l:t:d:u: flag
do
    case "${flag}" in
        m) model=${OPTARG};;
        g) gridname=${OPTARG};;
        l) lonname=${OPTARG};;
        t) latname=${OPTARG};;
        d) depth=${OPTARG};; # tos or thetao
        u) units=${OPTARG};;
    esac
done

echo "User inputs:";
echo "Model: $model";
echo "Grid to be selected: $gridname";
echo "Longitude name: $lonname";
echo "Latitude name: $latname";
echo "Surface or depth-resolved: $depth";
echo "Depth units: $units";

# Define directories

indir="raw/"
outdir="crop/"
mergedir="merged/"
finaldir="final1/"

# Define latitudinal boundaries (longitude min and max are already defined)
#latmin=-79
#latmax=-50
#lonmin=-20
#lonmax=50

# defining arrays
declare -a ExpArray=("ssp585")

for e in ${ExpArray[@]}; do
    echo $e
        curr_files=($(ls ${indir}*$depth*$model*$e*))
        num_files=${#curr_files[@]}

        for((i=0; i<=num_files-1; i++)); do
            curr_file=${curr_files[i]}
            cdo -sinfov $curr_file # Print details

            out_name1="tmpfile1.nc"
            cdo -selgrid,$gridname $curr_file $out_name1

            if [[ "$model" == "AWI-CM-1-1-MR" ]] # For models with unstructured grids (possibly those < 100km in res?)
                then
                cdo remapcon,global_1 $out_name1 temporary.nc
                out_name1="temporary.nc"
            else
                :
            fi

            out_name2="tmpfile2.nc"
            cdo -remapbil,global_1 $out_name1 $out_name2 # Remapping to 1x1 global grid; should I do this? There was at least one model that didn't span the globe...

            #out_name3="tmpfile3.nc"
            #cdo -sellonlatbox,$lonmin,$lonmax,$latmin,$latmax $out_name2 $out_name3 # crop the data

            out_name4="tmpfile4.nc"
            cdo -L selyear,2015/2100 -selname,$depth -chname,$lonname,lon -chname,$latname,lat $out_name2 $out_name4

            # Change frequency from monthly to yearly
            annual_name=${curr_file/_gn_/_cropped_} # Change name
            #annual_name=${curr_file/_gr_/_cropped_}
            annual_name=${annual_name/Omon/Oyr}
            annual_name=${annual_name/$indir/$outdir} # Change directory
            cdo -yearmean $out_name4 $annual_name

            # Clean up
            rm $out_name1 $out_name2 $out_name3 $out_name4
        done
    
    # Merge files into just one file for each model
    merged_name=${annual_name::${#annual_name}-16} # Remove dates
    merged_name=${merged_name/_cropped_/_cropped_merged.nc} # Change name
    merged_name=${merged_name/$outdir/$mergedir} # Change directory
    cdo -O -mergetime $outdir*$depth*$model*$e* $merged_name
    echo $outdir*$depth*$model*$e*

    if [[ "$depth" == "tos" ]] || [[ "$depth" == "o2os" ]] || [[ "$depth" == "phos" ]]
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
