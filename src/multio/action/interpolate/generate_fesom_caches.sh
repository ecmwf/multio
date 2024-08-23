#!/usr/bin/env bash

#SBATCH -p par
#SBATCH -J generate_fesom_cache
#SBATCH -o generate_fesom_cache_%j.out
#SBATCH -N 1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=128
#SBATCH --mem=0
#SBATCH --time=02:00:00

if [[ -n $SLURM_JOB_ID ]]; then
    echo "Searching in $1 for files matching $2..."
    echo "Using generator tool: $BDIR/bin/cache-generator-fesom-2-mir"
    files=$(find ${1} -iname "${2}")
    for f in $files
    do
        fname=$(basename ${f})
        ${BDIR}/bin/cache-generator-fesom-2-mir --inputPath=${1} --inputFile=${fname} --nCols=${3}
    done
else
    sbatch --export=BDIR $0 $1 $2 $3
fi
