#!/usr/bin/env bash
cd "$(dirname "$0")"

module purge

module load python3

echo "Generating file: statistics_param_mappings.yaml"
./statistics_param_mappings.py > statistics_param_mappings.yaml

echo "Generating file: statistics_operation_mappings.yaml"
./statistics_operation_mappings.py > statistics_operation_mappings.yaml

echo "Generating file: average_rate_param_mappings.yaml"
./average_rate_param_mappings.py > average_rate_param_mappings.yaml
