#!/usr/bin/env python3

from typing import Dict, List

import yaml

from statistics_param_mappings import get_statistically_processed_parameter_mappings


def get_statistical_parameter_to_operation_mappings() -> List[Dict[str, int]]:
    statistically_processed_parameter_mappings = get_statistically_processed_parameter_mappings()

    statistical_parameter_to_operation_mappings = []
    for mappings in statistically_processed_parameter_mappings:
        for mapping in mappings["mappings"]:
            typeOfStatisticalProcessing = mapping["typeOfStatisticalProcessing"]
            param_statistic = mapping["paramOut"]

            statistical_parameter_to_operation_mappings.append({
                "param": param_statistic,
                "typeOfStatisticalProcessing": typeOfStatisticalProcessing,
            })
    statistical_parameter_to_operation_mappings.sort(key=lambda d: d["param"])
    return statistical_parameter_to_operation_mappings


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description=(
            "List the statistics parameters and type of statistical processing."
            "The source of the data is the ECMWF Parameter Database.\n\n"
            "You must have access to the parameter database on the ECMWF HPC to run this script!"
        ), formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    args = parser.parse_args()

    statistical_parameter_to_operation_mappings = get_statistical_parameter_to_operation_mappings()

    print(yaml.dump(statistical_parameter_to_operation_mappings, sort_keys=False))
