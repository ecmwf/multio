#!/usr/bin/env python3

from typing import Dict, List

import yaml

from statistics_param_mappings import get_statistically_processed_parameter_mappings


def get_accumulated_to_average_parameter_mappings() -> List[Dict[str, int]]:
    statistically_processed_parameter_mappings = get_statistically_processed_parameter_mappings()

    accumulated_to_average_parameter_mappings = []
    for mappings in statistically_processed_parameter_mappings:
        param_intant = mappings["paramIn"]
        param_accumulated = None
        param_average = None
        for mapping in mappings["mappings"]:
            typeOfStatisticalProcessing = mapping["typeOfStatisticalProcessing"]
            if typeOfStatisticalProcessing == 0:
                param_average = mapping["paramOut"]
            if typeOfStatisticalProcessing == 1:
                param_accumulated = mapping["paramOut"]
        if param_accumulated and param_average:
            accumulated_to_average_parameter_mappings.append({
                "paramIn": param_accumulated,
                "paramOut": param_average,
            })
    accumulated_to_average_parameter_mappings.sort(key=lambda d: d["paramIn"])
    return accumulated_to_average_parameter_mappings


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description=(
            "List the accumulated parameters and their average variant by id."
            "The source of the data is the ECMWF Parameter Database.\n\n"
            "You must have access to the parameter database on the ECMWF HPC to run this script!"
        ), formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    args = parser.parse_args()

    accumulated_to_average_parameter_mappings = get_accumulated_to_average_parameter_mappings()

    print(yaml.dump(accumulated_to_average_parameter_mappings, sort_keys=False))
