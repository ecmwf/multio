#!/usr/bin/env python3

import sys
import os
from contextlib import closing
from typing import Any, Dict, List

import mysql.connector
import yaml

DB_CONFIG = {
    "host":     "webapps-db-prod",
    "user":     "ecmwf_ro",
    "password": "ecmwf_ro",
    "database": "param",
}

CENTRE_IDS = [
      0,  # WMO
     98,  # ECMWF
    -50,  # ERA6 (project)
    -60,  # DestinE (project)
]

ENCODINGS_QUERY = f"""
SELECT
  grib_encoding.param_id,
  grib_encoding.centre_id,
  grib_encoding.param_version,
  attribute.name,
  grib.attribute_value
FROM
  grib_encoding
  INNER JOIN grib ON grib.encoding_id = grib_encoding.id
  INNER JOIN attribute ON attribute.id = grib.attribute_id
  INNER JOIN param ON param.id = grib_encoding.param_id
WHERE
  grib_encoding.edition = 2 AND
  param.retired = 0 AND
  grib_encoding.is_mtg2_switch_1 = 1 AND
  ( {" OR\n    ".join(f"grib_encoding.centre_id = {c_id}" for c_id in CENTRE_IDS)} )
"""


def read_data(sql: str) -> List[tuple]:
    with closing(mysql.connector.connect(**DB_CONFIG)) as conn:
        with closing(conn.cursor()) as cursor:
            cursor.execute(sql)
            return cursor.fetchall()


def find_variants(
    statistically_processed_entries: list,
    instantaneous_encoding: dict,
) -> Dict[int, List[int]]:
    """
    Find the statically processed variants (encodings) for an instantaneous encoding
    with the same WMO lead centre as the instantaneous encoding.

    Args:
    statistically_processed_entries: List of all available statistically processed encodings.
    instantaneous_encoding: The instantaneous encoding for which to find the variants.

    Returns:
    A dictionary with the type of statistical processing of the variants found as keys and
    the corresponding variant param ids as values.

    The dictionary will be empty if no variants are found.
    """
    variants = {}
    for index, encoding in statistically_processed_entries.items():
        param_id, _, _ = index
        encoding_copy = encoding.copy()
        type_of_statistical_processing = encoding_copy.pop("typeOfStatisticalProcessing")

        if type_of_statistical_processing is None:
            continue

        if encoding_copy == instantaneous_encoding:
            variants.setdefault(type_of_statistical_processing, set()).add(param_id)

    for type_of_statistical_processing, param_ids in variants.items():
        variants[type_of_statistical_processing] = sorted(list(param_ids))
    return variants


def get_statically_processed_parameters_by_id() -> Dict[int, Dict[int, List[int]]]:
    """
    Get instananous parameters by id and their statically processed variants grouped by
    type of statistical processing.
    """
    formatted_data = {}
    for row in read_data(ENCODINGS_QUERY):
        param_id, centre_id, version, attribute_name, attribute_value = row

        # Exclude local centre parameters in the 500XXX range
        if param_id >= 500000:
            continue

        index = (param_id, centre_id, version)
        encoding_data = formatted_data.setdefault(index, {})

        # Expecting the same attribute to not be repeated, to safeguard against sql query possible issues
        if attribute_name in formatted_data[index]:
            raise Exception(param_id, centre_id, version, attribute_name, attribute_value)

        formatted_data[index][attribute_name] = attribute_value

    instantaneous_entries = {
        index: encoding
        for index, encoding in formatted_data.items()
        if "typeOfStatisticalProcessing" not in encoding
    }

    statistically_processed_entries = {
        index: encoding
        for index, encoding in formatted_data.items()
        if "typeOfStatisticalProcessing" in encoding
    }

    statistically_processed_variants = {}
    for index, encoding in instantaneous_entries.items():
        param_id, _, _ = index
        variants = find_variants(statistically_processed_entries, encoding)

        if not variants:
            continue

        for type_of_statistical_processing, param_ids in variants.items():
            statistically_processed_variants.setdefault(param_id, {}).setdefault(
                type_of_statistical_processing, set()
            ).update(param_ids)

    sorted_statistically_processed_variants = {}
    for param_id, variants in sorted(statistically_processed_variants.items()):
        sorted_variants = {
            type_of_statistical_processing: sorted(param_ids)
            for type_of_statistical_processing, param_ids in sorted(variants.items())
        }
        sorted_statistically_processed_variants[param_id] = sorted_variants
    return sorted_statistically_processed_variants


def get_statistically_processed_parameter_mappings() -> List[Dict[str, Any]]:
    statistically_processed_parameters_by_id = get_statically_processed_parameters_by_id()

    for param_id, variants in statistically_processed_parameters_by_id.items():
        for type_of_statistical_processing, variants in variants.items():
            if (len(variants) > 1):
                print(f"Mapping for param={param_id} and typeOfStatisticalProcessing={type_of_statistical_processing} contains more than one variant! Using the first variant {variants[0]}. All variants: {variants}", file=sys.stderr)

    return [
        {
            "paramIn": param_id,
            "mappings": [
                {
                    "typeOfStatisticalProcessing": int(type_of_statistical_processing),
                    "paramOut": variants[0],
                }
                for type_of_statistical_processing, variants in variants.items()
            ],
        }
        for param_id, variants in statistically_processed_parameters_by_id.items()
    ]


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description=(
            "List the instantaneous parameters by id and their statically processed variants "
            "under the same WMO lead centre.\n\n"
            "Only parameters with statically processed variants are included in the output.\n\n"
            "The source of the data is the ECMWF Parameter Database.\n\n"
            "You must have access to the parameter database on the ECMWF HPC to run this script!"
        ), formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    args = parser.parse_args()

    statistically_processed_parameter_mappings = get_statistically_processed_parameter_mappings()
    print(yaml.dump(statistically_processed_parameter_mappings, sort_keys=False))
