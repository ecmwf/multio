import os
from GenerateEncoding import (
    toYAML,
    toDictRepres,
    partialRule,
    combinePartialRules,
    mergePartialRules,
    buildRule,
    matchType,
    matchParam,
    PointInTime,
    TimeRange,
    levelConfig,
    paramConfig,
    ParamConfig,
    EnsembleConfig,
    TimeConfig,
    DirectionsFrequenciesConfig,
    hasType,
    lacksType,
    matchType,
    marsType,
    IdentTemplateNumber,
    gridDefinition,
    localUse,
    ChemConfig,
    PeriodConfig,
    DataRepresentation,
)

TYPES = [
    partialRule(
        [matchType("type", "forecast")],
        [marsType("fc"), IdentTemplateNumber(templateNumber=0)],
    )
]

GRIDS = [
    partialRule([matchType("repres", "gaussian-grid")], [gridDefinition(40, "gg")])
]

LOCALSECTION = [
    partialRule([lacksType("anoffset")], [localUse(1)]),
    partialRule([hasType("anoffset")], [localUse(36)]),
]

PROCESSTYPES = [
    partialRule([lacksType("number")], []),
    partialRule([hasType("number")], [EnsembleConfig()]),
]

PARAM_LEVTYPE = [
    # cloudbase
    partialRule(
        [matchType("levtype", "sfc"), matchParam([228023])],
        [PointInTime(), levelConfig("cloudbase")],
    ),
    # entireAtmosphere
    partialRule(
        [
            matchType("levtype", "sfc"),
            matchParam(
                [
                    59,
                    "78:79",
                    "136:137",
                    164,
                    206,
                    "162059:162063",
                    "162071:162072",
                    162093,
                    228044,
                    228050,
                    228052,
                    "228088:228090",
                    228164,
                    260132,
                ]
            ),
        ],
        [PointInTime(), levelConfig("entireAtmosphere")],
    ),
    # entireLake
    partialRule(
        [matchType("levtype", "sfc"), matchParam([228007, 228011])],
        [PointInTime(), levelConfig("entireLake")],
    ),
    # heightAboveGround - statistical
    partialRule(
        [matchType("levtype", "sfc"), matchParam([123])],
        [
            levelConfig("heightAboveGround"),
            paramConfig("paramIdECMF"),
            TimeRange(
                type="fixed-timerange",
                typeOfStatisticalProcessing="max",
                overallLengthOfTimeRange="6h",
                descriptiveName="max-over-last-6h",
            ),
        ],
    ),
    partialRule(
        [matchType("levtype", "sfc"), matchParam([121])],
        [
            levelConfig("heightAboveGround"),
            paramConfig("paramId"),
            TimeRange(
                type="fixed-timerange",
                typeOfStatisticalProcessing="max",
                overallLengthOfTimeRange="6h",
                descriptiveName="max-over-last-6h",
            ),
        ],
    ),
    partialRule(
        [matchType("levtype", "sfc"), matchParam([49, 201])],
        [
            levelConfig("heightAboveGround"),
            paramConfig("paramId"),
            TimeRange(
                type="since-last-post-processing-step",
                typeOfStatisticalProcessing="max",
                descriptiveName="max-since-last-pp",
            ),
        ],
    ),
    partialRule(
        [matchType("levtype", "sfc"), matchParam([122])],
        [
            levelConfig("heightAboveGround"),
            paramConfig("paramId"),
            TimeRange(
                type="fixed-timerange",
                typeOfStatisticalProcessing="min",
                overallLengthOfTimeRange="6h",
                descriptiveName="min-over-last-6h",
            ),
        ],
    ),
    partialRule(
        [matchType("levtype", "sfc"), matchParam([202])],
        [
            levelConfig("heightAboveGround"),
            paramConfig("paramId"),
            TimeRange(
                type="since-last-post-processing-step",
                typeOfStatisticalProcessing="min",
                descriptiveName="min-since-last-pp",
            ),
        ],
    ),
    # heightAboveGround - point in time
    partialRule(
        [
            matchType("levtype", "sfc"),
            matchParam(["165:168", 207, 174096, 228029, 228037, "228131:228132"]),
        ],
        [PointInTime(), levelConfig("heightAboveGround")],
    ),
    # heightAboveSea - point in time
    partialRule(
        [matchType("levtype", "sfc"), matchParam([140245, 140249, 140233])],
        [PointInTime(), levelConfig("heightAboveSea")],
    ),
    # highCloudLayer - point in time
    partialRule(
        [matchType("levtype", "sfc"), matchParam([3075])],
        [PointInTime(), levelConfig("highCloudLayer")],
    ),
    # mediumCloudLayer - point in time
    partialRule(
        [matchType("levtype", "sfc"), matchParam([3074])],
        [PointInTime(), levelConfig("mediumCloudLayer")],
    ),
    # lowCloudLayer - point in time
    partialRule(
        [matchType("levtype", "sfc"), matchParam([3074])],
        [PointInTime(), levelConfig("lowCloudLayer")],
    ),
    # iceLayerOnWater - point in time
    partialRule(
        [matchType("levtype", "sfc"), matchParam([228014])],
        [PointInTime(), levelConfig("iceLayerOnWater")],
    ),
    # iceTopOnWater - point in time
    partialRule(
        [matchType("levtype", "sfc"), matchParam([228013])],
        [PointInTime(), levelConfig("iceTopOnWater")],
    ),
    # lakeBottom - point in time
    partialRule(
        [matchType("levtype", "sfc"), matchParam([228010])],
        [PointInTime(), levelConfig("lakeBottom")],
    ),
    # meanSea - point in time
    partialRule(
        [matchType("levtype", "sfc"), matchParam([151])],
        [PointInTime(), levelConfig("meanSea")],
    ),
    # mixedLayerParcel - point in time
    partialRule(
        [matchType("levtype", "sfc"), matchParam(["228231:228234"])],
        [PointInTime(), levelConfig("mixedLayerParcel")],
    ),
    # mixingLayer - point in time
    partialRule(
        [matchType("levtype", "sfc"), matchParam(["228008:228009"])],
        [PointInTime(), levelConfig("mixingLayer")],
    ),
    # mostUnstableParcel - point in time
    partialRule(
        [matchType("levtype", "sfc"), matchParam(["228235:228237"])],
        [PointInTime(), levelConfig("mostUnstableParcel")],
    ),
    # nominalTop - point in time
    partialRule(
        [matchType("levtype", "sfc"), matchParam(["178:179", "208:209", 212])],
        [PointInTime(), levelConfig("nominalTop")],
    ),
    # tropopause - point in time
    partialRule(
        [matchType("levtype", "sfc"), matchParam([228045])],
        [PointInTime(), levelConfig("tropopause")],
    ),
    # surface - since beginning - chem - era6
    partialRule(
        [
            matchType("levtype", "sfc"),
            matchParam(["228080:228082", "233032:233035", "235062:235064"]),
        ],
        [
            levelConfig("surface"),
            paramConfig("paramId", "era6"),
            ChemConfig(),
            TimeRange(
                type="since-beginning-of-forecast",
                typeOfStatisticalProcessing="accumul",
                descriptiveName="since-beginning",
            ),
        ],
    ),
    # surface - since beginning
    partialRule(
        [
            matchType("levtype", "sfc"),
            matchParam(
                [
                    8,
                    9,
                    20,
                    44,
                    45,
                    47,
                    50,
                    57,
                    58,
                    "142:147",
                    169,
                    "175:177",
                    "180:182",
                    189,
                    "195:197",
                    205,
                    "210:211",
                    213,
                    228,
                    239,
                    240,
                    3062,
                    3099,
                    "162100:162113",
                    "222001:222256",
                    228021,
                    228022,
                    228129,
                    228130,
                    228143,
                    228144,
                    228216,
                    228228,
                    228251,
                    231001,
                    231002,
                    231003,
                    231005,
                    231010,
                    231012,
                    231057,
                    231058,
                    "233000:233031",
                    260259,
                ]
            ),
        ],
        [
            levelConfig("surface"),
            paramConfig("paramId"),
            TimeRange(
                type="since-beginning-of-forecast",
                typeOfStatisticalProcessing="accumul",
                encodeStepZero=True,
                descriptiveName="since-beginning",
            ),
        ],
    ),
    # surface - average over last 1h
    partialRule(
        [matchType("levtype", "sfc"), matchParam([228051, 228053])],
        [
            levelConfig("surface"),
            paramConfig("paramId"),
            TimeRange(
                type="fixed-timerange",
                typeOfStatisticalProcessing="average",
                overallLengthOfTimeRange="1h",
                descriptiveName="average-over-last-1h",
            ),
        ],
    ),
    # surface - average over last 3h
    partialRule(
        [matchType("levtype", "sfc"), matchParam([228057, 228059])],
        [
            levelConfig("surface"),
            paramConfig("paramId"),
            TimeRange(
                type="fixed-timerange",
                typeOfStatisticalProcessing="average",
                overallLengthOfTimeRange="3h",
                descriptiveName="average-over-last-3h",
            ),
        ],
    ),
    # surface - average over last 6h
    partialRule(
        [matchType("levtype", "sfc"), matchParam([228058, 228060])],
        [
            levelConfig("surface"),
            paramConfig("paramId"),
            TimeRange(
                type="fixed-timerange",
                typeOfStatisticalProcessing="average",
                overallLengthOfTimeRange="6h",
                descriptiveName="average-over-last-6h",
            ),
        ],
    ),
    # surface - max over last 3h - paramIdECMF
    partialRule(
        [matchType("levtype", "sfc"), matchParam([228026])],
        [
            levelConfig("surface"),
            paramConfig("paramIdECMF"),
            TimeRange(
                type="fixed-timerange",
                typeOfStatisticalProcessing="max",
                overallLengthOfTimeRange="3h",
                descriptiveName="max-over-last-3h",
            ),
        ],
    ),
    # surface - max over last 3h
    partialRule(
        [matchType("levtype", "sfc"), matchParam([228028])],
        [
            levelConfig("surface"),
            paramConfig("paramId"),
            TimeRange(
                type="fixed-timerange",
                typeOfStatisticalProcessing="max",
                overallLengthOfTimeRange="3h",
                descriptiveName="max-over-last-3h",
            ),
        ],
    ),
    # surface - max over last 6h
    partialRule(
        [matchType("levtype", "sfc"), matchParam([228224, 228035, 228036])],
        [
            levelConfig("surface"),
            paramConfig("paramId"),
            TimeRange(
                type="fixed-timerange",
                typeOfStatisticalProcessing="max",
                overallLengthOfTimeRange="6h",
                descriptiveName="max-over-last-6h",
            ),
        ],
    ),
    # surface - min over last 3h - paramIdECMF
    partialRule(
        [matchType("levtype", "sfc"), matchParam([228027])],
        [
            levelConfig("surface"),
            paramConfig("paramIdECMF"),
            TimeRange(
                type="fixed-timerange",
                typeOfStatisticalProcessing="min",
                overallLengthOfTimeRange="3h",
                descriptiveName="min-over-last-3h",
            ),
        ],
    ),
    # TODO - paramID is duplacted here ?
    # surface - min over last 3h
    partialRule(
        [matchType("levtype", "sfc"), matchParam([228027])],
        [
            levelConfig("surface"),
            paramConfig("paramId"),
            TimeRange(
                type="fixed-timerange",
                typeOfStatisticalProcessing="min",
                overallLengthOfTimeRange="3h",
                descriptiveName="min-over-last-3h",
            ),
        ],
    ),
    # surface - mode over last 1h
    partialRule(
        [matchType("levtype", "sfc"), matchParam([260320])],
        [
            levelConfig("surface"),
            paramConfig("paramId"),
            TimeRange(
                type="fixed-timerange",
                typeOfStatisticalProcessing="mode",
                overallLengthOfTimeRange="1h",
                descriptiveName="mode-over-last-1h",
            ),
        ],
    ),
    # surface - mode over last 3h
    partialRule(
        [matchType("levtype", "sfc"), matchParam([260321])],
        [
            levelConfig("surface"),
            paramConfig("paramId"),
            TimeRange(
                type="fixed-timerange",
                typeOfStatisticalProcessing="mode",
                overallLengthOfTimeRange="3h",
                descriptiveName="mode-over-last-3h",
            ),
        ],
    ),
    # surface - mode over last 6h
    partialRule(
        [matchType("levtype", "sfc"), matchParam([260339])],
        [
            levelConfig("surface"),
            paramConfig("paramId"),
            TimeRange(
                type="fixed-timerange",
                typeOfStatisticalProcessing="mode",
                overallLengthOfTimeRange="6h",
                descriptiveName="mode-over-last-6h",
            ),
        ],
    ),
    # surface - severity over last 1h
    partialRule(
        [matchType("levtype", "sfc"), matchParam([260318])],
        [
            levelConfig("surface"),
            paramConfig("paramId"),
            TimeRange(
                type="fixed-timerange",
                typeOfStatisticalProcessing="severity",
                overallLengthOfTimeRange="1h",
                descriptiveName="severity-over-last-1h",
            ),
        ],
    ),
    # surface - severity over last 3h
    partialRule(
        [matchType("levtype", "sfc"), matchParam([260319])],
        [
            levelConfig("surface"),
            paramConfig("paramId"),
            TimeRange(
                type="fixed-timerange",
                typeOfStatisticalProcessing="severity",
                overallLengthOfTimeRange="3h",
                descriptiveName="severity-over-last-3h",
            ),
        ],
    ),
    # surface - severity over last 6h
    partialRule(
        [matchType("levtype", "sfc"), matchParam([260338])],
        [
            levelConfig("surface"),
            paramConfig("paramId"),
            TimeRange(
                type="fixed-timerange",
                typeOfStatisticalProcessing="severity",
                overallLengthOfTimeRange="6h",
                descriptiveName="severity-over-last-6h",
            ),
        ],
    ),
    # surface - instant
    partialRule(
        [
            matchType("levtype", "sfc"),
            matchParam(
                [
                    "26:32",
                    33,
                    "34:43",
                    "66:67",
                    74,
                    129,
                    134,
                    139,
                    141,
                    148,
                    159,
                    "160:163",
                    170,
                    "172:174",
                    "186:188",
                    198,
                    "229:232",
                    "234:236",
                    238,
                    "243:245",
                    3020,
                    3073,
                    3074,
                    3075,
                    160198,
                    210200,
                    210201,
                    210202,
                    228003,
                    228012,
                    210262,
                    210263,
                    210264,
                    "228015:228020",
                    228024,
                    228032,
                    "228046:228048",
                    228141,
                    "228217:228221",
                    228227,
                    228239,
                    228240,
                    228241,
                    "228246:228247",
                    260015,
                    260048,
                    260109,
                    260121,
                    260123,
                    262139,
                    262140,
                    260289,
                    260509,
                ]
            ),
        ],
        [PointInTime(), levelConfig("surface"), paramConfig("paramId")],
    ),
    # surface - instant - chem
    partialRule(
        [matchType("levtype", "sfc"), matchParam(["228083:228085"])],
        [PointInTime(), levelConfig("surface"), paramConfig("paramId"), ChemConfig()],
        namePrefix="chem",
    ),
    # surface - instant - wam_int
    partialRule(
        [
            matchType("levtype", "sfc"),
            matchParam(
                [
                    "140098:140105",
                    "140112:140113",
                    "140121:140129",
                    "140207:140209",
                    "140211:140212",
                    "140214:140232",
                    "140234:140239",
                    140244,
                    "140252:140254",
                ]
            ),
        ],
        [PointInTime(), levelConfig("surface"), paramConfig("paramId")],
        namePrefix="wam_int",
    ),
    # surface - instant - wam_period
    partialRule(
        [matchType("levtype", "sfc"), matchParam(["140114:140120"])],
        [PointInTime(), levelConfig("surface"), paramConfig("paramId"), PeriodConfig()],
        namePrefix="wam_period",
    ),
    # surface - instant - wam_spectra
    partialRule(
        [matchType("levtype", "sfc"), matchParam([140251])],
        [
            PointInTime(),
            levelConfig("surface"),
            paramConfig("paramId"),
            DirectionsFrequenciesConfig(),
        ],
        namePrefix="wam_spectra",
    ),
    # surface - max since last pp
    partialRule(
        [matchType("levtype", "sfc"), matchParam([228226])],
        [
            levelConfig("surface"),
            paramConfig("paramId"),
            TimeRange(
                type="since-last-post-processing-step",
                typeOfStatisticalProcessing="max",
                descriptiveName="max-since-last-pp",
            ),
        ],
        namePrefix="max_since_last_pp",
    ),
    # sfc wam spec instant
    partialRule(
        [matchType("levtype", "sfc"), matchParam([140251])],
        [PointInTime(), paramConfig("paramId"), DirectionsFrequenciesConfig()],
    ),
]

PACKING = [
    partialRule(
        [matchType("packing", "simple")],
        [DataRepresentation(templateNumber=0, descriptiveName="simple")],
    ),
    partialRule(
        [matchType("packing", "ccsds")],
        [DataRepresentation(templateNumber=42, descriptiveName="ccsds")],
    ),
]


rules = list(
    combinePartialRules(
        [TYPES, GRIDS, LOCALSECTION, PROCESSTYPES, PARAM_LEVTYPE, PACKING]
    )
)

encodedRules = [buildRule(mergePartialRules(rl)) for rl in rules]

encodedRulesDict = {e.name: [] for e in encodedRules}
for rule in encodedRules:
    encodedRulesDict[rule.name].append(rule)

duplicatedRules = {key: val for (key, val) in encodedRulesDict.items() if len(val) > 1}


if len(duplicatedRules) > 0:
    raise ValueError(f"Not all rule names are unique")



def main():
    BASE_DIR = "test_output"

    if not os.path.exists(BASE_DIR):
        os.makedirs(BASE_DIR)

    for rule in encodedRules:
        fname = f"{BASE_DIR}/{rule.name}.yaml"
        with open(fname, "w") as fileOut:
            fileOut.write(toYAML(toDictRepres(rule)))



if __name__ == "__main__":
    main()
