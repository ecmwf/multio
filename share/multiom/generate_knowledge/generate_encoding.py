import pathlib
import os
import shutil
from GenerateEncoding import (
    toYAML,
    toDictRepres,
    partialRule,
    combinePartialRules,
    combineAndMergePartialRules,
    mergePartialRules,
    buildRule,
    matchType,
    matchParam,
    typeGE,
    typeGT,
    typeLE,
    typeLT,
    PointInTime,
    TimeRange,
    levelConfig,
    paramConfig,
    ParamConfig,
    EnsembleConfig,
    TimeConfig,
    TablesConfig,
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
    EncodeRule,
    composeAll,
    ComposeAll,
    MatchType,
)

# Helpers
isChemical = composeAll([hasType("chem"), lacksType("wavelength"), typeLT("chem", 900)])
isAerosol = composeAll([hasType("chem"), lacksType("wavelength"), typeGE("chem", 900)])
isOptical = composeAll([lacksType("chem"), hasType("wavelength")])
isChemicalOptical = composeAll([hasType("chem"), hasType("wavelength")])

#

TYPES = [
    # partialRule(
    #     [matchType("type", "forecast")],
    #     [marsType("fc"), IdentTemplateNumber(templateNumber=0)],
    # ),
    # partialRule(
    #     [matchType("type", "analysis")],
    #     [marsType("an"), IdentTemplateNumber(templateNumber=0)],
    # ),
    partialRule(
        [],
        [IdentTemplateNumber(templateNumber=0)],
    ),
]

GRIDS = [
    partialRule([matchType("repres", "gaussian-grid")], [gridDefinition(40, "gg")])
]

GRIDS_SH = [
    partialRule(
        [matchType("repres", "spherical-harmonics")], [gridDefinition(50, "complex")]
    )
]

LOCALSECTION = [
    partialRule([lacksType("anoffset")], [localUse(1)]),
    partialRule([hasType("anoffset")], [localUse(36)]),
]

PROCESSTYPES = [
    partialRule([lacksType("number")], []),
    partialRule([hasType("number")], [EnsembleConfig()]),
]


# SFC

PARAM_LEVTYPE_SFC = [
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
            matchParam(["165:168", 207, 174096, 129172, 228029, 228037, "228131:228132", "228239:228241"]),
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
        [matchType("levtype", "sfc"), matchParam([3073])],
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
        [
            TimeRange(
                type="since-beginning-of-forecast",
                typeOfStatisticalProcessing="accumul",
                encodeStepZero=True,
                descriptiveName="since-beginning",
            ),
            levelConfig("nominalTop"),
        ],
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
            isChemical,
        ],
        [
            levelConfig("surface"),
            paramConfig("paramId", "era6"),
            ChemConfig(),
            TimeRange(
                type="since-beginning-of-forecast",
                typeOfStatisticalProcessing="accumul",
                encodeStepZero=True,
                descriptiveName="since-beginning",
            ),
            TablesConfig(type="custom", localTablesVersion=0, tablesVersion=30),
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
    # partialRule(
    #     [matchType("levtype", "sfc"), matchParam([228027])],
    #     [
    #         levelConfig("surface"),
    #         paramConfig("paramId"),
    #         TimeRange(
    #             type="fixed-timerange",
    #             typeOfStatisticalProcessing="min",
    #             overallLengthOfTimeRange="3h",
    #             descriptiveName="min-over-last-3h",
    #         ),
    #     ],
    # ),
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
                    "15:18",
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
                    160198,
                    200199,
                    210200,
                    210201,
                    210202,
                    228003,
                    228012,
                    "210186:210191",
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
                    # 228239,
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
        [isChemical, matchType("levtype", "sfc"), matchParam(["228083:228085"])],
        [
            PointInTime(),
            levelConfig("surface"),
            paramConfig("paramId", "era6"),
            ChemConfig(),
        ],
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
        # namePrefix="max_since_last_pp",
    ),
    # sfc wam spec instant
    partialRule(
        [matchType("levtype", "sfc"), matchParam([140251])],
        [PointInTime(), paramConfig("paramId"), DirectionsFrequenciesConfig()],
    ),
]


# HL

PARAM_LEVTYPE_HL = [
    partialRule(
        [matchType("levtype", "hl"), matchParam([10, 131, 132])],
        [
            PointInTime(),
            levelConfig("heightAboveGround"),
            paramConfig("paramId", "era6"),
        ],
    ),
]


# ML

PARAM_LEVTYPE_ML = [
    partialRule(
        [matchType("levtype", "ml"), matchParam(["75:76", 133, 203, "246:248"])],
        [PointInTime(), levelConfig("hybrid"), paramConfig("paramId")],
    ),
    partialRule(
        [matchType("levtype", "ml"), matchParam(["162100:162113"])],
        [
            TimeRange(
                type="since-beginning-of-forecast",
                typeOfStatisticalProcessing="accumul",
                encodeStepZero=True,
                descriptiveName="since-beginning",
            ),
            levelConfig("hybrid"),
            paramConfig("paramId"),
        ],
    ),
]

PARAM_LEVTYPE_ML_SH = [
    partialRule(
        [matchType("levtype", "ml"), matchParam([77, 129, 130, 135, 138, 152, 155])],
        [PointInTime(), levelConfig("hybrid"), paramConfig("paramId")],
    ),
]


# PL

PL_LEVEL_CONFIGS = [
    partialRule(
        [typeGE("levelist", 100)],
        [levelConfig("isobaricinhpa")],
    ),
    partialRule(
        [typeLT("levelist", 100)],
        [levelConfig("isobaricinpa")],
    ),
]

PARAM_LEVTYPE_PL = combineAndMergePartialRules(
    [
        PL_LEVEL_CONFIGS,
        [
            partialRule(
                [
                    matchType("levtype", "pl"),
                    matchParam([60, "75:76", "129:135", 203, "246:248", 260290]),
                ],
                [PointInTime(), paramConfig("paramId")],
            ),
        ],
    ]
)

PARAM_LEVTYPE_PL_SH = combineAndMergePartialRules(
    [
        PL_LEVEL_CONFIGS,
        [
            partialRule(
                [
                    matchType("levtype", "pl"),
                    matchParam([129, 130, 135, 138, 152, 155, 157]),
                ],
                [PointInTime(), paramConfig("paramId")],
            ),
        ],
    ]
)


# SOIL

PARAM_LEVTYPE_SOL = [
    partialRule(
        [matchType("levtype", "sol"), matchParam([262024])],
        [PointInTime(), levelConfig("seaIceLayer"), paramConfig("paramId")],
    ),
    partialRule(
        [matchType("levtype", "sol"), matchParam([33, 74, 238, 228038, 228141])],
        [PointInTime(), levelConfig("snowLayer"), paramConfig("paramId")],
    ),
    partialRule(
        [matchType("levtype", "sol"), matchParam([260360, 260199, 183])],
        [PointInTime(), levelConfig("soilLayer"), paramConfig("paramId")],
    ),
]


# Combine all param levtype configurations

PARAM_LEVTYPE = (
    PARAM_LEVTYPE_SFC
    + PARAM_LEVTYPE_HL
    + PARAM_LEVTYPE_ML
    + PARAM_LEVTYPE_PL
    + PARAM_LEVTYPE_SOL
)
PARAM_LEVTYPE_SH = PARAM_LEVTYPE_ML_SH + PARAM_LEVTYPE_PL_SH

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

# TODO Usallay there is complex_simple and complex_ccsds
PACKING_SH = [
    partialRule(
        [matchType("packing", "complex")],
        [DataRepresentation(templateNumber=51, descriptiveName="complex")],
    )
]


rules = list(
    combinePartialRules(
        [TYPES, GRIDS, LOCALSECTION, PROCESSTYPES, PARAM_LEVTYPE, PACKING]
    )
)
rules_sh = list(
    combinePartialRules(
        [TYPES, GRIDS_SH, LOCALSECTION, PROCESSTYPES, PARAM_LEVTYPE_SH, PACKING_SH]
    )
)

encodedRules = [buildRule(mergePartialRules(rl)) for rl in rules + rules_sh]

encodedRulesDict = {e.name: [] for e in encodedRules}
for rule in encodedRules:
    encodedRulesDict[rule.name].append(rule)

duplicatedRules = {key: val for (key, val) in encodedRulesDict.items() if len(val) > 1}


if len(duplicatedRules) > 0:
    raise ValueError(f"Not all rule names are unique")


def templateCMakeFile(dir, subDirs):
    subDirsStr = "\n".join([f'add_subdirectory("{sd}")' for sd in subDirs])
    return f"""
file(GLOB encoding_rules RELATIVE ${{CMAKE_CURRENT_SOURCE_DIR}} "*.yaml")

{subDirsStr}


# Loop through each entry and add it as a subdirectory if it's a directory
foreach(rule ${{encoding_rules}})
    configure_file(${{CMAKE_CURRENT_SOURCE_DIR}}/${{rule}}
                   ${{CMAKE_CURRENT_BINARY_DIR}}/${{rule}}
                   COPYONLY)
endforeach()


install(
    FILES       ${{encoding_rules}}
    DESTINATION ${{MULTIOM_CONFIG_DIR}}/{dir}
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)
"""


# Take list of paths and return to a dictionary
def pathListToDict(pathList):
    def recSplitPathList(spl):
        tmp = {}
        for sp in spl:
            if len(sp) == 0:
                continue

            if sp[0] not in tmp.keys():
                tmp[sp[0]] = []
            tmp[sp[0]].append(sp[1:])

        return {key: recSplitPathList(subSpl) for (key, subSpl) in tmp.items()}

    return recSplitPathList(
        [list(filter(lambda s: s != "", p.split("/"))) for p in set(pathList)]
    )


def createCMakeFiles(relDir, pathList):
    pld = pathListToDict(pathList)

    def recPld(basePath, pld):
        for path, subPld in pld.items():

            newBasePath = f"{basePath}/{path}" if basePath != "" else path

            fileContent = templateCMakeFile(newBasePath, list(subPld.keys()))

            with open("/".join([relDir, newBasePath, "CMakeLists.txt"]), "w") as fileOut:
                fileOut.write(fileContent)

            recPld(newBasePath, subPld)

    recPld("", pld)


def pathForRule(baseDir: str, rule: EncodeRule):
    levtype = (
        "_".join(
            [
                f.filter.value
                for f in rule.filter.filter.filters
                if isinstance(f.filter, MatchType) and f.filter.type == "levtype"
            ]
        )
        if isinstance(rule.filter.filter, ComposeAll)
        else ""
    )

    marsType = None if rule.encode.identification.marsType is None else rule.encode.identification.marsType.type

    ensemble = "ensemble" if rule.encode.product.ensemble else "deterministic"
    packing = rule.encode.dataRepres.descriptiveName

    return (
        {"type": marsType, "packing": packing, "levtype": levtype, "process": ensemble},
        "/".join(filter(lambda d: d is not None, [baseDir,packing,ensemble,marsType,levtype])),
        f"{rule.name}.yaml",
    )


def main():
    RELATIVE_DIR = ".."
    BASE_DIR = "encodings"
    BASE_DIR_RULE_LIST = "{IFS_INSTALL_DIR}/share/multiom"
    ENCODING_RULES_SPLIT = ["packing", "process"]

    REL_BASE_DIR="/".join([RELATIVE_DIR, BASE_DIR])

    # Remove dir if already exists to avoid duplications or different directory structures
    if os.path.exists(REL_BASE_DIR):
        shutil.rmtree(REL_BASE_DIR)
    os.makedirs(REL_BASE_DIR)

    ruleFiles = [(pathForRule(BASE_DIR, rule), rule) for rule in encodedRules]

    for (attr, path, fname), rule in ruleFiles:
        pathlib.Path("/".join([RELATIVE_DIR, path])).mkdir(parents=True, exist_ok=True)
        with open("/".join([RELATIVE_DIR,path,fname]), "w") as fileOut:
            fileOut.write(toYAML(toDictRepres(rule)))

    pathList = list({r[0][1] for r in ruleFiles})

    createCMakeFiles(RELATIVE_DIR, pathList)

    def keyForRuleList(sel):
        return "-".join([sel[k] for k in ENCODING_RULES_SPLIT])

    mappedRuleFiles = [
        ({key: r[0][0][key] for key in ENCODING_RULES_SPLIT}, r[0][1], r[0][2])
        for r in ruleFiles
    ]
    rulesBySplit = {k: [] for k in {keyForRuleList(mr[0]) for mr in mappedRuleFiles}}
    allFiles=[]
    for sel, path, fname in mappedRuleFiles:
        key = keyForRuleList(sel)
        rulesBySplit[key].append({"file": "/".join([BASE_DIR_RULE_LIST, path, fname])})
        allFiles.append({"file": "/".join([BASE_DIR_RULE_LIST, path, fname])})

    for key, files in rulesBySplit.items():
        with open(f"{REL_BASE_DIR}/encoding-rules-{key}.yaml", "w") as fileOut:
            fileOut.write(toYAML({"encoding-rules": files}))

    with open(f"{REL_BASE_DIR}/encoding-rules.yaml", "w") as fileOut:
        fileOut.write(toYAML({"encoding-rules": allFiles}))


if __name__ == "__main__":
    main()
