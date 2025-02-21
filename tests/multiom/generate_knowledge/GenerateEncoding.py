import re
import os
import itertools
import yaml
from functools import reduce
from enum import Enum
from typing import Union, List, Optional, Annotated, TypeAlias, Dict
from pydantic import (
    BaseModel,
    Field,
    ValidationError,
    validator,
    AfterValidator,
    model_validator,
)

from InferPDT import inferPDT, pdtMatcher, pdtCategoriesHandleOrder

# from pydantic_yaml import parse_yaml_raw_as, to_yaml_str


class Quoted(BaseModel):
    value: str


def quoted(v: str) -> Quoted:
    return Quoted(value=v)


# define a custom representer for strings
def quoted_presenter(dumper, data):
    """
    Preserve multiline strings when dumping yaml.
    https://github.com/yaml/pyyaml/issues/240
    """
    if "\n" in data:
        # Remove trailing spaces messing out the output.
        block = "\n".join([line.rstrip() for line in data.splitlines()])
        if data.endswith("\n"):
            block += "\n"
        return dumper.represent_scalar("tag:yaml.org,2002:str", block, style="|")
    return dumper.represent_scalar("tag:yaml.org,2002:str", data.value, style='"')


# yaml.add_representer(Quoted, quoted_presenter)
# yaml.representer.SafeRepresenter.add_representer(Quoted, quoted_presenter)
# yaml.add_representer(str, quoted_presenter)


class MyDumper(yaml.SafeDumper):
    # HACK: insert blank lines between top-level objects
    # inspired by https://stackoverflow.com/a/44284819/3786245
    def write_line_break(self, data=None):
        super().write_line_break(data)

        if len(self.indents) == 1:
            super().write_line_break()

    def increase_indent(self, flow=False, indentless=False):
        return super(MyDumper, self).increase_indent(flow, False)

    def represent_data(self, data):
        if isinstance(data, Quoted):
            return quoted_presenter(self, data)
        return super().represent_data(data)

    # def expect_block_sequence(self):
    #     indentless = (self.mapping_context and not self.indention)
    #     self.increase_indent(flow=True)
    #     self.state = self.expect_first_block_sequence_item


def toYAML(d):
    # return yaml.dump(d, Dumper=MyDumper, default_flow_style=False, indent=2, sort_keys=False)
    return yaml.dump(d, Dumper=MyDumper, default_flow_style=False, sort_keys=False)


def isParamRange(val: str) -> str:
    regex = re.compile(r"^\d+:\d+$")
    if regex.match(val):
        # TODO elaborate checking range
        return val
    raise ValueError(f"Not a valid param range {val}")


ParamCheckedType: TypeAlias = Union[Annotated[str, AfterValidator(isParamRange)], int]
ParamBaseType: TypeAlias = Union[str, int]


class Param(BaseModel):
    value: ParamCheckedType = Field(union_mode="left_to_right")


def param(v: ParamBaseType) -> Param:
    return Param(value=v)


class MatchParam(BaseModel):
    values: List[Param]


def matchParam(l: List[ParamBaseType]) -> MatchParam:
    return MatchParam(values=[param(v) for v in l])


class MatchType(BaseModel):
    type: str
    value: str


def matchType(t: str, v: str) -> MatchType:
    return MatchType(type=t, value=v)
    
class TypeTreshold(BaseModel):
    type: str
    operation: str
    treshold: int

def typeGE(t: str, v: int) -> TypeTreshold:
    return TypeTreshold(type=t, operation="greater-equal", treshold=v)
    
def typeGT(t: str, v: int) -> TypeTreshold:
    return TypeTreshold(type=t, operation="greater-than", treshold=v)
    
def typeLE(t: str, v: int) -> TypeTreshold:
    return TypeTreshold(type=t, operation="lower-equal", treshold=v)
    
def typeLT(t: str, v: int) -> TypeTreshold:
    return TypeTreshold(type=t, operation="lower-than", treshold=v)


class HasType(BaseModel):
    type: str


def hasType(t: str) -> HasType:
    return HasType(type=t)


class LacksType(BaseModel):
    type: str


def lacksType(t: str) -> LacksType:
    return LacksType(type=t)


RuleFilterType: TypeAlias = Union[
    MatchParam, HasType, LacksType, MatchType, TypeTreshold, "ComposeAll"
]


class RuleFilter(BaseModel):
    filter: RuleFilterType = Field(union_mode="left_to_right")


def ruleFilter(v: Union[RuleFilter, RuleFilterType]) -> RuleFilter:
    match v:
        case RuleFilter():
            return v
        case _:
            return RuleFilter(filter=v)


class ComposeAll(BaseModel):
    filters: List[RuleFilter]


def composeAll(vals: List[Union[RuleFilter, RuleFilterType]]) -> ComposeAll:
    return ComposeAll(filters=[ruleFilter(v) for v in vals])


# Encode


# Section 0
class IndicatorSection(BaseModel):
    templateNumber: int = 0


# Section 1


class OriginConfig(BaseModel):
    type: str = "default"
    subCentre: int = 0


class DataTypeConfig(BaseModel):
    type: str = "default"


class ReferenceTimeConfig(BaseModel):
    type: str = "default"


class TablesConfig(BaseModel):
    type: str = "default"
    tablesVersion: Optional[int] = None
    localTablesVersion: int = 0


# Explicit object for mars keyword "type" to allow partial creation
class MarsType(BaseModel):
    type: str


def marsType(type: str) -> MarsType:
    return MarsType(type=type)


# Explict object for partial creation
class IdentTemplateNumber(BaseModel):
    templateNumber: int = 0


class IdentificationSection(BaseModel):
    marsType: MarsType  # Used to put in name
    templateNumber: IdentTemplateNumber = IdentTemplateNumber()
    origin: OriginConfig = OriginConfig()
    dataType: DataTypeConfig = DataTypeConfig()
    referenceTime: ReferenceTimeConfig = ReferenceTimeConfig()
    tables: TablesConfig = TablesConfig()


# Section 2
class LocalUse(BaseModel):
    templateNumber: int = 1


def localUse(tn: int) -> LocalUse:
    return LocalUse(templateNumber=tn)


# Section 3
class GridDefinition(BaseModel):
    templateNumber: int = 40
    shortName: str = "gg"


def gridDefinition(tn: int, n: str) -> GridDefinition:
    return GridDefinition(templateNumber=tn, shortName=n)


# Section 4
class PointInTime(BaseModel):
    type: str = "default"
    descriptiveName: str = "instant"


class TimeRange(BaseModel):
    type: str
    typeOfStatisticalProcessing: str
    overallLengthOfTimeRange: Optional[str] = None
    encodeStepZero: Optional[bool] = None
    descriptiveName: str


TimeConfigType: TypeAlias = Union[PointInTime, TimeRange]


class TimeConfig(BaseModel):
    config: TimeConfigType = Field(union_mode="left_to_right")


def timeConfig(cfg: TimeConfigType) -> TimeConfig:
    return TimeConfig(config=cfg)


class ParamConfig(BaseModel):
    type: str = "paramId"
    datasetForLocal: Optional[str] = None


def paramConfig(t: str, ds: Optional[str] = None) -> ParamConfig:
    return ParamConfig(type=t, datasetForLocal=ds)


class LevelConfig(BaseModel):
    type: str


def levelConfig(t: str) -> LevelConfig:
    return LevelConfig(type=t)


class EnsembleConfig(BaseModel):
    type: str = "default"


class ChemConfig(BaseModel):
    type: str = "chemical"


class DirectionsFrequenciesConfig(BaseModel):
    type: str = "default"


class PeriodConfig(BaseModel):
    type: str = "default"


class PDT(BaseModel):
    templateNumber: int = 0


class PDTCategoryPair(BaseModel):
    category: str
    value: Optional[str]


def pdtCatPair(cat: str, val: str):
    return PDTCategoryPair(category=cat, value=val)


PDTCategoryDict: TypeAlias = Dict[str, str]


def pdtCategoryPairsToDict(pairs: List[PDTCategoryPair]) -> PDTCategoryDict:
    countDict = {pair.category: 0 for pair in pairs}
    for p in pairs:
        countDict[p.category] = countDict[p.category] + 1

    catMoreThanOnce = [cat for (cat, count) in countDict.items() if count > 1]
    if len(catMoreThanOnce) > 0:
        problematicPairs = [p for p in pairs if p.category in catMoreThanOnce]
        raise ValueError(
            f"The following categories appear more than once: {catMoreThanOnce} - {problematicPairs}"
        )

    return {pair.category: pair.value for pair in pairs if pair.value is not None}


class ProductDefinition(BaseModel):
    pdt: PDT = PDT()
    timeConfig: TimeConfig = timeConfig(PointInTime())
    param: ParamConfig = ParamConfig()
    level: Optional[LevelConfig] = None
    ensemble: Optional[EnsembleConfig] = None
    chemical: Optional[ChemConfig] = None
    directionsFrequencies: Optional[DirectionsFrequenciesConfig] = None
    periodRange: Optional[PeriodConfig] = None


# Section 5
class DataRepresentation(BaseModel):
    templateNumber: int = 0
    descriptiveName: str = "simple"


# Encode class


class Encode(BaseModel):
    type: str = "grib2"
    indicator: IndicatorSection = IndicatorSection()
    identification: IdentificationSection
    localUse: LocalUse = LocalUse()
    grid: GridDefinition = GridDefinition()
    product: ProductDefinition
    dataRepres: DataRepresentation = DataRepresentation()


# Final rule
def nameFromEncode(encode: Encode, additionalPrefix: Optional[str] = None):
    level = encode.product.level.type if encode.product.level is not None else None
    wave = (
        encode.product.directionsFrequencies.type
        if encode.product.directionsFrequencies is not None
        else None
    )
    periodRange = (
        encode.product.periodRange.type
        if encode.product.periodRange is not None
        else None
    )

    levelWaveStr = "-".join([l for l in [level, wave, periodRange] if l is not None])

    grid = encode.grid.shortName
    marsType = encode.identification.marsType.type
    ensemble = "ensemble" if encode.product.ensemble else "deterministic"
    time = encode.product.timeConfig.config.descriptiveName
    packing = encode.dataRepres.descriptiveName
    paramConfig = encode.product.param.type
    dataset = (
        ""
        if encode.product.param.datasetForLocal is None
        else f"-{encode.product.param.datasetForLocal}"
    )
    local = encode.localUse.templateNumber
    
    product=""
    if encode.product.chemical is not None:
        product+="-chem"
    if encode.product.directionsFrequencies is not None:
        product+="-wave_spec"
    if encode.product.periodRange is not None:
        product+="-wave_period"
    # if encode.product.satelite is not None:
    #     product+="-satelite"
    
    pref = "" if additionalPrefix is None else f"-{additionalPrefix}"

    return f"rule{pref}-{levelWaveStr}-{grid}-{marsType}{product}-{ensemble}-{time}-{packing}-{paramConfig}{dataset}-{local}"


class EncodeRule(BaseModel):
    tag: str = "grib2"
    filter: RuleFilter
    encode: Encode
    name: Optional[str] = None

    @model_validator(mode="after")
    def set_name(self):
        if self.name is None:
            self.name = nameFromEncode(self.encode)
        return self


# Emitting...


def toDictRepres(val):
    match val:
        case MatchParam():
            return {
                "type": "param",
                "operation": "match",
                "values": [toDictRepres(v) for v in val.values],
            }
        case Param():
            if isinstance(val.value, str):
                return quoted(val.value)
            return val.value
        case MatchType():
            return {"type": val.type, "operation": "match", "value": val.value}
        case TypeTreshold():
            return {"type": val.type, "operation": val.operation, "treshold": val.treshold}
        case HasType():
            return {"type": val.type, "operation": "has"}
        case LacksType():
            return {"type": val.type, "operation": "lacks"}
        case RuleFilter():
            return toDictRepres(val.filter)
        case ComposeAll():
            return {
                "type": "composed",
                "operation": "all",
                "filters": [toDictRepres(v) for v in val.filters],
            }
        case Encode():
            return {
                "type": val.type,
                "indicator-section": toDictRepres(val.indicator),
                "identification-section": toDictRepres(val.identification),
                "identification-section": toDictRepres(val.identification),
                "local-use-section": toDictRepres(val.localUse),
                "grid-definition-section": toDictRepres(val.grid),
                "product-definition-section": toDictRepres(val.product),
                "data-representation-section": toDictRepres(val.dataRepres),
            }
        case IndicatorSection():
            return {"template-number": val.templateNumber}
        case IdentificationSection():
            return {
                "template-number": val.templateNumber.templateNumber,
                "origin-configurator": toDictRepres(val.origin),
                "data-type-configurator": toDictRepres(val.dataType),
                "reference-time-configurator": toDictRepres(val.referenceTime),
                "tables-configurator": toDictRepres(val.tables),
            }
        case LocalUse():
            return {"template-number": val.templateNumber}
        case GridDefinition():
            return {"template-number": val.templateNumber}
        case ProductDefinition():
            return {
                "template-number": val.pdt.templateNumber,
                **toDictRepres(val.timeConfig),
                "param-configurator": toDictRepres(val.param),
                **(
                    {"level-configurator": toDictRepres(val.level)}
                    if val.level is not None
                    else {}
                ),
                **(
                    {"ensemble-configurator": toDictRepres(val.ensemble)}
                    if val.ensemble is not None
                    else {}
                ),
                **(
                    {"chemistry-configurator": toDictRepres(val.chemical)}
                    if val.chemical is not None
                    else {}
                ),
                **(
                    {
                        "directions-frequencies-configurator": toDictRepres(
                            val.directionsFrequencies
                        )
                    }
                    if val.directionsFrequencies is not None
                    else {}
                ),
                **(
                    {"period-configurator": toDictRepres(val.periodRange)}
                    if val.periodRange is not None
                    else {}
                ),
            }
        case DataRepresentation():
            return {"template-number": val.templateNumber}
        case OriginConfig():
            return {"type": val.type, "sub-centre": val.subCentre}
        case DataTypeConfig():
            return {"type": val.type}
        case ReferenceTimeConfig():
            return {"type": val.type}
        case TablesConfig():
            return {"type": val.type, **({} if val.tablesVersion is None else {"tables-version": val.tablesVersion}), "local-tables-version": val.localTablesVersion}
        case TimeConfig():
            match val.config:
                case PointInTime():
                    return {"point-in-time-configurator": toDictRepres(val.config)}
                case TimeRange():
                    return {"time-statistics-configurator": toDictRepres(val.config)}
        case PointInTime():
            return {"type": val.type}
        case TimeRange():
            return {
                "type": val.type,
                "type-of-statistical-processing": val.typeOfStatisticalProcessing,
                **(
                    {}
                    if val.overallLengthOfTimeRange is None
                    else {"overall-length-of-timerange": val.overallLengthOfTimeRange}
                ),
                **(
                    {}
                    if val.encodeStepZero is None
                    else {"encode-step-zero": val.encodeStepZero}
                ),
            }
        case ParamConfig():
            return {
                "type": val.type,
                **(
                    {}
                    if val.datasetForLocal is None
                    else {"dataset-for-local": val.datasetForLocal}
                ),
            }
        case LevelConfig():
            return {"type": val.type}
        case EnsembleConfig():
            return {"type": val.type}
        case ChemConfig():
            return {"type": val.type}
        case DirectionsFrequenciesConfig():
            return {"type": val.type}
        case PeriodConfig():
            return {"type": val.type}
        case EncodeRule():
            return {
                "tag": val.tag,
                "name": val.name,
                "filter": toDictRepres(val.filter),
                "encoder": toDictRepres(val.encode),
            }
        case _:
            raise ValueError(f"toDict not specialized for {val}")


# Rule combination

# Partial Sections
Section1Part: TypeAlias = Union[
    OriginConfig,
    DataTypeConfig,
    ReferenceTimeConfig,
    TablesConfig,
    MarsType,
    IdentTemplateNumber,
    IdentificationSection,
]

Section4Part: TypeAlias = Union[
    PDT,
    # PDTCategoryPair,
    TimeConfig,
    PointInTime,
    TimeRange,
    ParamConfig,
    LevelConfig,
    EnsembleConfig,
    ChemConfig,
    DirectionsFrequenciesConfig,
    PeriodConfig,
]


EncodePart: TypeAlias = Union[
    # Section 0
    IndicatorSection,
    # Section 1
    OriginConfig,
    DataTypeConfig,
    ReferenceTimeConfig,
    TablesConfig,
    MarsType,
    IdentTemplateNumber,
    IdentificationSection,
    # Section 2
    LocalUse,
    # Section 3
    GridDefinition,
    # Section 4
    PDT,
    # PDTCategoryPair,  # Moved out of the list here because it is mapped from the configs
    TimeConfig,
    PointInTime,
    TimeRange,
    ParamConfig,
    LevelConfig,
    EnsembleConfig,
    ChemConfig,
    DirectionsFrequenciesConfig,
    PeriodConfig,
    ProductDefinition,
    # Section 5
    DataRepresentation,
]


def getCrumb(typeToSearch, crumbs: [EncodePart]):
    l = list(filter(lambda c: isinstance(c, typeToSearch), crumbs))
    if len(l) == 0:
        return None
    if len(l) > 1:
        raise ValueError(f"Type {typeToSearch} exists more than once: {l}")
    return l[0]


def withDefault(x, default):
    return default if x is None else x


def toArgDict(argName, value):
    return {} if value is None else {argName: value}


def mapPDTCategories(crumbs: List[EncodePart]) -> List[PDTCategoryPair]:
    def mapperGen(crumbs: List[EncodePart]):
        for crumb in crumbs:
            match crumb:
                case PointInTime():
                    yield pdtCatPair("timeExtent", "pointInTime")
                case TimeRange():
                    yield pdtCatPair("timeExtent", "timeRange")
                case ChemConfig():
                    yield pdtCatPair("productCategory", "chemical")
                case EnsembleConfig():
                    yield pdtCatPair("processSubType", "ensemble")
                case PeriodConfig():
                    yield pdtCatPair("productCategory", "wave")
                    yield pdtCatPair("productSubCategory", "periodRange")
                case DirectionsFrequenciesConfig():
                    yield pdtCatPair("productCategory", "wave")
                    yield pdtCatPair("productSubCategory", "spectraList")
                case _:
                    pass

    return list(mapperGen(crumbs))


def buildPDT(crumbs: List[EncodePart]):
    pdt = getCrumb(PDT, crumbs)
    # pdtPairs = list(filter(lambda c: isinstance(c, PDTCategoryPair),  crumbs))
    # if pdt is not None and len(pdtPairs) > 0:
    #    raise ValueError(f"PDT AND partial pdt category pairs are given: {pdt}, pairs: {pdtPairs}")
    if pdt is not None:
        return pdt

    pdtPairs = mapPDTCategories(crumbs)
    pdtDict = pdtCategoryPairsToDict(pdtPairs)
    try:
        pdtNum = inferPDT(pdtDict, pdtMatcher)
        return PDT(templateNumber=pdtNum)
    except Exception as e:
        raise Exception(
            f"Can not infer pdt from dict: {pdtDict} (pairs: {pdtPairs}, crumbs: {crumbs})"
        ) from e


def buildTimeConfig(crumbs: List[EncodePart]):
    timeConfigObj = getCrumb(TimeConfig, crumbs)
    timeCrumbs = list(filter(lambda c: isinstance(c, TimeConfigType), crumbs))
    if timeConfigObj is not None and len(timeCrumbs) > 0:
        raise ValueError(
            f"TimeConfig AND TimeConfigType are given: {timeConfigObj} - {timeCrumbs}"
        )
    if timeConfigObj is not None:
        return timeConfigObj
    if len(timeCrumbs) > 1:
        raise ValueError(f"Too many TimeConfigType are given: {timeCrumbs}")
    if len(timeCrumbs) == 0:
        return timeConfig(PointInTime())
    return timeConfig(timeCrumbs[0])


def buildProductDefiniton(crumbs: List[EncodePart]):
    productDef = getCrumb(ProductDefinition, crumbs)
    pdtCrumbs = list(filter(lambda c: isinstance(c, Section4Part), crumbs))
    if productDef is not None and len(pdtCrumbs) > 0:
        raise ValueError(
            f"Product definition AND partial definitons are given: {productDef}, crumbs: {pdtCrumbs}"
        )
    if productDef is not None:
        return productDef

    pdt = None
    try:
        pdt = buildPDT(pdtCrumbs)
    except Exception as e:
        raise Exception(f"Can not build pdt with crumbs: {pdtCrumbs}") from e

    args = {
        **toArgDict("pdt", pdt),
        **toArgDict("timeConfig", buildTimeConfig(pdtCrumbs)),
        **toArgDict("param", getCrumb(ParamConfig, pdtCrumbs)),
        **toArgDict("level", getCrumb(LevelConfig, pdtCrumbs)),
        **toArgDict("ensemble", getCrumb(EnsembleConfig, pdtCrumbs)),
        **toArgDict("chemical", getCrumb(ChemConfig, pdtCrumbs)),
        **toArgDict(
            "directionsFrequencies", getCrumb(DirectionsFrequenciesConfig, pdtCrumbs)
        ),
        **toArgDict("periodRange", getCrumb(PeriodConfig, pdtCrumbs)),
    }
    try:
        return ProductDefinition(**args)
    except Exception as e:
        raise Exception(f"Can not build ProductDefiniton with args: {args}") from e


def buildIdentification(crumbs: List[EncodePart]):
    identSection = getCrumb(IdentificationSection, crumbs)
    identCrumbs = list(filter(lambda c: isinstance(c, Section1Part), crumbs))
    if identSection is not None and len(identCrumbs) > 0:
        raise ValueError(
            f"Identification section  AND partial definitons are given: {identSection}, crumbs: {identCrumbs}"
        )
    if identSection is not None:
        return identSection

    args = {
        **toArgDict("marsType", getCrumb(MarsType, identCrumbs)),
        **toArgDict("templateNumber", getCrumb(IdentTemplateNumber, identCrumbs)),
        **toArgDict("origin", getCrumb(OriginConfig, identCrumbs)),
        **toArgDict("dataType", getCrumb(DataTypeConfig, identCrumbs)),
        **toArgDict("referenceTime", getCrumb(ReferenceTimeConfig, identCrumbs)),
        **toArgDict("tables", getCrumb(TablesConfig, identCrumbs)),
    }
    try:
        return IdentificationSection(**args)
    except Exception as e:
        raise Exception(f"Can not build IdentificationSection with args: {args}") from e


def buildEncode(crumbs: List[EncodePart]) -> Encode:
    args = None
    try:
        args = {
            **toArgDict("indicator", getCrumb(IndicatorSection, crumbs)),
            **toArgDict("identification", buildIdentification(crumbs)),
            **toArgDict("localUse", getCrumb(LocalUse, crumbs)),
            **toArgDict("grid", getCrumb(GridDefinition, crumbs)),
            **toArgDict("product", buildProductDefiniton(crumbs)),
            **toArgDict("dataRepres", getCrumb(DataRepresentation, crumbs)),
        }
    except Exception as e:
        raise Exception(f"Can not build encode with input: {crumbs}") from e

    try:
        return Encode(**args)
    except Exception as e:
        raise Exception(f"Can not build encode with args: {args}") from e


# Eventually we define partial Rules from which we perform combinations


class PartialRule(BaseModel):
    filters: List[RuleFilter]
    encode: List[EncodePart]
    namePrefix: Optional[str] = None


def partialRule(
    f: List[Union[RuleFilter, RuleFilterType]],
    e: List[EncodePart],
    namePrefix: Optional[str] = None,
) -> PartialRule:
    return PartialRule(
        filters=[ruleFilter(fi) for fi in f], encode=e, namePrefix=namePrefix
    )


def mergePartialRules(partialRules: List[PartialRule]):
    return PartialRule(
        filters=[f for pr in partialRules for f in pr.filters],
        encode=[e for pr in partialRules for e in pr.encode],
        namePrefix="_".join(
            [pr.namePrefix for pr in partialRules if pr.namePrefix is not None]
        )
        or None,
    )


# Declare AlternativeSet - partial rules that are exclusive to each other
AlternativeSet: TypeAlias = List[PartialRule]


def combinePartialRules(alternativeSets: List[AlternativeSet]):
    if len(alternativeSets) == 0:
        return []
    if len(alternativeSets) == 1:
        return alternativeSets[0]

    # Use a reduce/foldl and list comprehension (cartesion product)
    # to perform all products
    def reducer(acc, rhs):
        return (a + [r] for a in acc for r in rhs)

    return reduce(reducer, alternativeSets[1:], [[a] for a in alternativeSets[0]])

def combineAndMergePartialRules(alternativeSets: List[AlternativeSet]):
    return [mergePartialRules(pr) for pr in combinePartialRules(alternativeSets)]


def buildRule(prule: PartialRule):
    encode = buildEncode(prule.encode)
    name = None
    if prule.namePrefix is not None and prule.namePrefix != "":
        name = nameFromEncode(encode, additionalPrefix=prule.namePrefix)

    return EncodeRule(
        tag="grib2",
        filter=ruleFilter(composeAll(prule.filters)),
        encode=encode,
        name=name,
    )
