import re
import os
import itertools
import yaml
from functools import reduce
from enum import Enum
from typing import Union, List, Optional, Annotated, TypeAlias, Dict
from pydantic import BaseModel, Field, ValidationError, validator, AfterValidator, model_validator

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
    return dumper.represent_scalar('tag:yaml.org,2002:str', data.value, style='"')
    
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
    regex = re.compile(r'^\d+:\d+$')
    if regex.match(val):
        # TODO elaborate checking range
        return val;
    raise ValueError(f"Not a valid param range {val}")
    

ParamCheckedType: TypeAlias = Union[Annotated[str, AfterValidator(isParamRange)], int]
ParamBaseType: TypeAlias = Union[str, int]
    
class Param(BaseModel):
    value: ParamCheckedType = Field(union_mode='left_to_right')
    
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
    
    
class HasType(BaseModel):
    type: str

def hasType(t: str) -> HasType:
    return HasType(type=t)
    
class LacksType(BaseModel):
    type: str

def lacksType(t: str) -> LacksType:
    return LacksType(type=t)
    

RuleFilterType: TypeAlias = Union[MatchParam, HasType, LacksType, MatchType, 'ComposeAll']
class RuleFilter(BaseModel):
    filter: RuleFilterType = Field(union_mode='left_to_right')
    
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
    marsType: MarsType # Used to put in name 
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
    type: str = 'default'
    descriptiveName: str = 'instant'
    
class TimeRange(BaseModel):
    type: str
    typeOfStatisticalProcessing: str
    overallLengthOfTimeRange: Optional[str] = None
    encodeStepZero: Optional[bool] = None
    descriptiveName: str


TimeConfigType: TypeAlias = Union[PointInTime, TimeRange]
class TimeConfig(BaseModel):
    config: TimeConfigType = Field(union_mode='left_to_right')
    
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
        raise ValueError(f"The following categories appear more than once: {catMoreThanOnce} - {problematicPairs}")
    
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
def nameFromEncode(encode: Encode):
    level = encode.product.level.type if encode.product.level is not None else None
    wave = encode.product.directionsFrequencies.type if encode.product.directionsFrequencies is not None  else None
    periodRange = encode.product.periodRange.type if encode.product.periodRange is not None  else None
    
    levelWaveStr = "-".join([l for l in [level, wave, periodRange] if l is not None])
    
    grid = encode.grid.shortName 
    marsType = encode.identification.marsType.type
    ensemble = "ensemble" if encode.product.ensemble else "deterministic"
    time = encode.product.timeConfig.config.descriptiveName
    packing = encode.dataRepres.descriptiveName
    paramConfig = encode.product.param.type
    dataset = "" if encode.product.param.datasetForLocal is None else f"-{encode.product.param.datasetForLocal}"
    local = encode.localUse.templateNumber
    
    return f"rule-{levelWaveStr}-{grid}-{marsType}-{ensemble}-{time}-{packing}-{paramConfig}{dataset}-{local}";

class EncodeRule(BaseModel):
    tag: str = "grib2"
    filter: RuleFilter
    encode: Encode
    name: Optional[str] = None
    
    @model_validator(mode='after')
    def set_name(self):
        if self.name is None:
            self.name = nameFromEncode(self.encode)
        return self
    
    
# Emitting...

def toDictRepres(val):
    match val:
        case MatchParam():
            return { "type": "param", "operation": "match", "values": [toDictRepres(v) for v in val.values]}
        case Param():
            if isinstance(val.value, str):
                return quoted(val.value)
            return val.value
        case MatchType():
            return { "type": val.type, "operation": "match", "value": val.value}
        case HasType():
            return { "type": val.type, "operation": "has"}
        case LacksType():
            return { "type": val.type, "operation": "lacks"}
        case RuleFilter():
            return toDictRepres(val.filter)
        case ComposeAll():
            return { "type": "composed", "operation": "all", "filters": [toDictRepres(v) for v in val.filters]}
        case Encode():
            return { "type": val.type
                   , "indicator-section": toDictRepres(val.indicator)
                   , "identification-section": toDictRepres(val.identification)
                   , "identification-section": toDictRepres(val.identification)
                   , "local-use-section": toDictRepres(val.localUse)
                   , "grid-definition-section": toDictRepres(val.grid)
                   , "product-definition-section": toDictRepres(val.product)
                   , "data-representation-section": toDictRepres(val.dataRepres)
                   }
        case IndicatorSection():
            return { "template-number": val.templateNumber }
        case IdentificationSection():
            return { "template-number": val.templateNumber.templateNumber
                   , "origin-configurator": toDictRepres(val.origin) 
                   , "data-type-configurator": toDictRepres(val.dataType) 
                   , "reference-time-configurator": toDictRepres(val.referenceTime) 
                   , "tables-configurator": toDictRepres(val.tables) 
                   }
        case LocalUse():
            return { "template-number": val.templateNumber }
        case GridDefinition():
            return { "template-number": val.templateNumber }
        case ProductDefinition():
            return { "template-number": val.pdt.templateNumber 
                   , **toDictRepres(val.timeConfig)
                   , "param-configurator": toDictRepres(val.param)
                   , **( {"level-configurator": toDictRepres(val.level)} if val.level is not None else {})
                   , **( {"ensemble-configurator": toDictRepres(val.ensemble)} if val.ensemble is not None else {})
                   , **( {"chemical-configurator": toDictRepres(val.chemical)} if val.chemical is not None else {})
                   , **( {"directions-frequencies-configurator": toDictRepres(val.directionsFrequencies)} if val.directionsFrequencies is not None else {})
                   , **( {"period-configurator": toDictRepres(val.periodRange)} if val.periodRange is not None else {})
                   }
        case DataRepresentation():
            return { "template-number": val.templateNumber }
        case OriginConfig():
            return { "type": val.type, "sub-centre": val.subCentre }
        case DataTypeConfig():
            return { "type": val.type }
        case ReferenceTimeConfig():
            return { "type": val.type }
        case TablesConfig():
            return { "type": val.type, "local-tables-version": val.localTablesVersion }
        case TimeConfig():
            match val.config:
                case PointInTime():
                    return {"point-in-time-config": toDictRepres(val.config)}
                case TimeRange():
                    return {"time-statistics-configurator": toDictRepres(val.config)}
        case PointInTime():
            return { "type": val.type }
        case TimeRange():
            return { "type": val.type, "type-of-statistical-processing": val.typeOfStatisticalProcessing, **({} if val.overallLengthOfTimeRange is None else {"overall-length-of-timerange": val.overallLengthOfTimeRange}), **({} if val.encodeStepZero is None else {"encode-step-zero": val.encodeStepZero}) }
        case ParamConfig():
            return { "type": val.type, **({} if val.datasetForLocal is None else { "dataset-for-local": val.datasetForLocal }) }
        case LevelConfig():
            return { "type": val.type }
        case EnsembleConfig():
            return { "type": val.type }
        case ChemConfig():
            return { "type": val.type }
        case DirectionsFrequenciesConfig():
            return { "type": val.type }
        case PeriodConfig():
            return { "type": val.type }
        case EncodeRule():
            return { "tag": val.tag, "name": val.name, "filter": toDictRepres(val.filter), "encode": toDictRepres(val.encode) }
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
    PDTCategoryPair,
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
 PDTCategoryPair,
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
    l = list(filter(lambda c: isinstance(c, typeToSearch),  crumbs))
    if len(l) == 0:
        return None
    if len(l) > 1:
        raise ValueError(f"Type {typeToSearch} exists more than once: {l}")
    return l[0]

def withDefault(x, default):
    return (default if x is None else x)

def toArgDict(argName, value):
    return {} if value is None else {argName: value}
    
    
def buildPDT(crumbs: List[EncodePart]):
    pdt = getCrumb(PDT, crumbs)
    pdtPairs = list(filter(lambda c: isinstance(c, PDTCategoryPair),  crumbs))
    if pdt is not None and len(pdtPairs) > 0:
        raise ValueError(f"PDT AND partial pdt category pairs are given: {pdt}, pairs: {pdtPairs}")
    if pdt is not None:
        return pdt
        
    pdtDict = pdtCategoryPairsToDict(pdtPairs)
    try:
        pdtNum = inferPDT(pdtDict, pdtMatcher)
        return PDT(templateNumber=pdtNum)
    except Exception as e:
        raise Exception(f"Can not infer pdt from dict: {pdtDict} (pairs: {pdtPairs}, crumbs: {crumbs})") from e
        
def buildTimeConfig(crumbs: List[EncodePart]):
    timeConfigObj = getCrumb(TimeConfig, crumbs)
    timeCrumbs = list(filter(lambda c: isinstance(c, TimeConfigType),  crumbs))
    if timeConfigObj is not None and len(timeCrumbs) > 0:
        raise ValueError(f"TimeConfig AND TimeConfigType are given: {timeConfigObj} - {timeCrumbs}")
    if timeConfigObj is not None:
        return timeConfigObj
    if len(timeCrumbs) > 1:
        raise ValueError(f"Too many TimeConfigType are given: {timeCrumbs}")
    if len(timeCrumbs) == 0:
        return timeConfig(PointInTime())
    return timeConfig(timeCrumbs[0])


def buildProductDefiniton(crumbs: List[EncodePart]):
    productDef = getCrumb(ProductDefinition, crumbs)
    pdtCrumbs = list(filter(lambda c: isinstance(c, Section4Part),  crumbs))
    if productDef is not None and len(pdtCrumbs) > 0:
        raise ValueError(f"Product definition AND partial definitons are given: {productDef}, crumbs: {pdtCrumbs}")
    if productDef is not None:
        return productDef
    
    pdt=None
    try:
        pdt=buildPDT(pdtCrumbs)
    except Exception as e:
        raise Exception(f"Can not build pdt with crumbs: {pdtCrumbs}") from e
    
    args = {
        **toArgDict("pdt", pdt),
        **toArgDict("timeConfig",buildTimeConfig(pdtCrumbs)),
        **toArgDict("param",getCrumb(ParamConfig, pdtCrumbs)),
        **toArgDict("level",getCrumb(LevelConfig, pdtCrumbs)),
        **toArgDict("ensemble",getCrumb(EnsembleConfig, pdtCrumbs)),
        **toArgDict("chemical",getCrumb(ChemConfig, pdtCrumbs)),
        **toArgDict("directionsFrequencies",getCrumb(DirectionsFrequenciesConfig, pdtCrumbs)),
        **toArgDict("periodRange",getCrumb(PeriodConfig, pdtCrumbs)),
    }
    try:
        return ProductDefinition(**args)
    except Exception as e:
        raise Exception(f"Can not build ProductDefiniton with args: {args}") from e
    
    
def buildIdentification(crumbs: List[EncodePart]):
    identSection = getCrumb(IdentificationSection, crumbs)
    identCrumbs = list(filter(lambda c: isinstance(c, Section1Part),  crumbs))
    if identSection is not None and len(identCrumbs) > 0:
        raise ValueError(f"Identification section  AND partial definitons are given: {identSection}, crumbs: {identCrumbs}")
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
    args=None
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

def partialRule(f: List[Union[RuleFilter, RuleFilterType]], e: List[EncodePart], namePrefix: Optional[str] = None) -> PartialRule:
    return PartialRule(filters=[ruleFilter(fi) for fi in f], encode=e,namePrefix=namePrefix)

def mergePartialRules(partialRules: List[PartialRule]):
    return PartialRule(
        filters=[f for pr in partialRules for f in pr.filters],
        encode=[e for pr in partialRules for e in pr.encode],
        namePrefix = "_".join([pr.namePrefix for pr in partialRules if pr.namePrefix is not None]) or None,
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
        return (a+[r] for a in acc for r in rhs)
    return reduce(reducer, alternativeSets[1:], [[a] for a in alternativeSets[0]])
    
def buildRule(prule: PartialRule):
    encode=buildEncode(prule.encode)
    name=None
    if prule.namePrefix is not None and prule.namePrefix != "":
        name=f"{prule.namePrefix}-{nameFromEncode(encode)}"

    return EncodeRule(tag="grib2", filter=ruleFilter(composeAll(prule.filters)), encode=encode, name=name)
    


TYPES = [
    partialRule([matchType("type", "forecast")], [marsType("fc"), IdentTemplateNumber(templateNumber=0)])
]

GRIDS = [
    partialRule([matchType("repres", "gaussian-grid")], [gridDefinition(40, "gg")])
]

LOCALSECTION = [
    partialRule([lacksType("anoffset")], [localUse(1)]),
    partialRule([hasType("anoffset")], [localUse(36)])
]

PROCESSTYPES = [
    partialRule([lacksType("number")], [pdtCatPair("processType", None)]),
    partialRule([hasType("number")], [pdtCatPair("processType", "ensemble"), EnsembleConfig()])
]

PARAM_LEVTYPE = [
    # cloudbase
    partialRule([matchType("levtype", "sfc"), matchParam([228023])], [pdtCatPair("timeExtent", "pointInTime"), levelConfig("cloudbase")]),
    # entireAtmosphere
    partialRule([matchType("levtype", "sfc"), matchParam([
        59, '78:79', '136:137', 164, 206, '162059:162063', '162071:162072',
        162093, 228044, 228050, 228052, '228088:228090', 228164, 260132
      ])], [pdtCatPair("timeExtent", "pointInTime"), levelConfig("entireAtmosphere")]),
    # entireLake
    partialRule([matchType("levtype", "sfc"), matchParam([228007, 228011])], [pdtCatPair("timeExtent", "pointInTime"), levelConfig("entireLake")]),
    
    # heightAboveGround - statistical
    partialRule([matchType("levtype", "sfc"), matchParam([123])], [pdtCatPair("timeExtent", "timeRange"), levelConfig("heightAboveGround"), paramConfig("paramIdECMF"),
        TimeRange(type="fixed-timerange", typeOfStatisticalProcessing="max", overallLengthOfTimeRange="6h", descriptiveName="max-over-last-6h") ]),
    partialRule([matchType("levtype", "sfc"), matchParam([121])], [pdtCatPair("timeExtent", "timeRange"), levelConfig("heightAboveGround"), paramConfig("paramId"),
        TimeRange(type="fixed-timerange", typeOfStatisticalProcessing="max", overallLengthOfTimeRange="6h", descriptiveName="max-over-last-6h") ]),
    partialRule([matchType("levtype", "sfc"), matchParam([49, 201])], [pdtCatPair("timeExtent", "timeRange"), levelConfig("heightAboveGround"), paramConfig("paramId"),
        TimeRange(type="since-last-post-processing-step", typeOfStatisticalProcessing="max", descriptiveName="max-since-last-pp") ]),
    partialRule([matchType("levtype", "sfc"), matchParam([122])], [pdtCatPair("timeExtent", "timeRange"), levelConfig("heightAboveGround"), paramConfig("paramId"),
        TimeRange(type="fixed-timerange", typeOfStatisticalProcessing="min", overallLengthOfTimeRange="6h", descriptiveName="min-over-last-6h") ]),
    partialRule([matchType("levtype", "sfc"), matchParam([202])], [pdtCatPair("timeExtent", "timeRange"), levelConfig("heightAboveGround"), paramConfig("paramId"),
        TimeRange(type="since-last-post-processing-step", typeOfStatisticalProcessing="min", descriptiveName="min-since-last-pp") ]),
        
    # heightAboveGround - point in time
    partialRule([matchType("levtype", "sfc"), matchParam([
        '165:168', 207, 174096, 228029, 228037, '228131:228132'
      ])], [pdtCatPair("timeExtent", "pointInTime"), levelConfig("heightAboveGround")]),
      
    # heightAboveSea - point in time
    partialRule([matchType("levtype", "sfc"), matchParam([
        140245, 140249, 140233
      ])], [pdtCatPair("timeExtent", "pointInTime"), levelConfig("heightAboveSea")]),
      
    # highCloudLayer - point in time
    partialRule([matchType("levtype", "sfc"), matchParam([3075])], [pdtCatPair("timeExtent", "pointInTime"), levelConfig("highCloudLayer")]),
    # mediumCloudLayer - point in time
    partialRule([matchType("levtype", "sfc"), matchParam([3074])], [pdtCatPair("timeExtent", "pointInTime"), levelConfig("mediumCloudLayer")]),
    # lowCloudLayer - point in time
    partialRule([matchType("levtype", "sfc"), matchParam([3074])], [pdtCatPair("timeExtent", "pointInTime"), levelConfig("lowCloudLayer")]),
    
    # iceLayerOnWater - point in time
    partialRule([matchType("levtype", "sfc"), matchParam([228014])], [pdtCatPair("timeExtent", "pointInTime"), levelConfig("iceLayerOnWater")]),
    
    # iceTopOnWater - point in time
    partialRule([matchType("levtype", "sfc"), matchParam([228013])], [pdtCatPair("timeExtent", "pointInTime"), levelConfig("iceTopOnWater")]),
    
    # lakeBottom - point in time
    partialRule([matchType("levtype", "sfc"), matchParam([228010])], [pdtCatPair("timeExtent", "pointInTime"), levelConfig("lakeBottom")]),
    
    # meanSea - point in time
    partialRule([matchType("levtype", "sfc"), matchParam([151])], [pdtCatPair("timeExtent", "pointInTime"), levelConfig("meanSea")]),
    
    # mixedLayerParcel - point in time
    partialRule([matchType("levtype", "sfc"), matchParam(['228231:228234'])], [pdtCatPair("timeExtent", "pointInTime"), levelConfig("mixedLayerParcel")]),
    
    # mixingLayer - point in time
    partialRule([matchType("levtype", "sfc"), matchParam(['228008:228009'])], [pdtCatPair("timeExtent", "pointInTime"), levelConfig("mixingLayer")]),
    
    # mostUnstableParcel - point in time
    partialRule([matchType("levtype", "sfc"), matchParam(['228235:228237'])], [pdtCatPair("timeExtent", "pointInTime"), levelConfig("mostUnstableParcel")]),
    
    # nominalTop - point in time
    partialRule([matchType("levtype", "sfc"), matchParam(['178:179', '208:209', 212])], [pdtCatPair("timeExtent", "pointInTime"), levelConfig("nominalTop")]),
    
    # tropopause - point in time
    partialRule([matchType("levtype", "sfc"), matchParam([228045])], [pdtCatPair("timeExtent", "pointInTime"), levelConfig("tropopause")]),
    
      
    # surface - since beginning - chem - era6
    partialRule([matchType("levtype", "sfc"), matchParam(['228080:228082', '233032:233035', '235062:235064'])], [pdtCatPair("timeExtent", "timeRange"), pdtCatPair("productCategory", "chemical"), levelConfig("surface"), paramConfig("paramId", "era6"), ChemConfig(),
        TimeRange(type="since-beginning-of-forecast", typeOfStatisticalProcessing="accumul", descriptiveName="since-beginning") ]),
        
    # surface - since beginning  
    partialRule([matchType("levtype", "sfc"), matchParam([
        8, 9, 20, 44, 45, 47, 50, 57, 58, '142:147', 169, '175:177', '180:182', 189,
        '195:197', 205, '210:211', 213, 228, 239, 240, 3062, 3099, '162100:162113',
        '222001:222256', 228021, 228022, 228129, 228130, 228143, 228144,
        228216, 228228, 228251, 231001, 231002, 231003, 231005, 231010, 231012, 231057, 231058,
        '233000:233031', 260259
      ])], [pdtCatPair("timeExtent", "timeRange"), levelConfig("surface"), paramConfig("paramId"), 
        TimeRange(type="since-beginning-of-forecast", typeOfStatisticalProcessing="accumul", encodeStepZero=True, descriptiveName="since-beginning") ]),
        
    # surface - average over last 1h
    partialRule([matchType("levtype", "sfc"), matchParam([228051, 228053])], [pdtCatPair("timeExtent", "timeRange"), levelConfig("surface"), paramConfig("paramId"), 
        TimeRange(type="fixed-timerange", typeOfStatisticalProcessing="average", overallLengthOfTimeRange="1h", descriptiveName="average-over-last-1h") ]),
        
    # surface - average over last 3h
    partialRule([matchType("levtype", "sfc"), matchParam([228057, 228059])], [pdtCatPair("timeExtent", "timeRange"), levelConfig("surface"), paramConfig("paramId"), 
        TimeRange(type="fixed-timerange", typeOfStatisticalProcessing="average", overallLengthOfTimeRange="3h", descriptiveName="average-over-last-3h") ]),
        
    # surface - average over last 6h
    partialRule([matchType("levtype", "sfc"), matchParam([228058, 228060])], [pdtCatPair("timeExtent", "timeRange"), levelConfig("surface"), paramConfig("paramId"), 
        TimeRange(type="fixed-timerange", typeOfStatisticalProcessing="average", overallLengthOfTimeRange="6h", descriptiveName="average-over-last-6h") ]),
        
        
    # surface - max over last 3h - paramIdECMF
    partialRule([matchType("levtype", "sfc"), matchParam([228026])], [pdtCatPair("timeExtent", "timeRange"), levelConfig("surface"), paramConfig("paramIdECMF"), 
        TimeRange(type="fixed-timerange", typeOfStatisticalProcessing="max", overallLengthOfTimeRange="3h", descriptiveName="max-over-last-3h") ]),
        
    # surface - max over last 3h
    partialRule([matchType("levtype", "sfc"), matchParam([228028])], [pdtCatPair("timeExtent", "timeRange"), levelConfig("surface"), paramConfig("paramId"), 
        TimeRange(type="fixed-timerange", typeOfStatisticalProcessing="max", overallLengthOfTimeRange="3h", descriptiveName="max-over-last-3h") ]),
        
    # surface - max over last 6h
    partialRule([matchType("levtype", "sfc"), matchParam([228224, 228035, 228036])], [pdtCatPair("timeExtent", "timeRange"), levelConfig("surface"), paramConfig("paramId"), 
        TimeRange(type="fixed-timerange", typeOfStatisticalProcessing="max", overallLengthOfTimeRange="6h", descriptiveName="max-over-last-6h") ]),
        
    # surface - min over last 3h - paramIdECMF
    partialRule([matchType("levtype", "sfc"), matchParam([228027])], [pdtCatPair("timeExtent", "timeRange"), levelConfig("surface"), paramConfig("paramIdECMF"), 
        TimeRange(type="fixed-timerange", typeOfStatisticalProcessing="min", overallLengthOfTimeRange="3h", descriptiveName="min-over-last-3h") ]),
        
    # TODO - paramID is duplacted here ?
    # surface - min over last 3h
    partialRule([matchType("levtype", "sfc"), matchParam([228027])], [pdtCatPair("timeExtent", "timeRange"), levelConfig("surface"), paramConfig("paramId"), 
        TimeRange(type="fixed-timerange", typeOfStatisticalProcessing="min", overallLengthOfTimeRange="3h", descriptiveName="min-over-last-3h") ]),


    # surface - mode over last 1h
    partialRule([matchType("levtype", "sfc"), matchParam([260320])], [pdtCatPair("timeExtent", "timeRange"), levelConfig("surface"), paramConfig("paramId"), 
        TimeRange(type="fixed-timerange", typeOfStatisticalProcessing="mode", overallLengthOfTimeRange="1h", descriptiveName="mode-over-last-1h") ]),
        
    # surface - mode over last 3h
    partialRule([matchType("levtype", "sfc"), matchParam([260321])], [pdtCatPair("timeExtent", "timeRange"), levelConfig("surface"), paramConfig("paramId"), 
        TimeRange(type="fixed-timerange", typeOfStatisticalProcessing="mode", overallLengthOfTimeRange="3h", descriptiveName="mode-over-last-3h") ]),
        
    # surface - mode over last 6h
    partialRule([matchType("levtype", "sfc"), matchParam([260339])], [pdtCatPair("timeExtent", "timeRange"), levelConfig("surface"), paramConfig("paramId"), 
        TimeRange(type="fixed-timerange", typeOfStatisticalProcessing="mode", overallLengthOfTimeRange="6h", descriptiveName="mode-over-last-6h") ]),


    # surface - severity over last 1h
    partialRule([matchType("levtype", "sfc"), matchParam([260318])], [pdtCatPair("timeExtent", "timeRange"), levelConfig("surface"), paramConfig("paramId"), 
        TimeRange(type="fixed-timerange", typeOfStatisticalProcessing="severity", overallLengthOfTimeRange="1h", descriptiveName="severity-over-last-1h") ]),
        
    # surface - severity over last 3h
    partialRule([matchType("levtype", "sfc"), matchParam([260319])], [pdtCatPair("timeExtent", "timeRange"), levelConfig("surface"), paramConfig("paramId"), 
        TimeRange(type="fixed-timerange", typeOfStatisticalProcessing="severity", overallLengthOfTimeRange="3h", descriptiveName="severity-over-last-3h") ]),
        
    # surface - severity over last 6h
    partialRule([matchType("levtype", "sfc"), matchParam([260338])], [pdtCatPair("timeExtent", "timeRange"), levelConfig("surface"), paramConfig("paramId"), 
        TimeRange(type="fixed-timerange", typeOfStatisticalProcessing="severity", overallLengthOfTimeRange="6h", descriptiveName="severity-over-last-6h") ]),
        
        
    # surface - instant
    partialRule([matchType("levtype", "sfc"), matchParam([
        '26:32', 33, '34:43', '66:67', 74,
        129, 134, 139, 141, 148, 159, '160:163',
        170, '172:174', '186:188', 198, '229:232',
        '234:236', 238, '243:245', 3020, 3073, 3074, 3075,
        160198, 210200, 210201, 210202, 228003, 228012, 210262,
        210263, 210264, '228015:228020', 228024, 228032, '228046:228048',
        228141, '228217:228221',
        228227, 228239, 228240, 228241, '228246:228247',
        260015, 260048, 260109, 260121, 260123, 262139, 262140,
        260289, 260509])], [pdtCatPair("timeExtent", "pointInTime"),levelConfig("surface"), paramConfig("paramId")]),
        
    # surface - instant - chem 
    partialRule([matchType("levtype", "sfc"), matchParam(['228083:228085'])], [pdtCatPair("timeExtent", "pointInTime"), pdtCatPair("productCategory", "chemical"), levelConfig("surface"), paramConfig("paramId"), ChemConfig()], namePrefix="chem", ),
    
    # surface - instant - wam_int
    partialRule([matchType("levtype", "sfc"), matchParam([
        '140098:140105', '140112:140113', '140121:140129', '140207:140209',
        '140211:140212', '140214:140232', '140234:140239', 140244,
        '140252:140254'])], [pdtCatPair("timeExtent", "pointInTime"), levelConfig("surface"), paramConfig("paramId")], namePrefix="wam_int"),
        
    # surface - instant - wam_period
    partialRule([matchType("levtype", "sfc"), matchParam(['140114:140120'])], [pdtCatPair("timeExtent", "pointInTime"), levelConfig("surface"), pdtCatPair("productCategory", "wave"), pdtCatPair("productSubCategory", "periodRange"), paramConfig("paramId"), PeriodConfig()], namePrefix="wam_period"),
    
    # surface - instant - wam_spectra
    partialRule([matchType("levtype", "sfc"), matchParam([140251])], [pdtCatPair("timeExtent", "pointInTime"), levelConfig("surface"), pdtCatPair("productCategory", "wave"), pdtCatPair("productSubCategory", "spectraList"), paramConfig("paramId"), DirectionsFrequenciesConfig()], namePrefix="wam_spectra"),
    
    # surface - max since last pp
    partialRule([matchType("levtype", "sfc"), matchParam([228226])], [pdtCatPair("timeExtent", "timeRange"), levelConfig("surface"), paramConfig("paramId"),
                TimeRange(type="since-last-post-processing-step", typeOfStatisticalProcessing="max", descriptiveName="max-since-last-pp")], namePrefix="max_since_last_pp"),
        
        
    # sfc wam spec instant
    partialRule([matchType("levtype", "sfc"), matchParam([140251])], [pdtCatPair("timeExtent", "pointInTime"), pdtCatPair("productCategory", "wave"), pdtCatPair("productSubCategory", "spectraList"), paramConfig("paramId"), DirectionsFrequenciesConfig() ]),
]

PACKING = [
    partialRule([matchType("packing", "simple")], [DataRepresentation(templateNumber=0, descriptiveName="simple")]),
    partialRule([matchType("packing", "ccsds")], [DataRepresentation(templateNumber=42, descriptiveName="ccsds")])
]


rules=list(combinePartialRules([TYPES, GRIDS, LOCALSECTION, PROCESSTYPES, PARAM_LEVTYPE, PACKING]))

encodedRules=[buildRule(mergePartialRules(rl)) for rl in rules]

encodedRulesDict= {e.name: [] for e in encodedRules}
for rule in encodedRules:
    encodedRulesDict[rule.name].append(rule)
    
duplicatedRules={key: val for (key,val) in encodedRulesDict.items() if len(val) > 1}
    

if len(duplicatedRules) > 0:
    raise ValueError(f"Not all rule names are unique")


# BASE_DIR="../knowledge/49r2v9/"

BASE_DIR="test_output"

if not os.path.exists(BASE_DIR):
    os.makedirs(BASE_DIR)

for rule in encodedRules:
    fname = f"{BASE_DIR}/{rule.name}.yaml"
    with open(fname, 'w') as fileOut:
        fileOut.write(toYAML(toDictRepres(rule)))




def main():
    pass
    # if len(sys.argv) < 2:
    #     print("Usage: python script.py <directory>")
    #     sys.exit(1)

    # directory = sys.argv[1]
    # if not os.path.isdir(directory):
    #     print("Invalid directory.")
    #     sys.exit(1)

    # for file_name in os.listdir(directory):
    #     if file_name.startswith('rule-') and file_name.endswith('.yaml'):
    #         file_path = os.path.join(directory, file_name)
    #         process_yaml_file(file_path)

if __name__ == "__main__":
    main()

