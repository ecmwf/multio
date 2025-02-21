import os
import glob
import yaml
from enum import Enum
from typing import Union, List, Optional, Annotated, TypeAlias, Dict, Tuple
from pydantic import (
    BaseModel,
    Field,
    ValidationError,
    validator,
    AfterValidator,
    model_validator,
)


class YAMLRule(BaseModel):
    fname: str
    rule: Dict
    
RuleList: TypeAlias = List[YAMLRule]

class ComparedRuleLists(BaseModel):
    unmatched: Tuple[RuleList, RuleList] = ([], [])
    matchedFilters: List[Tuple[YAMLRule, YAMLRule, List]] = []
    matchAndCompareEqual: List[Tuple[YAMLRule, YAMLRule, List]] = []

FILTER_MAP={}

ANOFFSET_HAS={'type': 'anoffset', 'operation': 'has'}
ANOFFSET_HAS_ALT={'type': 'composed',
    'operation': 'any',
    'filters': [{'type': 'stream',
      'operation': 'match',
      'values': ['lwda', 'lwwv', 'elda', 'ewla']},
     {'type': 'composed',
      'operation': 'all',
      'filters': [{'type': 'stream',
        'operation': 'match',
        'values': ['efas', 'wfas']},
       {'type': 'type',
        'operation': 'match',
        'values': ['sfo', 'fu', 'go']}]}]}
FILTER_MAP[yaml.dump(ANOFFSET_HAS_ALT)] = ANOFFSET_HAS



IS_ENSEMBLE={'type': 'number', 'operation': 'has'}
IS_ENSEMBLE_ALT={'type': 'is-ensemble'}
FILTER_MAP[yaml.dump(IS_ENSEMBLE_ALT)] = IS_ENSEMBLE

IS_NO_ENSEMBLE={'type': 'number', 'operation': 'lacks'}
IS_NO_ENSEMBLE_ALT={'type': 'is-ensemble', 'negate': True}
FILTER_MAP[yaml.dump(IS_NO_ENSEMBLE_ALT)] = IS_NO_ENSEMBLE

IS_CHEMICAL={'type': 'composed', 'operation': 'all', 'filters': [{'type': 'chem', 'operation': 'has'}, {'type': 'wavelength', 'operation': 'lacks'}, {'type': 'chem', 'operation': 'lower-than', 'treshold': 900}]}
IS_CHEMICAL_ALT={'type': 'is-chemical'}
FILTER_MAP[yaml.dump(IS_CHEMICAL_ALT)] = IS_CHEMICAL

IS_AEROSOL={'type': 'composed', 'operation': 'all', 'filters': [{'type': 'chem', 'operation': 'has'}, {'type': 'wavelength', 'operation': 'lacks'}, {'type': 'chem', 'operation': 'greater-equal', 'treshold': 900}]}
IS_AEROSOL_ALT={'type': 'is-aerosol'}
FILTER_MAP[yaml.dump(IS_AEROSOL_ALT)] = IS_AEROSOL

IS_OPTICAL={'type': 'composed', 'operation': 'all', 'filters': [{'type': 'chem', 'operation': 'lacks'}, {'type': 'wavelength', 'operation': 'has'}]}
IS_OPTICAL_ALT={'type': 'is-optical'}
FILTER_MAP[yaml.dump(IS_OPTICAL_ALT)] = IS_OPTICAL

IS_CHEMICAL_OPTICAL={'type': 'composed', 'operation': 'all', 'filters': [{'type': 'chem', 'operation': 'has'}, {'type': 'wavelength', 'operation': 'has'}]}
IS_CHEMICAL_OPTICAL_ALT={'type': 'is-chemical-optical'}
FILTER_MAP[yaml.dump(IS_CHEMICAL_OPTICAL_ALT)] = IS_CHEMICAL_OPTICAL

# def compareFilter(lhs, rhs):
#     if lhs == rhs:
#         return True
    
#     def mapFilterMaybe(filter):
#         filtDump = yaml.dump(filter)
#         return yaml.dump(FILTER_MAP[filtDump] if filtDump in FILTER_MAP.keys() else filter)
    
#     return  mapFilterMaybe(lhs) == mapFilterMaybe(rhs)

def singleFilterToId(filter):
    filtDump = yaml.dump(filter)
    return yaml.dump(FILTER_MAP[filtDump] if filtDump in FILTER_MAP.keys() else filter)
    
def filterListToId(filterList):
    return yaml.dump(sorted([singleFilterToId(f) for f in filterList]))
    
def filterToId(filter):
    if filter.get("type") == "composed" and "filters" in filter.keys():
        if not isinstance(filter["filters"], List):
            raise ValueError(f"Expected a list for \"filters\": {filter}")
        return filterListToId(filter["filters"])
    else:
        return singleFilterToId(filter)
    
# def compareFilterLists(llhs, lrhs):
#     if len(llhs) != len(lrhs):
#         return False
        
#     def mapFilter(filter):
#         filtDump = yaml.dump(filter)
#         return yaml.dump(FILTER_MAP[filtDump] if filtDump in FILTER_MAP.keys() else filter)
        
#     return sorted([mapFilter(f) for f in llhs]) == sorted([mapFilter(f) for f in lrhs])
    
    
def diffDict(d1, d2, level='root'):
    if isinstance(d1, dict) and isinstance(d2, dict):
        if d1.keys() != d2.keys():
            s1 = set(d1.keys())
            s2 = set(d2.keys())
            yield (f'{level:<20}', f'{s1-s2} - {s2-s1}')
            common_keys = s1 & s2
        else:
            common_keys = set(d1.keys())

        for k in common_keys:
            for y in diffDict(d1[k], d2[k], level='{}.{}'.format(level, k)):
                yield y

    elif isinstance(d1, list) and isinstance(d2, list):
        if len(d1) != len(d2):
            yield (f'{level:<20}', f'len1={len(d1)}; len2={len(d2)}')
        common_len = min(len(d1), len(d2))

        for i in range(common_len):
            for y in diffDict(d1[i], d2[i], level='{}[{}]'.format(level, i)):
                yield y

    else:
        if d1 != d2:
            yield (f'{level:<20}', f'{d1} != {d2}')
            
    
def diffRuleEncoder(d1, d2):
    return list(diffDict(d1.rule["encoder"], d2.rule["encoder"]))

def compareRules(lhs: RuleList, rhs: RuleList) -> ComparedRuleLists:
    res = ComparedRuleLists()
    
    lhsDict = {filterToId(rule.rule["filter"]): rule for rule in lhs}
    rhsDict = {filterToId(rule.rule["filter"]): rule for rule in rhs}
    
    sameKeys = set(lhsDict.keys()).intersection(set(rhsDict.keys()))
    unmatchedLhsKeys = set(lhsDict.keys()).difference(set(rhsDict.keys()))
    unmatchedRhsKeys = set(rhsDict.keys()).difference(set(lhsDict.keys()))
    
    res.unmatched[0].extend([lhsDict[k] for k in unmatchedLhsKeys])
    res.unmatched[1].extend([rhsDict[k] for k in unmatchedRhsKeys])
    
    for k in sameKeys:
        if "encoder" not in lhsDict[k].rule.keys():
            raise ValueError(f"No key \"encoder\" in lhs dict {lhsDict[k]}")
        if "encoder" not in rhsDict[k].rule.keys():
            raise ValueError(f"No key \"encoder\" in rhs dict {rhsDict[k]}")
        
        if lhsDict[k].rule["encoder"] == rhsDict[k].rule["encoder"]:
            res.matchAndCompareEqual.append((lhsDict[k], rhsDict[k], diffRuleEncoder(lhsDict[k], rhsDict[k])))
        else:
            res.matchedFilters.append((lhsDict[k], rhsDict[k], diffRuleEncoder(lhsDict[k], rhsDict[k])))
    
    return res
        
        


def loadYAMLAsRule(basePath):
    files = glob.glob(basePath + "/**/rule-*.yaml", recursive=True)
    rules=[]
    for fname in files:
        with open(fname, 'r') as f:
            rules.append(YAMLRule(fname=fname, rule=yaml.load(f, yaml.SafeLoader)))
    return rules
        


def compare(path1=None, path2=None):
    P1 = "../knowledge/49r2v9/encodings/" if path1 is None else path1
    P2 = "./test_output/" if path2 is None else path2
    
    rule1 = loadYAMLAsRule(P1)
    rule2 = loadYAMLAsRule(P2)
    
    return compareRules(rule1, rule2)

def main():
    pass




if __name__ == "__main__":
    main()
    
P1 = "../knowledge/49r2v9/encodings/" 
P2 = "./test_output/" 

rule1 = loadYAMLAsRule(P1)
rule2 = loadYAMLAsRule(P2)
