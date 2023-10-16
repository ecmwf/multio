import functools
from PDT import categories, categorySelectorsWithMappedPdt, categoriesWithAllPossibleValues, pdt, keyMappings, keyTypes, allKeyTypes, camelToPascalCase, listAllCategoriesInOrder

def generateCPPEnumForCategory(categories, categoryName, setOfValues):
    """
    Produce string with valid CPP code representing a enum class for a given category.
    """
    values = []
    default = None if (None in setOfValues) or (categoryName not in categories.name()) else defaultCategoryValue(categories[categoryName])
    values.append("    {} = 0,".format(default if default is None else camelToPascalCase(default)))
    for v in setOfValues.difference(set([default])):
        values.append("    {},".format(v if v is None else camelToPascalCase(v)))


    return """enum class {enumName}: unsigned int
{{
{values}
}};\n
""".format(enumName=camelToPascalCase(categoryName), values="\n".join(values))



def generateCPPEnumParserForCategory(categories, categoryName, setOfValues):
    """
    Produce string with valid CPP code for a function that parses a string to enum value for a given category.
    """
    enumName = camelToPascalCase(categoryName)
    values = []
    default = None if (None in setOfValues) or (categoryName not in categories.name()) else defaultCategoryValue(categories[categoryName])


    for v in setOfValues:
        values.append("""{{"{stringVal}", {enumName}::{valName}}}""".format(enumName=enumName, valName=v if v is None else camelToPascalCase(v), stringVal=v))

    errString = "Value for {catName} does not match on of the following [{vals}]".format(catName=categoryName, vals=", ".join(["{}".format(v) for v in setOfValues]))


    return """{enumName} parse{enumName}(std::optional<std::string> val) const {{
    static const std::unordered_map<std::string, {enumName}> map{{{{{values}}}}};

    if(!val) {{
        return {enumName}::{defaultVal};
    }}

    auto it = map.find(*val);
    if (it != map.end()) {{
        return it->second;
    }}
    throw message::MetadataException{{std::string("parse{enumName}(") + *val + std::string("): {errString}")}};
}}
""".format(enumName=enumName, defaultVal=default, values=", ".join(values), errString=errString)



def generateCPPKeyTypeEnum(allKeyTypes):
    """
    Produce string with valid CPP code representing a enum class representing possible types of values key can map to.
    """

    values = []
    for v in allKeyTypes:
        values.append("    {},".format(v))

    return """enum class KeyTypes: unsigned int
{{
{values}
}};\n
""".format(values="\n".join(values))



def generateCPP(categories, categorySelectorsWithMappedPdt, categoriesWithAllPossibleValues, pdt, keyMappings, keyTypes, allKeyTypes):
    """
    Produce a string with valid CPP code that produces a function that can infer a PDT.
    """
    enums="\n\n\n".join([generateCPPEnumForCategory(categories, k, v) for (k,v) in categoriesWithAllPossibleValues.items()])
    parsers="\n\n\n".join([generateCPPEnumParserForCategory(categories, k, v) for (k,v) in categoriesWithAllPossibleValues.items()])

    categoryHandleOrder = listAllCategoriesInOrder(categories)

    def buildDecisionMapTypeString(cats):
        if len(cats) == 0:
            return "std::int64_t"
        else:
            return "std::unordered_map<{enumName}, {sub}>".format(enumName=camelToPascalCase(cats[0]), sub=buildDecisionMapTypeString(cats[1:]))

    keyTypeLookupMapTypesString = """
using KeyList = std::vector<Key>;
struct KeyInfo {
    std::set<KeyTypes> types;
    std::optional<KeyList> alternativeKeys;
};
using KeyInfoMap = std::unordered_map<Key, KeyInfo>;
using PdtKeyMap = std::unordered_map<std::int64_t, KeyList>;
"""
    decisionMapTypeString = "using DecisionMap = {};".format(buildDecisionMapTypeString(categoryHandleOrder))

    def checkKey(k):
        if k not in keyTypes.keys():
            raise ValueError("Key {} is is not listed in keyTypes list".format(k))
        return k


    def buildKeyTypeMapStr():
        def buildTypes(t):
            if isinstance(t, list):
                return "{{{}}}".format(", ".join(("KeyTypes::{}".format(ti) for ti in t)))

            else:
                return "{{KeyTypes::{}}}".format(t)


        def buildAltKeys(k):
            checkKey(k)
            if k in keyMappings.keys():
                keyMappings[k].copy();
                return "{{{{{}}}}}".format(", ".join(("\"{}\"".format(checkKey(ki)) for ki in keyMappings[k])))
            else:
                return "{{}}"
        return "{{{}}}".format(", ".join("{{ \"{key}\", {{ {types}, {altKeys} }} }}".format(key=k,types=buildTypes(v), altKeys=buildAltKeys(k)) for (k, v) in keyTypes.items()))

    def buildPdtKeyMapStr():
        keyMappingsRevMap = {}
        for (k, v) in keyMappings.items():
            for vi in v:
                if vi in keyMappingsRevMap.keys():
                    raise ValueError("Key {} is mapped by multiple key mappings: {} and {}".format(vi, k, keyMappingsRevMap[vi]))
                checkKey(vi)
                keyMappingsRevMap[vi] = k

        def buildKeyList(pdt, keys):
            ret = []
            for k in keys:
                checkKey(k)
                kToAdd = keyMappingsRevMap[k] if k in keyMappingsRevMap.keys() else k
                if kToAdd not in ret:
                    ret.append(kToAdd)

            return "{{{}}}".format(", ".join(("\"{}\"".format(k) for k in ret)));

        return "{{{}}}".format(", ".join(("{{ {key}, {keyList} }}".format(key=k,keyList=buildKeyList(k,v["keySet"])) for (k, v) in pdt.items())))


    def buildDecisionMapString(subCatList, selector, mappedSelectorList):
        if len(subCatList) == 0:
            if len(mappedSelectorList) == 1:
                return "{}".format(mappedSelectorList[0][3][0])
            else:
                # Should not occur, as already checked
                raise ValueError("buildDecisionMapString: selector {} matches multiple or no pdts: {}".format(selector, mappedSelectorList))

        else:
            cat = subCatList[0]
            enumName = camelToPascalCase(cat)
            allVals = categoriesWithAllPossibleValues[cat]

            valsInSelectors = set([])
            for selTuple in mappedSelectorList:
                sel = selTuple[0]
                valsInSelectors.add(sel[cat] if cat in sel.keys() else None)

            cases = []
            for val in valsInSelectors:
                def filterSelector(sel):
                    if val is None:
                        if cat not in sel.keys():
                            return True
                        else:
                            return (sel[cat] is None)
                    else:
                        if cat not in sel.keys():
                            return False
                        else:
                            return (sel[cat] == val)

                # Recursion on a proper selection
                selectorsForVal = [s for s in mappedSelectorList if filterSelector(s[0])]
                sub = buildDecisionMapString(subCatList[1:], {cat: val, **selector}, selectorsForVal)

                caseStr = "{{{enumName}::{enumVal}, {sub}}}".format(enumName=enumName, enumVal= val if val is None else camelToPascalCase(val), sub=sub)
                cases.append(caseStr)

            return "{{{}}}".format(", ".join(cases))

    def buildParseMapString(subCatList, prevCats, mapName="map"):
        if len(subCatList) == 0:
            return """    return {cat}It->second;""".format(cat=prevCats[-1])
        else:
            cat = subCatList[0];

            ossCats = """        oss << ", ";\n""".join(["""        oss << "{cat}: " << ({cat}Str ? std::string(*{cat}Str) : std::string("None"));\n""".format(cat=cat) for cat in (prevCats + [cat])])

            return """
    auto {cat}Str = metadata.getOpt<std::string>("{cat}");
    auto {cat} = parse{enumName}({cat}Str);

    auto {cat}It = {mapName}.find({cat});

    if ({cat}It == {mapName}.end()) {{
        std::ostringstream oss;
        oss << "No productDefinitionTemplate can be mapped for ";
{ossCats}

        throw message::MetadataException{{oss.str()}};
    }}

{nextEval}""".format(enumName=camelToPascalCase(cat), cat=cat, mapName=mapName, ossCats=ossCats, nextEval=buildParseMapString(subCatList[1:], prevCats + [cat], mapName="{}It->second".format(cat)))

    return """
#pragma once


//
//                           !!!!!!!!
//                         !!        !!
//                       !!            !!
//                       !!            !!
//                     !!    !!!!!!!!    !!
//                   !!    !!!!!!!!!!!!    !!
//                   !!    !!!!!!!!!!!!    !!
//                 !!      !!!!!!!!!!!!      !!
//               !!          !!!!!!!!          !!
//               !!          !!!!!!!!          !!
//             !!            !!!!!!!!            !!
//           !!                !!!!                !!
//           !!                !!!!                !!
//         !!                                        !!
//       !!                    !!!!                    !!
//       !!                  !!!!!!!!                  !!
//     !!                  !!!!!!!!!!!!                  !!
//   !!                    !!!!!!!!!!!!                    !!
//   !!                      !!!!!!!!                      !!
//   !!                        !!!!                        !!
//     !!                                                !!
//       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// !!! Do not modify.                                               !!!
// !!! This file was generated by running `python3 GenerateCPP.py`  !!!
// !!! in multio/share/grib2                                      . !!!
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#include <string>
#include <cstdint>
#include <sstream>
#include <optional>
#include <variant>
#include <unordered_map>
#include <vector>
#include <set>
#include <functional>

#include "multio/message/Metadata.h"
#include "multio/message/MetadataException.h"

namespace multio::grib2 {{

{keyTypeEnum}


template<typename Key = std::string>
struct Grib2ProductHandler {{

{keyTypeLookupMapTypes}

{enums}

{parsers}

{decisionMapType}

std::int64_t inferProductDefinitionTemplateNumber(const message::Metadata& metadata) const {{
    // TODO may be read from file in future
    static const DecisionMap map{{{decisionMap}}};

    {parseMap}
}}


std::reference_wrapper<const KeyList> keysForPdt(std::int64_t pdt) const {{
    // TODO may be read from file in future
    static const PdtKeyMap map{{{pdtKeyMap}}};

    if (auto search = map.find(pdt); search != map.end()) {{
        return std::cref(search->second);
    }}

    std::ostringstream oss;
    oss << "Unknown ProductDefinitionTemplate " << pdt;
    throw message::MetadataException{{oss.str()}};
}}

std::reference_wrapper<const KeyInfo> keyInfoForKey(const Key& k) const {{
    static const KeyInfoMap map{{{keyInfoMap}}};

    if (auto search = map.find(k); search != map.end()) {{
        return std::cref(search->second);
    }}

    std::ostringstream oss;
    oss << "Unknown key " << k;
    throw message::MetadataException{{oss.str()}};
}}

}};

}}
""".format(
    keyTypeEnum=generateCPPKeyTypeEnum(allKeyTypes),
    keyTypeLookupMapTypes=keyTypeLookupMapTypesString,
    enums=enums,
    parsers=parsers,
    decisionMapType=decisionMapTypeString,
    decisionMap=buildDecisionMapString(categoryHandleOrder, {}, categorySelectorsWithMappedPdt),
    parseMap=buildParseMapString(categoryHandleOrder, []),
    pdtKeyMap=buildPdtKeyMapStr(),
    keyInfoMap=buildKeyTypeMapStr(),
)



def generateCPPTestInclude(categorySelectorsWithMappedPdt):
    """
    Produce a string with valid CPP code that contains a vector with an pairs of ptd and an unordered map as selector it should be mapped by
    """
    vals = ["""{{{pdt}, {{{selStr}}} }}""".format(pdt=s[3][0], selStr=", ".join(["""{{ "{}", "{}" }}""".format(k,v) for (k,v) in s[0].items()])) for s in categorySelectorsWithMappedPdt]
    return """
#pragma once
#include <vector>
#include <unordered_map>

#include "multio/message/Metadata.h"

namespace multio::grib2::test {{

struct PdtWithSelector{{
    std::int64_t productDefinitionTemplateNumber;
    message::Metadata selector;
}};

const static std::vector<PdtWithSelector> mappedPdtAndSelectors{{
{}
}};
}}
""".format(", ".join(vals))




# Gen C++
with open('../../src/multio/grib2/GeneratedProductHandler.h', "w") as outFile:
    cppString = generateCPP(categories, categorySelectorsWithMappedPdt, categoriesWithAllPossibleValues, pdt, keyMappings, keyTypes, allKeyTypes)
    outFile.write(cppString)

with open('../../src/multio/grib2/GeneratedProductHandlerTest.h', "w") as outFile:
    cppString = generateCPPTestInclude(categorySelectorsWithMappedPdt)
    outFile.write(cppString)


