import functools
import os
from PDT import categories, categorySelectorsWithMappedPdt, categoriesWithAllPossibleValues, camelToPascalCase, listAllCategoriesInOrder, defaultCategoryValue

def generateCPPEnumForCategory(categories, categoryName, setOfValues, namespace):
    """
    Produce string with valid CPP code representing a enum class for a given category.
    """
    values = []
    default = None if (None in setOfValues) or (categoryName not in categories.name()) else defaultCategoryValue(categories[categoryName])
    defaultVal = default if default is None else camelToPascalCase(default)
    values.append(f"    {defaultVal} = 0,")
    for v in setOfValues.difference(set([default])):
        values.append("    {},".format(v if v is None else camelToPascalCase(v)))


    enumName=camelToPascalCase(categoryName)
    valuesStr="\n".join(values)
    
    readIfElse=[]
    writeCase=[]
    for v in setOfValues:
        valName=v if v is None else camelToPascalCase(v)
        values.append(f"""{{"{v}", {enumName}::{valName}}}""")
        
        readIfElse.append(f"""if (s == "{v}") {{ return {enumName}::{valName}; }}""")
        writeCase.append(f"""case {enumName}::{valName}: return "{v}"; """)
    
    readIfElseStr="\n".join(readIfElse)
    writeCaseStr="\n".join(writeCase)
    
    
    errReadEnumValuesStr=", ".join([f"{v}" for v in setOfValues])
    errReadEnumStr = f"Value for {categoryName} does not match on of the following [{errReadEnumValuesStr}]"
    return f"""
namespace {namespace} {{

enum class {enumName}: std::uint64_t
{{
{valuesStr}
}};

}}

namespace multio::datamod {{

template <>
struct DumpType<{namespace}::{enumName}> {{
    static std::string dump({namespace}::{enumName} v) {{
        using namespace {namespace};
        switch (v) {{
            {writeCaseStr}
            default:
                throw multio::mars2grib::Mars2GribException("DumpType<{enumName}>::dump: Unexpected value for {enumName}", Here());
        }}
    }}
}};

template <>
struct ParseType<{namespace}::{enumName}> {{
    static {namespace}::{enumName} parse(const std::string& s) {{
        using namespace {namespace};
        {readIfElseStr}
        throw multio::mars2grib::Mars2GribException{{std::string("ParseType<{enumName}>::parse(\"") + s + std::string("\"): {errReadEnumStr}"), Here()}};
    }}
}};

}}


namespace multio::util {{

template<>
struct Print<{namespace}::{enumName}> {{
    static void print(PrintStream& ps, const {namespace}::{enumName}& t) {{
      util::print(ps, multio::datamod::TypeDumper<{namespace}::{enumName}>::dump(t));
    }}
}};

template <>
struct TypeToString<{namespace}::{enumName}> {{
    std::string operator()() const {{ return "{namespace}::{enumName}"; }};
}};

}}  // namespace multio::util


"""


def generateCPPPDTCatRecord(categories, categoriesWithAllPossibleValues, categoryHandleOrder,pdtCatName, namespace):
    """
    Produce string with valid CPP code defining a enum with all categories and a KeySet
    """
    pdtCatStringValue="product-categories"
    
    catEnumValues = []
    for c in categoryHandleOrder:
        catEnumValues.append(f"    {camelToPascalCase(c)},")


    catEnumValuesStr="\n".join(catEnumValues)
    
    
    entryDefs=[]
    for index, c in enumerate(categoryHandleOrder, start=1):
        default = None if (None in categoriesWithAllPossibleValues[c]) or (c not in categories.name()) else defaultCategoryValue(categories[c])
        defaultVal = default if default is None else camelToPascalCase(default)
        kdStr = f"""constexpr auto {camelToPascalCase(c)}Entry = dm::EntryDef<{camelToPascalCase(c)}>{{"{c}"}}.withDefault({camelToPascalCase(c)}::{defaultVal}).withAccessor([](auto&& v){{ return &v.{c}; }});"""
        entryDefs.append(kdStr)
   
    entryDefStr="\n".join(entryDefs)


    entries=[]
    for index, c in enumerate(categoryHandleOrder, start=1):
        entries.append(f"""    dm::EntryType_t<decltype({camelToPascalCase(c)}Entry)> {c};""")
   
    entriesStr="\n".join(entries)

    entriesList=[]
    for index, c in enumerate(categoryHandleOrder, start=1):
        entriesList.append(f"{camelToPascalCase(c)}Entry")
   
    entriesListStr=", ".join(entriesList)
   
    return f"""
namespace {namespace} {{

namespace dm = multio::datamod;

{entryDefStr}

struct {pdtCatName} {{
{entriesStr}

    static constexpr std::string_view record_name_ = "{pdtCatStringValue}";
    static constexpr auto record_entries_
        = std::make_tuple({entriesListStr});
}};

}};  // namespace {namespace}

namespace std {{

template<>
struct equal_to<{namespace}::{pdtCatName}>: multio::datamod::EqualToRecord {{}};
template<>
struct not_equal_to<{namespace}::{pdtCatName}>: multio::datamod::NotEqualToRecord {{}};


template<>
struct hash<{namespace}::{pdtCatName}>: multio::datamod::HashRecord {{}};
}}

namespace multio::util {{
template<>
struct Print<{namespace}::{pdtCatName}>: multio::datamod::PrintRecord {{}};

template <>
struct TypeToString<{namespace}::{pdtCatName}> {{
    std::string operator()() const {{ return "{namespace}::{pdtCatName}"; }};
}};
}}
"""





def generateCPP(categories, categorySelectorsWithMappedPdt, categoriesWithAllPossibleValues, namespace):
    """
    Produce a string with valid CPP code that produces a function that can infer a PDT.
    """
    enums="\n\n\n".join([generateCPPEnumForCategory(categories, k, v, namespace=namespace) for (k,v) in categoriesWithAllPossibleValues.items()])

    categoryHandleOrder = listAllCategoriesInOrder(categories)
    
    pdtCatName="PDTCat"
    
    pdtRecord = generateCPPPDTCatRecord(categories, categoriesWithAllPossibleValues, categoryHandleOrder, pdtCatName=pdtCatName, namespace=namespace);

    decisionMapTypeString = f"using DecisionMap = std::unordered_map<{pdtCatName}, std::int64_t>; "

    def buildDecisionMapString(subCatList, selector, mappedSelectorList):
        if len(subCatList) == 0:
            if len(mappedSelectorList) == 1:
                return [([], "{}".format(mappedSelectorList[0][3][0]))]
            else:
                # Should not occur, as already checked
                raise ValueError(f"buildDecisionMapString: selector {selector} matches multiple or no pdts: {mappedSelectorList}")

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

                
                default = None if (None in categoriesWithAllPossibleValues[cat]) or (cat not in categories.name()) else defaultCategoryValue(categories[cat])
                defaultVal = default if default is None else camelToPascalCase(default)
        
                enumVal= val if val is None else camelToPascalCase(val)
                kvStr = f"{enumName}::{enumVal}"

                for (subEnums, pdtVal) in buildDecisionMapString(subCatList[1:], {cat: val, **selector}, selectorsForVal):
                    # Remove this if cond when None/default should be explicitly in the list
                    if val is None:
                        # Basically just forward
                        cases.append(([f"{enumName}::{defaultVal}", *subEnums], pdtVal))
                    else:
                        cases.append(([kvStr, *subEnums], pdtVal))

            return cases

    decisionMapCases=buildDecisionMapString(categoryHandleOrder, {}, categorySelectorsWithMappedPdt)
    
    decisionMapCasesStr=",\n".join([f"""{{{pdtCatName}{{ {", ".join(subEnums)} }}, {pdtVal} }}""" for (subEnums, pdtVal) in decisionMapCases])
    decisionMap=f"""{{{decisionMapCasesStr}}}""" # Wrapped in initializer list
    return f"""
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
// !!! in multio/share/multiom/generate_knowledge.                  !!!
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#include <optional>
#include <variant>
#include <unordered_map>
#include <vector>
#include <functional>

#include "multio/util/Hash.h"
#include "multio/util/TypeTraits.h"
#include "multio/util/TypeToString.h"
#include "multio/util/Print.h"

#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/core/TypeParserDumper.h"
#include "multio/datamod/core/Hash.h"
#include "multio/datamod/core/Print.h"
#include "multio/datamod/core/Compare.h"
#include "multio/mars2grib/Mars2GribException.h"

{enums}

{pdtRecord}

namespace {namespace} {{

template<typename DummyArg = void>
struct InferPdt {{

{decisionMapTypeString}


std::int64_t inferProductDefinitionTemplateNumber(const {pdtCatName}& pdtCat) const {{
    using namespace multio::datamod;
    static const DecisionMap map{{{decisionMap}}};
    
    if (auto search = map.find(pdtCat); search != map.end()) {{
        return search->second;
    }}
    
    std::ostringstream oss;
    oss << "PDT categories can not be mapped to a pdt number: ";
    util::print(oss, pdtCat);
    throw Mars2GribException(oss.str(), Here());
}}

}};

}}
"""



def generateCPPTestInclude(categorySelectorsWithMappedPdt, namespace):
    """
    Produce a string with valid CPP code that contains a vector with an pairs of ptd and an unordered map as selector it should be mapped by
    """
    vals = ", ".join(["""{{{pdt}, {{{selStr}}} }}""".format(pdt=s[3][0], selStr=", ".join(["""{{ "{}", "{}" }}""".format(k,v) for (k,v) in s[0].items()])) for s in categorySelectorsWithMappedPdt])
    return f"""
#pragma once
#include <vector>
#include <unordered_map>

#include "multio/message/Metadata.h"

namespace {namespace}::test {{

struct PdtWithSelector{{
    std::int64_t productDefinitionTemplateNumber;
    message::Metadata selector;
}};

const static std::vector<PdtWithSelector> mappedPdtAndSelectors{{
{vals}
}};
}}
"""


namespace="multio::mars2grib::rules"

def clangFormat(fname):
    os.system(f"clang-format -i {fname}")

# Gen C++
cppFile='../../../src/multio/mars2grib/generated/InferPDT.h'
with open(cppFile, "w") as outFile:
    cppString = generateCPP(categories, categorySelectorsWithMappedPdt, categoriesWithAllPossibleValues, namespace=namespace)
    outFile.write(cppString)
clangFormat(cppFile)
    

cppTestFile='../../../src/multio/mars2grib/generated/InferPDTTest.h'
with open(cppTestFile, "w") as outFile:
    cppString = generateCPPTestInclude(categorySelectorsWithMappedPdt, namespace=namespace)
    outFile.write(cppString)
clangFormat(cppTestFile)



