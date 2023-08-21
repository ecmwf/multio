# import pyyaml module
import os
import yaml
import functools

from PDT import pdt, groups, categoriesWithAllPossibleValues, categorySelectorsWithMappedPdt, generateCategoryGroupArcs, categories, evalToFlatKeys, flattenGroups, flattenGroupsToArcs, keyMappings
from yaml.loader import SafeLoader



pdtNodes = {pdtId: {"type": "pdt"} for pdtId in pdt.keys()}
groupNodes = {g: {"type": "keys::group"} for g in groups.keys()}
categoryNodes = { c: {"type": "category::"} for (c, v) in categoriesWithAllPossibleValues.items() }
categoryValueNodes = { ("{}::{}".format(c,vi)): {"type": "category::{}".format(c)} for (c, v) in categoriesWithAllPossibleValues.items() for vi in v}
keyMapNodes = {"keyMap::{}".format(k): {"type": "keys::map"} for k in keyMappings.keys() }
keyNodes = {"key::{}".format(k): {"type": "keys::key"} for k in set([k for g in groups.values() for k in evalToFlatKeys(g, groups)]) }


nodes = {**pdtNodes, **groupNodes, **categoryNodes, **categoryValueNodes, **keyNodes, **keyMapNodes}
sourceTargetMap = { n: set([]) for n in nodes.keys()}


# Get group - pdt arcs - change source/target
allPDTGroupLinks = [{"source": arc["target"], "target": arc["source"] } for p in pdt.values() for arc in p['arcs']]
for l in allPDTGroupLinks:
    sourceTargetMap[l["source"]].add(l["target"])

for (c, v) in categoriesWithAllPossibleValues.items():
    for vi in v:
        sourceTargetMap[c].add("{}::{}".format(c,vi))


# Keys
for (sel, igroups, egroups, pdtDef) in categorySelectorsWithMappedPdt:
    for (k,v) in sel.items():
        source = "{}::{}".format(k,v)

        sourceTargetMap[source].add(pdtDef[0])

    for missingCat in set(categoriesWithAllPossibleValues.keys()).difference(set(sel.keys())):
        k = missingCat
        v = None
        source = "{}::{}".format(k,v)

        sourceTargetMap[source].add(pdtDef[0])




for catArcs in  generateCategoryGroupArcs(categories):
    s = catArcs["source"]
    t = catArcs["target"]
    sourceTargetMap[s].add(t)

# Keys
for (group, keys) in groups.items():
    for k in evalToFlatKeys(keys, groups, doExtend=False):
        sourceTargetMap[group].add("key::{}".format(k))

for (km, keys) in keyMappings.items():
    kmStr = "keyMap::{}".format(km)
    for k in evalToFlatKeys(keys, keyMappings, doExtend=False):
        kStr = "key::{}".format(k)
        if kStr in keyNodes.keys():
            sourceTargetMap[kStr].add(kmStr)

    for k in flattenGroups(keys, keyMappings, default=None):
        sourceTargetMap["keyMap::{}".format(k)].add(kmStr)

nodeList = [ {"id": "{}".format(nid), **v} for (nid, v) in nodes.items()]
linkList = [ {"source": "{}".format(source), "target": "{}".format(target), "sourceLinkCount": len(targets)} for (source, targets) in sourceTargetMap.items() for target in targets]

types = set([n["type"] for n in nodes.values()])

def buildTypeGroupValues(types):
    return functools.reduce(
        lambda acc, tt: {**acc, tt[0]: (acc[tt[0]] + [tt[1]] if tt[0] in acc.keys() else [tt[1]])},
        ( (ts[0] if len(ts) > 1 else "",
           "::".join(ts[1:]) if len(ts) > 1 else ts[0])
           for ts in (t.split("::") for t in types)
        ),
        {})

typeGroupValues = buildTypeGroupValues(types)

# Custom order
typeGroupOrder = [ "", "category", "keys" ]
typeValueOrder = [ "", "timeExtent", "timeFormat", "forecastType", "forecastSubType", "productCategory", "productSubCategory" ]

with open('d3jsInteractive/data.js', "w") as outFile:
    strVal = """
data = {}
""".format({"links": linkList, "nodes": nodeList, "nodeMap": {"{}".format(k): v for (k,v) in nodes.items()}, "types": list(types), "typeGroups": typeGroupValues, "typeGroupOrder": typeGroupOrder, "typeValueOrder": typeValueOrder })
    outFile.write(strVal)
