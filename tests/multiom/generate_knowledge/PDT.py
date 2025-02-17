# import pyyaml module
import os
import yaml
import functools

from yaml.loader import SafeLoader


def camelToPascalCase(n):
    return n[0].upper () + n[1:]


def extendOrAppend(l, v):
    if isinstance(v, list):
        l.extend(v)
    else:
        l.append(v)



# Parsing Groups and Keys

def evalToFlatKeys(v, groups, default=None, doExtend=True, **kwargs):
    """
    Lookup groups recursively and generate a flat list
    """
    if v is None:
        return []

    def extend(g,**kwargs):
        if not doExtend:
            return []

        if g not in groups.keys():
            raise ValueError("{} not found in groups".format(g))
        return evalToFlatKeys(groups[g], groups, default=None, doExtend=doExtend, **kwargs)

    def extendList(l, default=None, **kwargs):
        res = []
        for li in l:
            extendOrAppend(res, evalToFlatKeys(li, groups=groups, default=default, doExtend=doExtend, **kwargs))
        return res


    if (isinstance(v, list)):
        return extendList(v, default=default, **kwargs)

    elif (isinstance(v, dict)):
        if ("groups" in v.keys()):
            return extendList(v["groups"], default="extend", **kwargs)

        elif ("extend" in v.keys()):
            return extend(v["extend"], **{**kwargs, **{k: v[k] for k in (set(v.keys()).difference({'extend'}))}});

        elif ("append" in v.keys()):
            if v["append"] not in kwargs.keys():
                return []
            else:
                return evalToFlatKeys(kwargs[v["append"]], groups, doExtend=doExtend, **kwargs)

        elif ("repeatFor" in v.keys()):
            return extendList(v["values"], default=None, **kwargs);
        else:
            raise ValueError("Unknown dict to evaluate: {}".format(str(v)))

    if (default == "extend"):
        return extend(v, **kwargs)
    else:
        return v



def flattenGroups(g, groups, default="extend", **kwargs):
    """
    Iterate groups and explicitly list all nested groups and composed types in a flat list
    """
    if g is None:
        return []

    def extend(g,**kwargs):
        res = [g]
        if g not in groups.keys():
            raise ValueError("{} not found in groups".format(g))

        # Add group and eval nested groups if existing
        extendOrAppend(res, flattenGroups(groups[g], groups, default=None, **kwargs))
        return res

    def extendList(l, default=None, **kwargs):
        res = []
        for li in l:
            extendOrAppend(res, flattenGroups(li, groups=groups, default=default, **kwargs))
        return res

    if (isinstance(g, list)):
        return extendList(g, default=default, **kwargs)

    elif (isinstance(g, dict)):
        if ("groups" in g.keys()):
            return extendList(g["groups"], default="extend", **kwargs);

        elif ("extend" in g.keys()):
            return extend(g["extend"], **{**kwargs, **{k: g[k] for k in (set(g.keys()).difference({'extend'}))}});

        elif ("append" in g.keys()):
            if g["append"] not in kwargs.keys():
                return []
            else:
                return flattenGroups(kwargs[g["append"]], groups, default=None, **kwargs)

        elif ("repeatFor" in g.keys()):
            return extendList(g["values"], default=None, **kwargs);
        else:
            raise ValueError("Unknown dict to evaluate: {}".format(str(g)))

    if (default == "extend"):
        return extend(g, **kwargs)
    else:
        return []


def flattenGroupsToArcs(fromValue, g, groups, default="extend", listNestedOnExtension=True, **kwargs):
    """
    Iterate all groups and list all connections to nested groups in a dict with keys "target" (derived/extended group) and "source" (nested/base group).
    Used
    """
    if g is None:
        return []

    def extend(f, g,**kwargs):
        res = [{"target": f, "source": g}]
        if g not in groups.keys():
            raise ValueError("{} not found in groups".format(g))

        # Add group and eval nested groups if existing
        extendOrAppend(res, flattenGroupsToArcs(g, groups[g], groups, default=None, listNestedOnExtension=listNestedOnExtension, **kwargs))
        return res

    def extendList(f, l, default=None, **kwargs):
        res = []
        for li in l:
            extendOrAppend(res, flattenGroupsToArcs(f, li, groups=groups, default=default, listNestedOnExtension=listNestedOnExtension, **kwargs))
        return res

    if (isinstance(g, list)):
        return extendList(fromValue, g, default=default, **kwargs)

    elif (isinstance(g, dict)):
        if ("groups" in g.keys()):
            return extendList(fromValue, g["groups"], default="extend", **kwargs);

        elif ("extend" in g.keys()):
            if listNestedOnExtension:
                ret = []
                extendOrAppend(ret, extend(fromValue, g["extend"], **kwargs))
                for k in (set(g.keys()).difference({'extend'})):
                    extendOrAppend(ret, extendList(fromValue, g[k], default=None, **kwargs))
                return ret;
            else:
                return extend(fromValue, g["extend"], **{**kwargs, **{k: g[k] for k in (set(g.keys()).difference({'extend'}))}});

        elif ("append" in g.keys()):
            if listNestedOnExtension:
                return []

            if g["append"] not in kwargs.keys():
                return []
            else:
                return flattenGroupsToArcs(fromValue, kwargs[g["append"]], groups, default=None, listNestedOnExtension=listNestedOnExtension, **kwargs)

        elif ("repeatFor" in g.keys()):
            return extendList(fromValue, g["values"], default=None, **kwargs);
        else:
            raise ValueError("Unknown dict to evaluate: {}".format(str(g)))

    if (default == "extend"):
        return extend(fromValue, g, **kwargs)
    else:
        return []





# Category operations

def valueListForCategory(c):
    """
    Extract list of values from a category definition
    """
    if isinstance(c, list):
        return c
    elif isinstance(c, dict):
        if "values" in c.keys():
            return c["values"]
        raise ValueError("valueListForCategory: no key 'values' in category dict: {}".format(c))
    else:
        raise ValueError("valueListForCategory: category is no list and no dict: {}".format(c))

def categoryValueGroupsAndSubDefinitions(entry):
    """
    Extact the string value, list of groups and possible subdefinitons (or None)
    from an entry in a category value list
    """
    if isinstance(entry,str):
        return (entry, [entry], None)
    elif isinstance(entry,dict):
        if len(entry) == 1:
            (name, val) = next(iter(entry.items()))
            # check if val is a list of groups - then name is just an alias
            if isinstance(val, list):
                return (name, val, None)
            else:
                return (name, [name], val)
        elif "name" in entry.keys():
            return (entry["name"], entry["groups"] if "groups" in entry.keys() else ([entry["name"]] if entry["name"] is not None else None), entry["categories"] if "categories" in entry.keys() else None)
        else:
            raise ValueError("categoryValueGroupsAndSubDefinitions: value dict must have exactly one mapping or the keys 'name' and 'groups' (optionally 'categories'): {}".format(entry))


    elif entry is None:
        return (None, None, None)
    raise ValueError("categoryValueGroupsAndSubDefinitions: value is no str, no dict and not None: {}".format(entry))



def defaultCategoryValue(v):
    """
    Returns the default value (may be None) of a category definition
    """
    def defaultFromList(l):
        # Check in none is contained in list
        for li in l:
            if li is None:
                return None

        # Return first
        return categoryValueGroupsAndSubDefinitions(l[0])[0]

    if isinstance(v, list):
        return defaultFromList(v)

    elif isinstance(v, dict):
        if "default" in v.keys():
            vd = v["default"]
            if isinstance(vd, str):
                return vd
            else:
                raise ValueError("defaultCategoryValue: default value is no string: {}".format(vd))
        if "values" in v.keys():
            return defaultFromList(v["values"])
        else:
            raise ValueError("defaultCategoryValue: no key 'default' or 'values' in category dict: {}".format(v))


def groupsFromCategorySelection(categorySelection, categories):
    """
    Evaluates a categorySelector (dictionary with category name as keys and a specific value of the corresponding category as value)
    and returns a set of groups that the specific combination of categories form.

    Note: This function is not used, by maybe useful for development and interactive exlporation with python.
    """
    finishedCategories = set()
    groupSet = set()
    exclusiveGroupSet = set()

    def resolveCategory(cName, cDef):
        if cName not in finishedCategories:
            finishedCategories.add(cName)

            # First resolve value
            cv = None
            if cName in categorySelection.keys():
                cv = categorySelection[cName]
                if not isinstance(cv, str):
                    raise ValueError("groupsFromCategorySelection: mapped value in categorySelection for category {} is supposed to be a string: {}".format(cName, cv))
            else:
                cv = defaultCategoryValue(cDef)

            # Now lookup definition
            for v in valueListForCategory(cDef):
                (cValName, groups, subDefs) = categoryValueGroupsAndSubDefinitions(v)
                if (cValName == cv) or (cValName is None and cv is None):
                    groupSet.update(set(groups))

                    if subDefs is not None:
                        for (subCName, subDef) in subDefs.items():
                            resolveCategory(subCName, subDef)
                else:
                    # if not (isinstance(cDef, dict) and "nonExclusive" in cDef.keys() and cDef["nonExclusive"]):
                    exclusiveGroupSet.update(set(groups))

    for (c, cdef) in categories.items():
        resolveCategory(c, cdef)

    return {"groups": groupSet, "exclusiveGroups": exclusiveGroupSet}


def checkPDTSForUnqiueSetPerCategory(pdt, categories):
    """
    Takes dict with pdt definitons and categories and checks that all values of a category are
    exclusive to each other for the set of groups of each pdt.
    """
    for (pdtId, pdtVal) in pdt.items():
        groupSet = pdtVal["groupSet"]

        def checkCat(cName, cDef):
            alreadyMatched = None
            for v in valueListForCategory(cDef):
                (cValName, groups, subDefs) = categoryValueGroupsAndSubDefinitions(v)

                if groups is not None and set(groups).issubset(groupSet):
                    if alreadyMatched is not None:
                        raise ValueError("checkPDTSForUnqiueSetPerCategory Already matched pdt: {}, cName: {}, groups: {} - alreadyMatched: {}".format(pdtId, cName, groups, alreadyMatched))
                    alreadyMatched = set(groups)

                    if subDefs is not None:
                        for (subCName, subDef) in subDefs.items():
                            checkCat(subCName, subDef)

        for (cName, cDef) in categories.items():
            checkCat(cName, cDef)


def listAllCategoriesInOrder(categories):
    """
    Takes a dict of categories and lists category names and subcategory names in order.
    """
    def gen(cs):
        for (cName, cDef) in cs.items():
            yield cName

            for v in valueListForCategory(cDef):
                (_, _, subDefs) = categoryValueGroupsAndSubDefinitions(v)
                if subDefs is not None:
                    for n in gen(subDefs):
                        yield n

    vals = []
    for v in gen(categories):
        if v not in vals:
            vals.append(v)
    return vals

def generateAllCategoryCombinations(categories, emitWithDefaultExcluded=False):
    """
    Generator and yields dicts with all possible combinations of specific category -> category value mappings.
    """
    def generateForSingleCatgeory(cName, cDef):
        cDefValName = defaultCategoryValue(cDef)

        valWithGroups = [categoryValueGroupsAndSubDefinitions(v) for v in valueListForCategory(cDef)]
        allGroups = { g  for v in valWithGroups for g in ([] if v[1] is None else v[1])}
        for (cValName, igroups, subDefs) in valWithGroups:
            # egroups = set([])
            # if not (isinstance(cDef, dict) and "nonExclusive" in cDef.keys() and cDef["nonExclusive"]):
            egroups = allGroups.difference(set([]) if igroups is None else set(igroups))

            (sel1, groups1, egroups1) = ({ cName: cValName }, set([] if igroups is None else igroups), set([] if egroups is None else egroups))

            if subDefs is None:
                # If the value is default, also emit a selector without the value
                if emitWithDefaultExcluded and ((cValName == cDefValName) or (cValName is None and cDefValName is None)):
                    yield ({}, groups1, egroups1)
                yield (sel1, groups1, egroups1)
            else:
                for (sel2, groups2, egroups2) in combineCategories(subDefs):
                    if emitWithDefaultExcluded and ((cValName == cDefValName) or (cValName is None and cDefValName is None)):
                        yield ({ **sel2}, {*groups1, *groups2}, {*egroups1, *egroups2})
                    yield ({ **sel1, **sel2}, {*groups1, *groups2}, {*egroups1, *egroups2})

    def combineCategories(cats):
        if len(cats) == 0:
            yield ({}, set([]), set([]))
            return

        (cName, cDef) = next(iter(cats.items()))

        for (sel1, groups1, egroups1) in generateForSingleCatgeory(cName, cDef):
            for (sel2, groups2, egroups2) in combineCategories({k: v for (k,v) in cats.items() if k != cName}):
                yield ({**sel1, **sel2}, {*groups1, *groups2}, {*egroups1, *egroups2})

    return combineCategories(categories)

def generateCategoryGroupArcs(categories, valueTosubValueArcs=True):
    """
    Generator and yields dicts with all possible combinations of specific category -> category value mappings.
    """
    def generateForSingleCatgeory(cName, cDef, pCat=None, pCatVal=None):
        cDefValName = defaultCategoryValue(cDef)

        if pCat is not None:
            yield {"source": pCat, "target": cName}

        valWithGroups = [categoryValueGroupsAndSubDefinitions(v) for v in valueListForCategory(cDef)]
        for (cValName, igroups, subDefs) in valWithGroups:
            yield {"source": cName, "target": "{}::{}".format(cName, cValName)}
            if valueTosubValueArcs and pCat is not None:
                yield {"source": "{}::{}".format(pCat, pCatVal), "target": "{}::{}".format(cName, cValName)}

            if igroups is not None:
                for g in igroups:
                    yield {"source": "{}::{}".format(cName, cValName), "target": g}

            if subDefs is not None:
                for propagate in generateForCategories(subDefs, cName, cValName):
                    yield propagate

    def generateForCategories(cats, pCat=None, pCatVal=None):
        for (cName, cDef) in cats.items():
            for propagate in generateForSingleCatgeory(cName, cDef, pCat, pCatVal):
                yield propagate

    return generateForCategories(categories)



def pdtsMatchingGroupSet(pdt, groupSet, exclusiveGroupSet=set([])):
    """
    Selects all pdt which contain all groups in groupSet and not the groups in exclusiveGroupSet.
    """
    return {pdtId: pdtVal for (pdtId, pdtVal) in pdt.items() if (len(set(pdtVal["groupSet"]).intersection(exclusiveGroupSet)) == 0) and set(pdtVal["groupSet"]).issuperset(groupSet) }



def pdtsWithFewestKeysMatchingGroupSet(pdt, groupSet, exclusiveGroupSet=set([])):
    """
    Calls pdtsMatchingGroupSet and takes the pdt with fewest number of keys
    """
    pdts = pdtsMatchingGroupSet(pdt, groupSet, exclusiveGroupSet)
    if len(pdts) == 0:
        return None
    return next(iter(sorted( pdts.items(), key=lambda tup: len(tup[1]["keySet"]))))




# Check if all pdt could been matched
def checkIfAllPDTHaveAtLeastOneSelector(categorySelectorsWithMappedPdt):
  mappedPdtIds = {m[3][0] for m in categorySelectorsWithMappedPdt}
  allPdtIds = set(pdt.keys())

  if(mappedPdtIds != allPdtIds):
     raise ValueError("checkIfAllPDTHaveAtLeastOneSelector: The following pdt have not been mapped from a selector: {}".format(allPdtIds.difference(mappedPdtIds)))



# Compute all mappings
def listCategorySelectorsWithMappedPdt(pdt, categories):
    return list(((sel, groups, egroups, matchedPdt) for (sel, groups, egroups, matchedPdt) in ((sel, groups, egroups, pdtsWithFewestKeysMatchingGroupSet(pdt, groups, egroups)) for  (sel, groups, egroups) in generateAllCategoryCombinations(categories)) if matchedPdt is not None))






# Read in Files
def readPDT(fname='input/pdt.yaml'):
    pdt = None
    groups = None
    categories = None
    with open(fname) as f:
        pdt = yaml.load(f, Loader=SafeLoader)
        groups = pdt["groups"]
        categories = pdt["categories"]
        pdt = { k: {**v, "keySet": evalToFlatKeys(v["keys"], groups), "groupSet": flattenGroups(v["groups"], groups), "arcs": flattenGroupsToArcs(k, v["groups"], groups, listNestedOnExtension=True)} for (k,v) in pdt["pdt"].items()}
        # print(pdt)
    return (pdt, groups, categories)

allKeyTypes = set(["IntType", "StringType", "IntArrayType", "StringArrayType", "FloatType", "FloatArrayType"])
def readKeyMappings(fname="input/keyMappings.yaml"):
    keyMappings = None
    keyTypes = None
    with open(fname) as f:
        keyMappingsYAML = yaml.load(f, Loader=SafeLoader)
        keyMappings = keyMappingsYAML["keyMappings"]
        keyTypes = keyMappingsYAML["keyTypes"]
    return (keyMappings, keyTypes)




(pdt, groups, categories) = readPDT()
(keyMappings, keyTypes) = readKeyMappings()

keyMappingsForKey = functools.reduce(
        lambda acc, kTup: {**acc, kTup[1]: (acc[kTup[1]] + [kTup[0]] if kTup[1] in acc.keys() else [kTup[0]])},
        ((km, k) for (km,m) in keyMappings.items() for k in evalToFlatKeys(m, keyMappings)),
        {})

groupDependencyCountMap = { g: 0 for g in groups.keys() }
for (pdtVal, pdtDict) in pdt.items():
    for g in pdtDict["groupSet"]:
        groupDependencyCountMap[g] = groupDependencyCountMap[g]+1

# Check pdt keys
for (pdtId, pdtVal) in pdt.items():
    genKeys = evalToFlatKeys(pdtVal["groups"], groups, default="extend")
    pdtVal["keys"]
    for (l, r) in zip(genKeys, evalToFlatKeys(pdtVal["keys"], groups)):
        if l != r:
            print("Keys not matched for pdt {}: {} != {}".format(pdtId, l,r))


# Check categories
checkPDTSForUnqiueSetPerCategory(pdt, categories)

# Compute all mappings
categorySelectorsWithMappedPdt = listCategorySelectorsWithMappedPdt(pdt, categories)


# Gather all (sub)categories with all possible values
categoriesWithAllPossibleValues = {}

for s in categorySelectorsWithMappedPdt:
    for (k,v) in s[0].items():
        if not k in categoriesWithAllPossibleValues.keys():
            categoriesWithAllPossibleValues[k] = set([])
        categoriesWithAllPossibleValues[k].add(v)

# Explicitly check if some subcategories are not listed in a selector and add a default value None
for s in categorySelectorsWithMappedPdt:
    for (c, cv) in categoriesWithAllPossibleValues.items():
        if c not in s[0].keys():
            cv.add(None)



checkIfAllPDTHaveAtLeastOneSelector(categorySelectorsWithMappedPdt)

for (sel, igroups, egroups, pdtItem) in categorySelectorsWithMappedPdt:
    (pdtId, _) = pdtItem
    if "categorySelectors" not in pdt[pdtId].keys():
        pdt[pdtId]["categorySelectors"] = []
    pdt[pdtId]["categorySelectors"].append((sel, igroups, egroups))


# Optionall check if all PDT have exacly one selector - this depends if default values have been expanded
for (pdtId, pdtVal) in pdt.items():
    if len(pdtVal["categorySelectors"]) > 1:
        raise ValueError("pdt {} hase multiple categorySelectors: {}".format(pId, pdtVal["categorySelectors"]))
