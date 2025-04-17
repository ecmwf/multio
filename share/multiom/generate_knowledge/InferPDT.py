from pydantic import BaseModel
from typing import Union, List, Optional, TypeAlias, Dict

from PDT import categories, categorySelectorsWithMappedPdt, categoriesWithAllPossibleValues, listAllCategoriesInOrder


class FinalPDT(BaseModel):
    pdt: int


MatchPDTType: TypeAlias = Union[FinalPDT, 'MatchPDTCategory']

class MatchPDTCategory(BaseModel):
    category: str
    subMatch: Dict[str, MatchPDTType]
    default: Optional[MatchPDTType]


def inferPDT(catValuePairs: Dict[str, str], matcher: MatchPDTType) -> int:
    match matcher:
        case FinalPDT():
            return matcher.pdt
        case MatchPDTCategory():
            cat = matcher.category
            if cat not in catValuePairs.keys():
                if matcher.default:
                    return inferPDT(catValuePairs, matcher.default)
                raise ValueError(f"Category {cat} is not available in dict {catValuePairs}. Matcher {matcher}")
            catVal = catValuePairs[cat]

            if catVal not in matcher.subMatch.keys():
                if matcher.default:
                    return inferPDT(catValuePairs, matcher.default)
                raise ValueError(f"Category with value {{{cat}: {catVal}}} has no entry in sub matcher and no default is given. Possible values {list(matcher.subMatch.keys())}")

            subMatch = matcher.subMatch[catVal]
            return inferPDT(catValuePairs, subMatch)


def buildDecisionMap(subCatList, selector, mappedSelectorList) -> MatchPDTType:
    if len(subCatList) == 0:
        if len(mappedSelectorList) == 1:
            return FinalPDT(pdt = mappedSelectorList[0][3][0])
        else:
            # Should not occur, as already checked
            raise ValueError(f"buildDecisionMap: selector {selector} matches multiple or no pdts: {mappedSelectorList}")

    else:
        cat = subCatList[0]
        allVals = categoriesWithAllPossibleValues[cat]

        valsInSelectors = set([])
        for selTuple in mappedSelectorList:
            sel = selTuple[0]
            valsInSelectors.add(sel[cat] if cat in sel.keys() else None)

        default = None
        subMatch = {}
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
            sub = buildDecisionMap(subCatList[1:], {cat: val, **selector}, selectorsForVal)


            if val is None:
                # If there is only 1 val and it is default, we can skip checking the whole category
                if len(valsInSelectors) == 1:
                    return sub
                else:
                    default = sub
            else:
                subMatch[val] = sub

        return MatchPDTCategory(category=cat, subMatch=subMatch, default=default)


pdtCategoriesHandleOrder = listAllCategoriesInOrder(categories)
pdtMatcher=buildDecisionMap(pdtCategoriesHandleOrder, {}, categorySelectorsWithMappedPdt)
