#include "eckit/config/YAMLConfiguration.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/mars2grib/Mars2GribException.h"
#include "multio/mars2grib/EncoderConf.h"
#include "multio/mars2grib/Mars2GribException.h"
#include "multio/message/Metadata.h"

#include <sstream>

namespace multio::test {

auto mkAifsSingleBaseMd() {
    return message::Metadata{
        {"class", "ai"},  {"model", "aifs-single"}, {"expver", "0001"}, {"type", "fc"},   {"stream", "oper"},
        {"repres", "gg"}, {"packing", "ccsds"},     {"levelist", 700},  {"grid", "N320"}, {"step", 6},
        {"time", 0},      {"date", 20230901},       {"levtype", "pl"},  {"param", 133}};
}

auto mkMd() {
    return mkAifsSingleBaseMd();
}


std::vector<message::Metadata> aifsSingleParams() {
    std::vector<message::Metadata> res;

    // SFC params
    for (auto param : std::vector<int>{{134, 151, 165, 166, 167, 168, 235, 141, 136, 143, 228}}) {
        res.push_back({{"param", param}, {"levtype", "sfc"}});
    }

    // PL params
    for (auto param : std::vector<int>{{129, 130, 131, 132, 133, 135}}) {
        for (auto levelist : std::vector<int>{{50, 100, 150, 200, 250, 300, 400, 500, 600, 700, 850, 925, 1000}}) {
            res.push_back({{"param", param}, {"levtype", "pl"}, {"levelist", levelist}});
        }
    }

    return res;
}

std::vector<message::Metadata> aifsEnsParams() {
    auto res = aifsSingleParams();
    for (auto& md : res) {
        md.set("number", 27);
    }
    return res;
}

std::vector<message::Metadata> combineWithBaseMd(std::vector<message::Metadata> res) {
    for (auto& md : res) {
        md.updateNoOverwrite(mkAifsSingleBaseMd());
    }
    return res;
}
std::vector<message::Metadata> addSteps(const std::vector<message::Metadata>& inp) {
    std::vector<message::Metadata> res;
    for (auto step : std::vector<int>{{6, 12, 18, 24, 32}}) {
        for (auto md : inp) {
            md.set("step", step);
            res.push_back(std::move(md));
        }
    }

    return res;
}

std::vector<message::Metadata> mkAifsSingleMd() {
    return combineWithBaseMd(addSteps(aifsSingleParams()));
}
std::vector<message::Metadata> mkAifsEnsMd() {
    return combineWithBaseMd(addSteps(aifsEnsParams()));
}


mars2grib::EncoderSections expectedAIFSSingleEncoderSections(const datamod::MarsKeyValueSet& mars) {
    using namespace datamod;
    auto param = key<MarsKeys::PARAM>(mars);
    auto levelist = key<MarsKeys::LEVELIST>(mars);
    ASSERT(param.has());

    auto toSections = [](std::string&& str) {
        return read(datamod::KeySet<mars2grib::EncoderSectionsDef>{},
                    eckit::LocalConfiguration{eckit::YAMLConfiguration{std::move(str)}});
    };
    switch (param.get()) {
        case 134:
        case 235:
        case 141:
            return toSections(
                R"json({"indicator-section":{"template-number":0},"identification-section":{"template-number":0,"origin-configurator":{"type":"default"},"data-type-configurator":{"type":"default"},"reference-time-configurator":{"type":"default"},"tables-configurator":{"type":"default","local-tables-version":0}},"local-use-section":{"template-number":1},"grid-definition-section":{"template-number":40},"product-definition-section":{"template-number":0,"product-categories":{"timeExtent":"pointInTime","timeFormat":"None","spatialExtent":"None","processType":"None","processSubType":"None","productCategory":"None","productSubCategory":"None"},"param-configurator":{"type":"paramId"},"model-configurator":{"type":"default"},"point-in-time-configurator":{"type":"default"},"level-configurator":{"type":"surface"}},"data-representation-section":{"template-number":42}})json");
        case 151:
            return toSections(
                R"json({"indicator-section":{"template-number":0},"identification-section":{"template-number":0,"origin-configurator":{"type":"default"},"data-type-configurator":{"type":"default"},"reference-time-configurator":{"type":"default"},"tables-configurator":{"type":"default","local-tables-version":0}},"local-use-section":{"template-number":1},"grid-definition-section":{"template-number":40},"product-definition-section":{"template-number":0,"product-categories":{"timeExtent":"pointInTime","timeFormat":"None","spatialExtent":"None","processType":"None","processSubType":"None","productCategory":"None","productSubCategory":"None"},"param-configurator":{"type":"paramId"},"model-configurator":{"type":"default"},"point-in-time-configurator":{"type":"default"},"level-configurator":{"type":"meanSea"}},"data-representation-section":{"template-number":42}})json");
        case 165:
        case 166:
            return toSections(
                R"json({"indicator-section":{"template-number":0},"identification-section":{"template-number":0,"origin-configurator":{"type":"default"},"data-type-configurator":{"type":"default"},"reference-time-configurator":{"type":"default"},"tables-configurator":{"type":"default","local-tables-version":0}},"local-use-section":{"template-number":1},"grid-definition-section":{"template-number":40},"product-definition-section":{"template-number":0,"product-categories":{"timeExtent":"pointInTime","timeFormat":"None","spatialExtent":"None","processType":"None","processSubType":"None","productCategory":"None","productSubCategory":"None"},"param-configurator":{"type":"paramId"},"model-configurator":{"type":"default"},"point-in-time-configurator":{"type":"default"},"level-configurator":{"type":"heightAboveGroundAt10m","fixed-level":10}},"data-representation-section":{"template-number":42}})json");
        case 167:
        case 168:
            return toSections(
                R"json({"indicator-section":{"template-number":0},"identification-section":{"template-number":0,"origin-configurator":{"type":"default"},"data-type-configurator":{"type":"default"},"reference-time-configurator":{"type":"default"},"tables-configurator":{"type":"default","local-tables-version":0}},"local-use-section":{"template-number":1},"grid-definition-section":{"template-number":40},"product-definition-section":{"template-number":0,"product-categories":{"timeExtent":"pointInTime","timeFormat":"None","spatialExtent":"None","processType":"None","processSubType":"None","productCategory":"None","productSubCategory":"None"},"param-configurator":{"type":"paramId"},"model-configurator":{"type":"default"},"point-in-time-configurator":{"type":"default"},"level-configurator":{"type":"heightAboveGroundAt2m","fixed-level":2}},"data-representation-section":{"template-number":42}})json");
        case 136:
            return toSections(
                R"json({"indicator-section":{"template-number":0},"identification-section":{"template-number":0,"origin-configurator":{"type":"default"},"data-type-configurator":{"type":"default"},"reference-time-configurator":{"type":"default"},"tables-configurator":{"type":"default","local-tables-version":0}},"local-use-section":{"template-number":1},"grid-definition-section":{"template-number":40},"product-definition-section":{"template-number":0,"product-categories":{"timeExtent":"pointInTime","timeFormat":"None","spatialExtent":"None","processType":"None","processSubType":"None","productCategory":"None","productSubCategory":"None"},"param-configurator":{"type":"paramId"},"model-configurator":{"type":"default"},"point-in-time-configurator":{"type":"default"},"level-configurator":{"type":"entireAtmosphere"}},"data-representation-section":{"template-number":42}})json");
        case 143:
        case 228:
            return toSections(
                R"json({"indicator-section":{"template-number":0},"identification-section":{"template-number":0,"origin-configurator":{"type":"default"},"data-type-configurator":{"type":"default"},"reference-time-configurator":{"type":"default"},"tables-configurator":{"type":"default","local-tables-version":0}},"local-use-section":{"template-number":1},"grid-definition-section":{"template-number":40},"product-definition-section":{"template-number":8,"product-categories":{"timeExtent":"timeRange","timeFormat":"None","spatialExtent":"None","processType":"None","processSubType":"None","productCategory":"None","productSubCategory":"None"},"param-configurator":{"type":"paramId"},"model-configurator":{"type":"default"},"time-statistics-configurator":{"type":"since-beginning-of-forecast","type-of-statistical-processing":"accumul","overall-length-of-timerange":"1h"},"level-configurator":{"type":"surface"}},"data-representation-section":{"template-number":42}})json");
        case 129:
        case 130:
        case 131:
        case 132:
        case 133:
        case 135: {
            ASSERT(levelist.has());
            if (levelist.get() < 100) {
                return toSections(
                    R"json({"indicator-section":{"template-number":0},"identification-section":{"template-number":0,"origin-configurator":{"type":"default"},"data-type-configurator":{"type":"default"},"reference-time-configurator":{"type":"default"},"tables-configurator":{"type":"default","local-tables-version":0}},"local-use-section":{"template-number":1},"grid-definition-section":{"template-number":40},"product-definition-section":{"template-number":0,"product-categories":{"timeExtent":"pointInTime","timeFormat":"None","spatialExtent":"None","processType":"None","processSubType":"None","productCategory":"None","productSubCategory":"None"},"param-configurator":{"type":"paramId"},"model-configurator":{"type":"default"},"point-in-time-configurator":{"type":"default"},"level-configurator":{"type":"isobaricInPa"}},"data-representation-section":{"template-number":42}})json");
            }
            else {
                return toSections(
                    R"json({"indicator-section":{"template-number":0},"identification-section":{"template-number":0,"origin-configurator":{"type":"default"},"data-type-configurator":{"type":"default"},"reference-time-configurator":{"type":"default"},"tables-configurator":{"type":"default","local-tables-version":0}},"local-use-section":{"template-number":1},"grid-definition-section":{"template-number":40},"product-definition-section":{"template-number":0,"product-categories":{"timeExtent":"pointInTime","timeFormat":"None","spatialExtent":"None","processType":"None","processSubType":"None","productCategory":"None","productSubCategory":"None"},"param-configurator":{"type":"paramId"},"model-configurator":{"type":"default"},"point-in-time-configurator":{"type":"default"},"level-configurator":{"type":"isobaricInhPa"}},"data-representation-section":{"template-number":42}})json");
            }
        }
        default:
            std::ostringstream oss;
            oss << "aifsSingleConfForParam: no mapping for param: " << param;
            throw mars2grib::Mars2GribException(oss.str(), Here());
    }
}


mars2grib::EncoderSections expectedAIFSEnsEncoderSections(const datamod::MarsKeyValueSet& mars) {
    using namespace datamod;
    auto param = key<MarsKeys::PARAM>(mars);
    auto levelist = key<MarsKeys::LEVELIST>(mars);
    ASSERT(param.has());

    auto toSections = [](std::string&& str) {
        return read(datamod::KeySet<mars2grib::EncoderSectionsDef>{},
                    eckit::LocalConfiguration{eckit::YAMLConfiguration{std::move(str)}});
    };
    switch (param.get()) {
        case 134:
        case 235:
        case 141:
            return toSections(
                R"json({"indicator-section":{"template-number":0},"identification-section":{"template-number":0,"origin-configurator":{"type":"default"},"data-type-configurator":{"type":"default"},"reference-time-configurator":{"type":"default"},"tables-configurator":{"type":"default","local-tables-version":0}},"local-use-section":{"template-number":1},"grid-definition-section":{"template-number":40},"product-definition-section":{"template-number":1,"product-categories":{"timeExtent":"pointInTime","timeFormat":"None","spatialExtent":"None","processType":"None","processSubType":"ensemble","productCategory":"None","productSubCategory":"None"},"param-configurator":{"type":"paramId"},"model-configurator":{"type":"default"}, "ensemble-configurator":{"type":"default"},"point-in-time-configurator":{"type":"default"},"level-configurator":{"type":"surface"}},"data-representation-section":{"template-number":42}})json");
        case 151:
            return toSections(
                R"json({"indicator-section":{"template-number":0},"identification-section":{"template-number":0,"origin-configurator":{"type":"default"},"data-type-configurator":{"type":"default"},"reference-time-configurator":{"type":"default"},"tables-configurator":{"type":"default","local-tables-version":0}},"local-use-section":{"template-number":1},"grid-definition-section":{"template-number":40},"product-definition-section":{"template-number":1,"product-categories":{"timeExtent":"pointInTime","timeFormat":"None","spatialExtent":"None","processType":"None","processSubType":"ensemble","productCategory":"None","productSubCategory":"None"},"param-configurator":{"type":"paramId"},"model-configurator":{"type":"default"}, "ensemble-configurator":{"type":"default"},"point-in-time-configurator":{"type":"default"},"level-configurator":{"type":"meanSea"}},"data-representation-section":{"template-number":42}})json");
        case 165:
        case 166:
            return toSections(
                R"json({"indicator-section":{"template-number":0},"identification-section":{"template-number":0,"origin-configurator":{"type":"default"},"data-type-configurator":{"type":"default"},"reference-time-configurator":{"type":"default"},"tables-configurator":{"type":"default","local-tables-version":0}},"local-use-section":{"template-number":1},"grid-definition-section":{"template-number":40},"product-definition-section":{"template-number":1,"product-categories":{"timeExtent":"pointInTime","timeFormat":"None","spatialExtent":"None","processType":"None","processSubType":"ensemble","productCategory":"None","productSubCategory":"None"},"param-configurator":{"type":"paramId"},"model-configurator":{"type":"default"}, "ensemble-configurator":{"type":"default"},"point-in-time-configurator":{"type":"default"},"level-configurator":{"type":"heightAboveGroundAt10m","fixed-level":10}},"data-representation-section":{"template-number":42}})json");
        case 167:
        case 168:
            return toSections(
                R"json({"indicator-section":{"template-number":0},"identification-section":{"template-number":0,"origin-configurator":{"type":"default"},"data-type-configurator":{"type":"default"},"reference-time-configurator":{"type":"default"},"tables-configurator":{"type":"default","local-tables-version":0}},"local-use-section":{"template-number":1},"grid-definition-section":{"template-number":40},"product-definition-section":{"template-number":1,"product-categories":{"timeExtent":"pointInTime","timeFormat":"None","spatialExtent":"None","processType":"None","processSubType":"ensemble","productCategory":"None","productSubCategory":"None"},"param-configurator":{"type":"paramId"},"model-configurator":{"type":"default"}, "ensemble-configurator":{"type":"default"},"point-in-time-configurator":{"type":"default"},"level-configurator":{"type":"heightAboveGroundAt2m","fixed-level":2}},"data-representation-section":{"template-number":42}})json");
        case 136:
            return toSections(
                R"json({"indicator-section":{"template-number":0},"identification-section":{"template-number":0,"origin-configurator":{"type":"default"},"data-type-configurator":{"type":"default"},"reference-time-configurator":{"type":"default"},"tables-configurator":{"type":"default","local-tables-version":0}},"local-use-section":{"template-number":1},"grid-definition-section":{"template-number":40},"product-definition-section":{"template-number":1,"product-categories":{"timeExtent":"pointInTime","timeFormat":"None","spatialExtent":"None","processType":"None","processSubType":"ensemble","productCategory":"None","productSubCategory":"None"},"param-configurator":{"type":"paramId"},"model-configurator":{"type":"default"}, "ensemble-configurator":{"type":"default"},"point-in-time-configurator":{"type":"default"},"level-configurator":{"type":"entireAtmosphere"}},"data-representation-section":{"template-number":42}})json");
        case 143:
        case 228:
            return toSections(
                R"json({"indicator-section":{"template-number":0},"identification-section":{"template-number":0,"origin-configurator":{"type":"default"},"data-type-configurator":{"type":"default"},"reference-time-configurator":{"type":"default"},"tables-configurator":{"type":"default","local-tables-version":0}},"local-use-section":{"template-number":1},"grid-definition-section":{"template-number":40},"product-definition-section":{"template-number":11,"product-categories":{"timeExtent":"timeRange","timeFormat":"None","spatialExtent":"None","processType":"None","processSubType":"ensemble","productCategory":"None","productSubCategory":"None"},"param-configurator":{"type":"paramId"},"model-configurator":{"type":"default"}, "ensemble-configurator":{"type":"default"},"time-statistics-configurator":{"type":"since-beginning-of-forecast","type-of-statistical-processing":"accumul","overall-length-of-timerange":"1h"},"level-configurator":{"type":"surface"}},"data-representation-section":{"template-number":42}})json");
        case 129:
        case 130:
        case 131:
        case 132:
        case 133:
        case 135: {
            ASSERT(levelist.has());
            if (levelist.get() < 100) {
                return toSections(
                    R"json({"indicator-section":{"template-number":0},"identification-section":{"template-number":0,"origin-configurator":{"type":"default"},"data-type-configurator":{"type":"default"},"reference-time-configurator":{"type":"default"},"tables-configurator":{"type":"default","local-tables-version":0}},"local-use-section":{"template-number":1},"grid-definition-section":{"template-number":40},"product-definition-section":{"template-number":1,"product-categories":{"timeExtent":"pointInTime","timeFormat":"None","spatialExtent":"None","processType":"None","processSubType":"ensemble","productCategory":"None","productSubCategory":"None"},"param-configurator":{"type":"paramId"},"model-configurator":{"type":"default"}, "ensemble-configurator":{"type":"default"},"point-in-time-configurator":{"type":"default"},"level-configurator":{"type":"isobaricInPa"}},"data-representation-section":{"template-number":42}})json");
            }
            else {
                return toSections(
                    R"json({"indicator-section":{"template-number":0},"identification-section":{"template-number":0,"origin-configurator":{"type":"default"},"data-type-configurator":{"type":"default"},"reference-time-configurator":{"type":"default"},"tables-configurator":{"type":"default","local-tables-version":0}},"local-use-section":{"template-number":1},"grid-definition-section":{"template-number":40},"product-definition-section":{"template-number":1,"product-categories":{"timeExtent":"pointInTime","timeFormat":"None","spatialExtent":"None","processType":"None","processSubType":"ensemble","productCategory":"None","productSubCategory":"None"},"param-configurator":{"type":"paramId"},"model-configurator":{"type":"default"}, "ensemble-configurator":{"type":"default"},"point-in-time-configurator":{"type":"default"},"level-configurator":{"type":"isobaricInhPa"}},"data-representation-section":{"template-number":42}})json");
            }
        }
        default:
            std::ostringstream oss;
            oss << "aifsSingleConfForParam: no mapping for param: " << param;
            throw mars2grib::Mars2GribException(oss.str(), Here());
    }
}

}  // namespace multio::test
