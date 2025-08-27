#include "eckit/config/YAMLConfiguration.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/mars2grib/EncoderConf.h"
#include "multio/mars2grib/Mars2GribException.h"

#include <sstream>

namespace multio::test {

namespace dm = multio::datamod;

mars2grib::SectionsConf expectedAIFSSingleEncoderSections(const dm::FullMarsRecord& mars) {
    ASSERT(mars.param.isSet());

    auto toSections = [](std::string&& str) {
        return dm::readRecord<mars2grib::SectionsConf>(
            eckit::LocalConfiguration{eckit::YAMLConfiguration{std::move(str)}});
    };
    switch (mars.param.get()) {
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
                R"json({"indicator-section":{"template-number":0},"identification-section":{"template-number":0,"origin-configurator":{"type":"default"},"data-type-configurator":{"type":"default"},"reference-time-configurator":{"type":"default"},"tables-configurator":{"type":"default","local-tables-version":0}},"local-use-section":{"template-number":1},"grid-definition-section":{"template-number":40},"product-definition-section":{"template-number":8,"product-categories":{"timeExtent":"timeRange","timeFormat":"None","spatialExtent":"None","processType":"None","processSubType":"None","productCategory":"None","productSubCategory":"None"},"param-configurator":{"type":"paramId"},"model-configurator":{"type":"default"},"time-statistics-configurator":{"type":"since-beginning-of-forecast","type-of-statistical-processing":"accumul"},"level-configurator":{"type":"surface"}},"data-representation-section":{"template-number":42}})json");
        case 129:
        case 130:
        case 131:
        case 132:
        case 133:
        case 135: {
            ASSERT(mars.levelist.isSet());
            if (mars.levelist.get() < 100) {
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
            util::PrintStream ps(oss);
            ps << "aifsSingleConfForParam: no mapping for param: " << mars.param;
            throw mars2grib::Mars2GribException(oss.str(), Here());
    }
}


mars2grib::SectionsConf expectedAIFSEnsEncoderSections(const dm::FullMarsRecord& mars) {
    ASSERT(mars.param.isSet());

    auto toSections = [](std::string&& str) {
        return dm::readRecord<mars2grib::SectionsConf>(
            eckit::LocalConfiguration{eckit::YAMLConfiguration{std::move(str)}});
    };
    switch (mars.param.get()) {
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
                R"json({"indicator-section":{"template-number":0},"identification-section":{"template-number":0,"origin-configurator":{"type":"default"},"data-type-configurator":{"type":"default"},"reference-time-configurator":{"type":"default"},"tables-configurator":{"type":"default","local-tables-version":0}},"local-use-section":{"template-number":1},"grid-definition-section":{"template-number":40},"product-definition-section":{"template-number":11,"product-categories":{"timeExtent":"timeRange","timeFormat":"None","spatialExtent":"None","processType":"None","processSubType":"ensemble","productCategory":"None","productSubCategory":"None"},"param-configurator":{"type":"paramId"},"model-configurator":{"type":"default"}, "ensemble-configurator":{"type":"default"},"time-statistics-configurator":{"type":"since-beginning-of-forecast","type-of-statistical-processing":"accumul"},"level-configurator":{"type":"surface"}},"data-representation-section":{"template-number":42}})json");
        case 129:
        case 130:
        case 131:
        case 132:
        case 133:
        case 135: {
            ASSERT(mars.levelist.isSet());
            if (mars.levelist.get() < 100) {
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
            util::PrintStream ps(oss);
            ps << "aifsSingleConfForParam: no mapping for param: " << mars.param;
            throw mars2grib::Mars2GribException(oss.str(), Here());
    }
}

}  // namespace multio::test
