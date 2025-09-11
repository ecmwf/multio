/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/testing/Test.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/core/Record.h"
#include "multio/mars2grib/LegacyEncoderConf.h"
#include "multio/util/Substitution.h"

#include "multio/LibMultio.h"

#include <unordered_set>


namespace multio::test {

namespace dm = multio::datamod;

eckit::PathName encodeRulesPath() {
    const auto root = multio::LibMultio::instance().libraryHome();
    setenv("IFS_INSTALL_DIR", root.c_str(), 0);

    return root + "/share/multiom/encodings/encoding-rules.yaml";
}

struct LoadedConf {
    eckit::PathName file;
    eckit::LocalConfiguration conf;
};


bool compareLocalConf(const eckit::LocalConfiguration& lhs, const eckit::LocalConfiguration& rhs) {
    auto lhsKeysVector = lhs.keys();
    auto rhsKeysVector = rhs.keys();
    std::unordered_set<std::string> lhsKeys{lhsKeysVector.begin(), lhsKeysVector.end()},
        rhsKeys{rhsKeysVector.begin(), rhsKeysVector.end()};

    if (lhsKeys.size() != rhsKeys.size()) {
        std::cout << "Different number of keys: " << lhsKeysVector << " vs " << rhsKeysVector;
        return false;
    }
    for (const auto& k : lhsKeys) {
        if (auto search = rhsKeys.find(k); search == rhsKeys.end()) {
            std::cout << "Keys " << k << " not given in rhs: " << rhsKeysVector;
            return false;
        }
    }

    // Now iterate all keys and compare values
    for (const auto& k : lhsKeys) {
        // Using most ugly "reflection" possible
        if (lhs.isString(k)) {
            if (!rhs.isString(k)) {
                std::cout << "Keys " << k << " is String but rhs is not " << rhs;
                return false;
            }
            auto lhsVal = lhs.getString(k);
            auto rhsVal = rhs.getString(k);
            if (lhsVal != rhsVal) {
                std::cout << "Values for key " << k << " differ: " << lhsVal << " != " << rhsVal << std::endl;
                return false;
            }
        }
        else if (lhs.isBoolean(k)) {
            if (!rhs.isBoolean(k)) {
                std::cout << "Keys " << k << " is Boolean but rhs is not " << rhs;
                return false;
            }
            auto lhsVal = lhs.getBool(k);
            auto rhsVal = rhs.getBool(k);
            if (lhsVal != rhsVal) {
                std::cout << "Values for key " << k << " differ: " << lhsVal << " != " << rhsVal << std::endl;
                return false;
            }
        }
        else if (lhs.isFloatingPoint(k)) {
            if (!rhs.isFloatingPoint(k)) {
                std::cout << "Keys " << k << " is FloatingPoint but rhs is not " << rhs;
                return false;
            }
            auto lhsVal = lhs.getDouble(k);
            auto rhsVal = rhs.getDouble(k);
            if (lhsVal != rhsVal) {
                std::cout << "Values for key " << k << " differ: " << lhsVal << " != " << rhsVal << std::endl;
                return false;
            }
        }
        else if (lhs.isIntegral(k)) {
            if (!rhs.isIntegral(k)) {
                std::cout << "Keys " << k << " is Integral but rhs is not " << rhs;
                return false;
            }
            auto lhsVal = lhs.getInt64(k);
            auto rhsVal = rhs.getInt64(k);
            if (lhsVal != rhsVal) {
                std::cout << "Values for key " << k << " differ: " << lhsVal << " != " << rhsVal << std::endl;
                return false;
            }
        }
        else if (lhs.isSubConfiguration(k)) {
            if (!rhs.isSubConfiguration(k)) {
                std::cout << "Keys " << k << " is subConfiguration but rhs is not " << rhs;
                return false;
            }
            if (!compareLocalConf(rhs.getSubConfiguration(k), lhs.getSubConfiguration(k))) {
                return false;
            }
        }
        else {
            std::cout << "Unhandled type for key " << k << " - lhs: " << lhs << " rhs: " << rhs;
            return false;
        }
    }

    // if (lhs.get() == rhs.get()) {
    //     return true;
    // }

    // eckit::JSON json(std::cout);
    // std::cout << "LHS: \n";
    // json << lhs;
    // std::cout << "\n\t != RHS: \n";
    // json << rhs;
    return true;
};

std::vector<LoadedConf> loadRuleFiles() {
    auto rules = eckit::LocalConfiguration{eckit::YAMLConfiguration{encodeRulesPath()}};
    if (!rules.has("encoding-rules")) {
        std::ostringstream oss;
        oss << "No key \"encoding-rules\" in rules file " << rules << std::endl;
        throw eckit::Exception(oss.str(), Here());
    }
    auto subConf = rules.getSubConfigurations("encoding-rules");

    std::vector<LoadedConf> ret;
    ret.reserve(subConf.size());

    for (auto& sc : subConf) {
        eckit::PathName file = util::replaceCurly(sc.getString("file"));
        eckit::LocalConfiguration conf = eckit::LocalConfiguration{eckit::YAMLConfiguration{file}};
        ret.push_back({std::move(file), std::move(conf)});
    }
    return ret;
}


CASE("Test parsing and comparing rule files") {
    for (const auto& loadedRule : loadRuleFiles()) {
        using namespace datamod;
        std::cout << "Parsing and comparing rule: " << loadedRule.file << std::endl;

        auto encoderConf = loadedRule.conf.getSubConfiguration("encoder");
        auto sections = dm::readRecord<mars2grib::LegacySectionsConf>(encoderConf);
        // Delete key "type" because we don't consider it
        // encoderConf.remove("type");

        auto rewriteConf = dm::dumpRecord<eckit::LocalConfiguration>(sections);
        EXPECT(compareLocalConf(encoderConf, rewriteConf));
    }
};

}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
