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
#include "multio/action/encode-mtg2/EncodeMtg2Exception.h"
#include "multio/action/encode-mtg2/EncoderConf.h"
#include "multio/action/encode-mtg2/Options.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/DataModelling.h"
#include "multio/util/Substitution.h"

#include <unordered_set>


namespace multio::test {

action::EncodeMtg2Conf getConf() {
    using namespace datamod;

    action::EncodeMtg2Conf opts;
    datamod::alterAndValidate(opts);

    auto& root = key<datamod::EncodeMtg2Def::KnowledgeRoot>(opts).get();
    key<EncodeMtg2Def::EncodingRules>(opts).set(root + "/share/multiom/encodings/encoding-rules.yaml");

    setenv("IFS_INSTALL_DIR", std::string(root).c_str(), 0);

    return opts;
};

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
        } else {
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

std::vector<LoadedConf> loadRuleFiles(const action::EncodeMtg2Conf& opts) {
    using namespace datamod;
    auto rules = eckit::LocalConfiguration{eckit::YAMLConfiguration{key<EncodeMtg2Def::EncodingRules>(opts).get()}};
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
    for (const auto& loadedRule : loadRuleFiles(getConf())) {
        using namespace datamod;
        std::cout << "Parsing and comparing rule: " << loadedRule.file << std::endl;

        auto encoderInf = read(action::EncoderInfoKeySet{}, loadedRule.conf);
        
        auto readConf = loadedRule.conf.getSubConfiguration("encoder");
        // Delete key "type" because we don't consider it
        readConf.remove("type");

        auto rewriteConf = write<eckit::LocalConfiguration>(key<EncoderInfoDef::Sections>(encoderInf).get());
        EXPECT(compareLocalConf(readConf, rewriteConf));
    }
};

}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
