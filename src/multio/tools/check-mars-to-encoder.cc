
#include <sstream>
#include <unordered_set>
#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/JSONConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/LocalPathName.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/log/CodeLocation.h"
#include "eckit/log/JSON.h"
#include "eckit/option/CmdArgs.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/core/EntryParser.h"
#include "multio/datamod/core/EntryDumper.h"
#include "multio/datamod/core/Record.h"
#include "multio/mars2grib/Rules.h"
#include "multio/tools/MultioTool.h"


bool compareLocalConfig(const eckit::LocalConfiguration& lhs, const eckit::LocalConfiguration& rhs) {
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

    for (const auto& k : lhsKeys) {
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
            if (!compareLocalConfig(rhs.getSubConfiguration(k), lhs.getSubConfiguration(k))) {
                return false;
            }
        }
        else {
            std::cout << "Unhandled type for key " << k << " - lhs: " << lhs << " rhs: " << rhs;
            return false;
        }
    }
    return true;
}


class CheckMarsToEncoder : public multio::MultioTool {
public:
    CheckMarsToEncoder(int argc, char** argv) : multio::MultioTool(argc, argv) {

    }

    void usage(const std::string& tool) const override {
        std::cout << "\nUsage: " << tool << " inputFile" << std::endl;
    }

private:
    void init(const eckit::option::CmdArgs& args) override {}

    void execute(const eckit::option::CmdArgs& args) override {
        eckit::JSON json{std::cout};

        const eckit::LocalConfiguration testCases{eckit::JSONConfiguration{eckit::PathName(args(0))}};
        std::cout << "Loaded " << testCases.getSubConfigurations().size() << " test cases!" << std::endl;

        size_t count = 0;
        size_t failed = 0;
        for (const auto& testCase : testCases.getSubConfigurations()) {
            count++;

            const auto& mars = testCase.getSubConfiguration("mars");
            const auto& expectedEncoder = testCase.getSubConfiguration("encoder-config");

            const auto marsKeys = multio::datamod::readRecordByValue<multio::datamod::FullMarsRecord>(mars);

            const auto encoderConf = multio::mars2grib::rules::buildEncoderConf(marsKeys);
            const auto actualEncoder = multio::datamod::dumpRecord<eckit::LocalConfiguration>(encoderConf);

            if (!compareLocalConfig(expectedEncoder, actualEncoder)) {
                std::cout << "==================== FAILURE! ====================" << std::endl;
                json << testCase;
                std::cout << "\n==================================================" << std::endl;
                failed++;
            }
        }

        std::ostringstream oss;
        oss << "Failed " << failed << " cases failed out of " << count;
        std::cout << oss.str();
        if (failed != 0) {
            throw eckit::Exception(oss.str(), Here());
        }
    }

    void finish(const eckit::option::CmdArgs& args) override {}

    int minimumPositionalArguments() const override { return 1; }
};


int main(int argc, char** argv) {
    CheckMarsToEncoder tool{argc, argv};
    return tool.start();
}
