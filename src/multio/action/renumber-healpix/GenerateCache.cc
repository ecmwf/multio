#include <iostream>

#include <algorithm>
#include <array>
#include <fstream>
#include <functional>
#include <iostream>
#include <iterator>
#include <regex>
#include <sstream>
#include <string>
#include <vector>


#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/io/FileHandle.h"
#include "eckit/io/PeekHandle.h"
#include "eckit/io/StdFile.h"
#include "eckit/log/Log.h"
#include "eckit/maths/Functions.h"
#include "eckit/message/Decoder.h"
#include "eckit/message/Message.h"
#include "eckit/message/Reader.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "eckit/option/VectorOption.h"
#include "eckit/value/Value.h"


#include "HEALPix.h"
#include "multio/ifsio/ifsio.h"
#include "multio/tools/MultioTool.h"

#include "eckit/codec/codec.h"

namespace multio::action {

namespace {

std::vector<size_t> split(const std::string& gstr) {
    std::string str{gstr};
    int lo = str.find("[");
    int hi = str.find("]");
    str = str.substr(lo + 1, hi - 1);
    std::string token = ",";
    std::vector<size_t> result;
    while (str.size()) {
        int index = str.find(token);
        if (index != std::string::npos) {
            std::string tmp = str.substr(0, index);
            tmp.erase(remove_if(tmp.begin(), tmp.end(), isspace), tmp.end());
            result.push_back(stoul(tmp));
            str = str.substr(index + token.size());
        }
        else {
            std::string tmp = str;
            tmp.erase(remove_if(tmp.begin(), tmp.end(), isspace), tmp.end());
            result.push_back(stoul(tmp));
            str = "";
        }
    }
    return result;
}

}  // namespace


class CacheGenerator final : public multio::MultioTool {
public:  // methods
    CacheGenerator(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
        eckit::Log::info() << "EXAMPLE: " << std::endl
                           << "multio-generate-healpix-cache --output=ring2nest.atlas --from=2 --to=16 " << std::endl
                           << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs&) override;

    void execute(const eckit::option::CmdArgs& args) override;

    void loadTriplets();

    // bool subtocExists() const;

    int numberOfPositionalArguments() const override { return 0; }
    int minimumPositionalArguments() const override { return 0; }

    std::string output_;
    size_t from_;
    size_t to_;
    size_t by_;

    std::vector<size_t> list_;
};


CacheGenerator::CacheGenerator(int argc, char** argv) :
    multio::MultioTool{argc, argv}, output_{"./HEALPix_ring2nest.atlas"}, from_{0}, to_{10}, by_{1}, list_(0) {

    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "output", "WorkingMode [fromTriplets|fromAtlasIO]. Default( \"fromTriplets\" )"));
    options_.push_back(
        new eckit::option::SimpleOption<size_t>("from", "Path of the input files with the triplets. Default( \".\" )"));
    options_.push_back(new eckit::option::SimpleOption<size_t>(
        "to", "ordering convention used to create the triplets. Default(\"ring\")"));
    options_.push_back(new eckit::option::SimpleOption<size_t>(
        "by", "ordering convention used to create the triplets. Default(\"ring\")"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "list", "ordering convention used to create the triplets. Default(\"ring\")"));
}


void CacheGenerator::init(const eckit::option::CmdArgs& args) {

    std::string tmp;

    args.get("output", output_);
    args.get("from", from_);
    args.get("to", to_);
    args.get("by", by_);
    args.get("list", tmp);

    if (tmp.size() == 0) {
        for (int i = from_; i <= to_; i += by_) {
            list_.push_back(i);
        }
    }
    else {
        list_ = split(tmp);
    }
}

void CacheGenerator::execute(const eckit::option::CmdArgs& args) {
    size_t ref = 1;
    eckit::codec::RecordWriter record;
    record.compression("none");
    // for ( size_t i=from_; i<=to_; ++i ){
    for (size_t i = 0; i < list_.size(); ++i) {
        size_t Nside = ref << list_[i];
        HEALPix Idx(static_cast<int>(Nside));
        std::vector<size_t> map(Nside * Nside * 12, 0);
        for (size_t j = 0; j < Nside * Nside * 12; ++j) {
            map[j] = static_cast<size_t>(Idx.ring_to_nest(static_cast<int>(j)));
        }
        std::ostringstream os;
        os << "H" << std::setfill('0') << std::setw(8) << Nside << "_ring2nest";
        record.set(os.str(), map);
    }
    record.write(output_);
};


void CacheGenerator::finish(const eckit::option::CmdArgs&) {}

}  // namespace multio::action


int main(int argc, char** argv) {
    multio::action::CacheGenerator tool(argc, argv);
    return tool.start();
}
