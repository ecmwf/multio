#include "multio/message/Metadata.h"


namespace multio::util::sample_gen {

namespace aifs {

auto mkAifsSingleBaseMd() {
    return message::Metadata{
        {"class", "ai"},  {"model", "aifs-single"}, {"expver", "0001"}, {"type", "fc"},   {"stream", "oper"},
        {"repres", "gg"}, {"packing", "ccsds"},     {"levelist", 700},  {"grid", "N320"}, {"step", 6},
        {"time", 0},      {"date", 20230901},       {"levtype", "pl"},  {"param", 133}};
}


std::vector<message::Metadata> aifsSingleParams() {
    std::vector<message::Metadata> res;

    // SFC params
    for (auto param : std::vector<int>{{134, 151, 165, 166, 167, 168, 235, 141, 136}}) {
        res.push_back({{"param", param}, {"levtype", "sfc"}});
    }
    
    // SFC params stat
    for (auto param : std::vector<int>{{143, 228}}) {
        // TODO(pgeier) don't know if the timespan is right - added to fix test
        res.push_back({{"param", param}, {"levtype", "sfc"}, {"timespan", "1h"}});
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

}  // namespace aifs

std::vector<message::Metadata> mkAifsSingleMd() {
    using namespace aifs;
    return combineWithBaseMd(addSteps(aifsSingleParams()));
}
std::vector<message::Metadata> mkAifsEnsMd() {
    using namespace aifs;
    return combineWithBaseMd(addSteps(aifsEnsParams()));
}

auto mkMd() {
    using namespace aifs;
    return mkAifsSingleBaseMd();
}


}  // namespace multio::util::sample_gen
