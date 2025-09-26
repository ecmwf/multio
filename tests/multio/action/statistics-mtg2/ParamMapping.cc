
#include "eckit/testing/Test.h"

#include "../../MultioTestEnvironment.h"

namespace multio::test::statistics_mtg2 {

using multio::test::MultioTestEnvironment;
using multio::message::Message;
using multio::message::Metadata;

// param + operation -> param
using StatisticsParamMapping = std::tuple<std::int64_t, std::string, std::int64_t>;

std::vector<StatisticsParamMapping> mappings = {
    // ClimateDT --- hourly/daily/monthly means
    // Commented out mappings are (currently) not supported!

    // Atmosphere (levtype sfc)
    {78, "average", 235087},
    {79, "average", 235088},
    {134, "average", 235134},
    {136, "average", 235136},
    {137, "average", 235137},
    {228141, "average", 235078},
    {151, "average", 235151},
    {228164, "average", 235288},
    {165, "average", 235165},
    {166, "average", 235166},
    {167, "average", 228004},
    {168, "average", 235168},
    {207, "average", 228005},
    {235, "average", 235079},
    // {8, "fixed-window-flux-average", 235020},  // 8 -> 231010 -> (231009) -> 235020
    // {9, "fixed-window-flux-average", 235021},
    // {144, "fixed-window-flux-average", 235031},
    // {146, "average", 235033},
    // {147, "average", 235034},
    // {169, "average", 235035},
    // {175, "average", 235036},
    // {176, "average", 235037},
    // {177, "average", 235038},
    // {178, "average", 235039},
    // {179, "average", 235040},
    // {180, "average", 235041},
    // {260654, "average", 235041},  // ICON
    // {181, "average", 235042},
    // {260665, "average", 235042},  // ICON
    // {182, "fixed-window-flux-average", 235043},
    // {208, "average", 235049},
    // {209, "average", 235050},
    // {210, "average", 235051},
    // {211, "average", 235052},
    // {212, "average", 235053},
    // {228, "fixed-window-flux-average", 235055},

    // Snow (levtype sol)
    {228141, "average", 235078},

    // Soil (levtype sol)
    {260199, "average", 235077},

    // Sea ice (levtype o2d) --- original param not verified
    // Ocean (levtype o2d) --- original param not verified

    // Fields on pressure levels (levtype pl)
    {60, "average", 235100},
    {129, "average", 235129},
    {130, "average", 235130},
    {131, "average", 235131},  // Also on levtype hl 100, 150
    {132, "average", 235132},  // Also on levtype hl 100, 150
    {133, "average", 235133},
    {135, "average", 235135},
    {157, "average", 235157},
    {246, "average", 235246}

    // Ocean (levtype o3d) --- original param not verified
};

std::int64_t testParameterMapping(std::int64_t param, std::string op) {
    const auto plan =
        "{ \"name\": \"statistics_param_mapping_" + std::to_string(param) + "_" + op + "_test\", "
        "\"actions\": [ { "
        "\"type\": \"statistics-mtg2\", "
        "\"output-frequency\": \"1h\", "
        "\"operations\": [ \"" + op + "\" ], "
        "\"options\": { \"initial-condition-present\": \"true\", \"disable-strict-mapping\": \"true\" } },"
        "{ \"type\": \"debug-sink\" } ] }";

    auto env = MultioTestEnvironment(plan);
    EXPECT_EQUAL(env.debugSink().size(), 0);

    // Send step 0 and 1 message with the input parameter ID
    for (int64_t step = 0; step <= 1; ++step) {
        auto md = Metadata({
            {"param", param},
            {"levtype", "none"},
            {"grid", "none"},
            {"startDate", 20200721},
            {"startTime", 0000},
            {"step", step},
            {"misc-precision", "double"}
        });
        auto pl = eckit::Buffer();
        auto msg = Message({Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl));
        EXPECT_NO_THROW(env.process(std::move(msg)));
    }
    EXPECT_EQUAL(env.debugSink().size(), 0);

    // Send a flush last-step to trigger emitting the statistics message
    EXPECT_NO_THROW(env.process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}}));
    EXPECT_EQUAL(env.debugSink().size(), 2);

    // Return the output parameter ID of the statistics message
    return env.debugSink().front().metadata().get<std::int64_t>("param");
}

CASE("statistics parameter mappings") {
    for (StatisticsParamMapping mapping : mappings) {
        std::int64_t inParam = std::get<0>(mapping);
        std::string inOp = std::get<1>(mapping);
        std::int64_t outParam = std::get<2>(mapping);

        std::ostringstream os;
        os << "param=" << inParam << ", operation=" << inOp << " -> param=" << outParam;

        SECTION(os.str()) {
            EXPECT_EQUAL(testParameterMapping(inParam, inOp), outParam);
        }
    }
}

}  // multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
