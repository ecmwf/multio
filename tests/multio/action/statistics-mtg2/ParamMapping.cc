
#include "eckit/testing/Test.h"

#include "../../MultioTestEnvironment.h"
#include "StatisticsTestHelpers.h"

namespace multio::test::statistics_mtg2 {

using multio::message::Message;
using multio::message::Metadata;
using multio::test::MultioTestEnvironment;

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

// Accumulated parameters, accumulated under their own paramId (levtype sfc); input and output paramId are identical
std::vector<StatisticsParamMapping> accumulationMappings = {
    // Water, kg m**-2
    {228228, "accumulate", 228228},
    {228143, "accumulate", 228143},
    {3062, "accumulate", 3062},
    {228144, "accumulate", 228144},
    {260259, "accumulate", 260259},
    {231002, "accumulate", 231002},
    {231010, "accumulate", 231010},
    {231012, "accumulate", 231012},

    // Energy, J m**-2
    {146, "accumulate", 146},
    {147, "accumulate", 147},
    {176, "accumulate", 176},
    {177, "accumulate", 177},
    {169, "accumulate", 169},
    {175, "accumulate", 175},
    {178, "accumulate", 178},
    {179, "accumulate", 179},
    {212, "accumulate", 212},

    // Stress, N m**-2 s
    {180, "accumulate", 180},
    {181, "accumulate", 181},
};

std::string makePlan(std::int64_t param, const std::string& op, bool strict,
                     const std::string& outputFrequency = "1h") {
    return "{ \"name\": \"statistics_param_mapping_" + std::to_string(param) + "_" + op
         + "_test\", "
           "\"actions\": [ { "
           "\"type\": \"statistics-mtg2\", "
           "\"output-frequency\": \""
         + outputFrequency
         + "\", "
           "\"operations\": [ \""
         + op
         + "\" ], "
           "\"options\": { \"initial-condition-present\": \"true\", \"disable-strict-mapping\": \""
         + std::string(strict ? "false" : "true")
         + "\" } },"
           "{ \"type\": \"debug-sink\" } ] }";
}

// Send step 0 and 1 messages with the input parameter ID
void sendStepZeroAndOne(MultioTestEnvironment& env, std::int64_t param) {
    for (int64_t step = 0; step <= 1; ++step) {
        auto md = Metadata({{"param", param},
                            {"levtype", "sfc"},
                            {"grid", "O80"},
                            {"date", 20200721},
                            {"time", 0000},
                            {"step", step},
                            {"misc-precision", "double"}});
        setTimingMetadata(md, 3600);
        auto pl = eckit::Buffer();
        auto msg = Message({Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl));
        EXPECT_NO_THROW(env.process(std::move(msg)));
    }
}

// The mapping is applied when the statistics message is emitted; with strict mapping on, an undefined mapping throws.
bool statisticsMappingIsRejected(std::int64_t param, const std::string& op) {
    auto env = MultioTestEnvironment(makePlan(param, op, true));
    sendSimulationStart(env, 20200721);
    sendStepZeroAndOne(env, param);
    try {
        env.process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}});
    }
    catch (const eckit::Exception& e) {
        // The failure-aware action wraps the exception: look for the mapping error in the nested message
        std::ostringstream os;
        os << e;
        return os.str().find("is undefined!") != std::string::npos;
    }
    return false;
}

std::int64_t testParameterMapping(std::int64_t param, std::string op, bool strict = false) {
    auto env = MultioTestEnvironment(makePlan(param, op, strict));
    EXPECT_EQUAL(env.debugSink().size(), 0);
    sendSimulationStart(env, 20200721);

    sendStepZeroAndOne(env, param);
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

// The mappings above are applied with strict mapping on, i.e. without `disable-strict-mapping`
CASE("statistics parameter mappings of the IFS accumulated parameters, strict mapping") {
    for (StatisticsParamMapping mapping : accumulationMappings) {
        std::int64_t inParam = std::get<0>(mapping);
        std::string inOp = std::get<1>(mapping);
        std::int64_t outParam = std::get<2>(mapping);

        std::ostringstream os;
        os << "strict param=" << inParam << ", operation=" << inOp << " -> param=" << outParam;

        SECTION(os.str()) {
            EXPECT_EQUAL(testParameterMapping(inParam, inOp, true), outParam);
        }
    }
}

// The accumulated parameters are only mapped for accumulation, and only under their own paramId: other operations and
// the legacy IFS paramIds in metres (143, 142, ...) stay undefined.
CASE("statistics parameter mappings of the accumulated parameters, unmapped combinations are rejected") {
    const std::vector<StatisticsParamMapping> rejected = {
        {228143, "average", 0}, {146, "average", 0},    {180, "average", 0},
        {143, "accumulate", 0}, {142, "accumulate", 0}, {228, "accumulate", 0},
    };
    for (const auto& [param, op, ignored] : rejected) {
        std::ostringstream os;
        os << "strict param=" << param << ", operation=" << op;

        SECTION(os.str()) {
            EXPECT(statisticsMappingIsRejected(param, op));
        }
    }
}

// The accumulation is the plain sum of the increments, in the units of the input: nothing is converted.
CASE("statistics accumulation of the accumulated parameters keeps the units of the increments") {
    for (std::int64_t param : {228143, 3062, 146, 180}) {
        SECTION("strict param=" + std::to_string(param)) {
            auto env = MultioTestEnvironment(makePlan(param, "accumulate", true, "3h"));
            sendSimulationStart(env, 20200721);

            // Step 0 is the anchor of the window (initial condition); steps 1..3 carry the increments 1, 2 and 3
            for (std::int64_t step = 0; step <= 3; ++step) {
                auto md = Metadata({{"param", param},
                                    {"levtype", "sfc"},
                                    {"grid", "O80"},
                                    {"date", 20200721},
                                    {"time", 0000},
                                    {"step", step},
                                    {"misc-precision", "double"}});
                setTimingMetadata(md, 3600);
                auto data = static_cast<double>(step);
                auto pl = eckit::Buffer{&data, sizeof(double)};
                EXPECT_NO_THROW(env.process(Message({Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl))));
            }

            EXPECT_NO_THROW(env.process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}}));
            EXPECT_EQUAL(env.debugSink().size(), 2);

            EXPECT_EQUAL(sizeof(double), env.debugSink().front().size());
            auto sum = static_cast<const double*>(env.debugSink().front().payload().data());
            EXPECT_EQUAL(1.0 + 2.0 + 3.0, *sum);
            EXPECT_EQUAL(param, env.debugSink().front().metadata().get<std::int64_t>("param"));
        }
    }
}

}  // namespace multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
