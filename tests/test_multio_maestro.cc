/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */
#include "multio/maestro/MaestroCdo.h"
#include "multio/maestro/MaestroSelector.h"
#include "multio/maestro/MaestroSubscription.h"
#include "multio/maestro/MaestroStatistics.h"

#include <chrono>
#include "unistd.h"
#include "eckit/testing/Test.h"

namespace multio {
namespace test {

class TestHarness {
public:
    TestHarness() : size_{822329}, data_(size_, 'A') {
        mstro_init("Testing workflow", "Test Component", 0);
    }
    ~TestHarness() {
        mstro_finalize();
    }
    uint64_t size() { return size_; }
    const void* data() { return static_cast<const void*>(data_.c_str()); }
    MaestroStatistics statistics_;
private:
    uint64_t size_;
    std::string data_;
};

CASE("CDO construction") {

    TestHarness test;

    // only name
    EXPECT_NO_THROW(MaestroCdo("name", test.statistics_));

    // long name
    EXPECT_NO_THROW(MaestroCdo(std::string(1000, 'A'), test.statistics_));

    // name, data and size
    EXPECT_NO_THROW(MaestroCdo("name", test.data(), test.size(), test.statistics_));

    // move section
    MaestroCdo cdo1{"name", test.data(), test.size(), test.statistics_};
    
    // move constructor
    MaestroCdo cdo2{std::move(cdo1)};

    std::string data = std::string(static_cast<const char*>(cdo2.data())).substr(0, cdo2.size());
    EXPECT(data.compare(static_cast<const char*>(test.data())) == 0);
    EXPECT(cdo1.data() == nullptr);

    EXPECT(cdo2.size() == test.size());
    EXPECT(cdo1.size() == 0);

    // move assignment constructor
    MaestroCdo cdo3 = std::move(cdo2);

    data = std::string(static_cast<const char*>(cdo3.data()));
    EXPECT(data.compare(static_cast<const char*>(test.data())) == 0);
    EXPECT(cdo2.data() == nullptr);

    EXPECT(cdo3.size() == test.size());
    EXPECT(cdo2.size() == 0);
}

CASE("Set and get CDO attributes") {

    TestHarness test;

    MaestroCdo cdo{"name", test.statistics_};

    int64_t step{2};
    cdo.set_attribute<int64_t>(".maestro.ecmwf.step", step, true);  // int()

    uint64_t time{12};
    cdo.set_attribute<uint64_t>(".maestro.ecmwf.time", time, true);  // uint()

    std::string expver{"0001"};
    cdo.set_attribute<const char*>(".maestro.ecmwf.expver", expver.c_str(), true);  // str()

    std::time_t now = std::time(NULL);
    auto seconds = static_cast<int>(std::chrono::seconds(now).count());
    auto nanoseconds = static_cast<int>(std::chrono::nanoseconds(now).count());
    mstro_timestamp date{seconds, nanoseconds, 0};
    cdo.set_attribute<mstro_timestamp>(".maestro.ecmwf.date", date, true);  // timestamp()

    // it takes some time to copy attribute values
    // we should do a short sleep before getting the attribute values
    // cdo.seal() does not help
    ::sleep(1);

    EXPECT(cdo.get_attribute<int64_t>(".maestro.ecmwf.step") == step);
    EXPECT(cdo.get_attribute<uint64_t>(".maestro.ecmwf.time") == time);
    EXPECT(expver.compare(cdo.get_attribute<const char*>(".maestro.ecmwf.expver")) == 0);
    mstro_timestamp getdate = cdo.get_attribute<mstro_timestamp>(".maestro.ecmwf.date");
    EXPECT(getdate.sec == date.sec);
    EXPECT(getdate.nsec == date.nsec);
    EXPECT(getdate.offset == date.offset);
}

CASE("Maestro CDO get size and data") {

    TestHarness test;

    MaestroCdo cdo1{"nodata", test.statistics_};
    EXPECT(cdo1.size() == 0);
    EXPECT(cdo1.data() == nullptr);

    MaestroCdo cdo2{"name", test.data(), test.size(), test.statistics_};
    std::string data = std::string(static_cast<const char*>(cdo2.data())).substr(0, cdo2.size());
    EXPECT(cdo2.size() == test.size());
    EXPECT(data.compare(static_cast<const char*>(test.data())) == 0);
}

CASE("Maestro core CDO operation") {

    TestHarness test;

    MaestroCdo cdo{"name", test.statistics_};
    EXPECT_NO_THROW(cdo.seal());
    EXPECT_NO_THROW(cdo.offer());
    EXPECT_NO_THROW(cdo.withdraw());

    MaestroCdo otherCdo{"other", test.statistics_};
    otherCdo.require();
    otherCdo.retract();
}

CASE("Maestro core CDO operations with local transfer") {

    TestHarness test;

    int64_t step{2};
    std::string stream{"oper"};

    MaestroCdo sourceCdo{"name", test.statistics_};
    sourceCdo.set_attribute<int64_t>(".maestro.ecmwf.step", step, true);
    sourceCdo.set_attribute<const char*>(".maestro.ecmwf.stream", stream.c_str(), true);
    sourceCdo.seal();
    sourceCdo.offer();

    MaestroCdo destCdo{"name", test.statistics_};
    destCdo.require();
    destCdo.demand();

    EXPECT(destCdo.get_attribute<int64_t>(".maestro.ecmwf.step") == step);
    EXPECT(stream.compare(destCdo.get_attribute<const char*>(".maestro.ecmwf.stream")) == 0);

    sourceCdo.withdraw();
}

CASE("Selector and subscription") {

    TestHarness test;

    MaestroSelector selector1{"(has .maestro.ecmwf.step)", test.statistics_, nullptr, nullptr};
    MaestroSelector selector2{"(has .maestro.ecmwf.step)", test.statistics_};

    MaestroSubscription subscription = selector2.subscribe(MSTRO_POOL_EVENT_DECLARE|MSTRO_POOL_EVENT_OFFER, MSTRO_SUBSCRIPTION_OPTS_DEFAULT);

    MaestroEvent event1 = subscription.poll();
    EXPECT(!event1);

    struct timespec ts = {1};
    MaestroEvent event2 = subscription.timedwait(&ts);
    EXPECT(!event2);
    EXPECT(event2.isNull());

    MaestroSubscription ackSub = selector2.subscribe(MSTRO_POOL_EVENT_OFFER, MSTRO_SUBSCRIPTION_OPTS_REQUIRE_ACK);
    MaestroEvent e = ackSub.poll();
    ackSub.ack(e);
}

}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
