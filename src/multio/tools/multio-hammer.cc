
#include <unistd.h>

#include <algorithm>
#include <fstream>
#include <random>

#include "eccodes.h"

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/io/StdFile.h"
#include "eckit/log/Log.h"
#include "eckit/mpi/Comm.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"

#include "metkit/codes/GribHandle.h"

#include "multio/LibMultio.h"
#include "multio/action/Plan.h"

#include "multio/domain/Domain.h"
#include "multio/message/Message.h"
#include "multio/server/Listener.h"
#include "multio/tools/MultioTool.h"
#include "multio/transport/MpiTransport.h"
#include "multio/transport/TcpTransport.h"
#include "multio/transport/ThreadTransport.h"
#include "multio/util/ConfigurationContext.h"
#include "multio/util/ConfigurationPath.h"
#include "multio/util/ScopedTimer.h"
#include "multio/util/print_buffer.h"

using multio::LibMultio;
using multio::action::Plan;
using multio::domain::Domain;
using multio::domain::Unstructured;
using multio::message::Message;
using multio::message::Metadata;
using multio::message::Peer;
using multio::transport::MpiPeer;
using multio::transport::TcpPeer;
using multio::transport::ThreadPeer;
using multio::transport::Transport;
using multio::transport::TransportFactory;
using multio::util::ComponentTag;
using multio::util::configuration_path_name;
using multio::util::ConfigurationContext;

using namespace multio::server;

//----------------------------------------------------------------------------------------------------------------------

namespace {
size_t& field_size() {
    static size_t val;
    return (!val ? (val = 23) : val);
}

size_t& root() {
    static size_t rt;  // = 0 if not set to another value
    return rt;
}

bool& new_random_data_each_run() {
    static bool val = false;
    return val;
}

std::mutex& mutex() {
    static std::mutex mut;
    return mut;
}

std::vector<long> sequence(size_t sz, size_t start) {
    std::vector<long> vals(sz);
    iota(begin(vals), end(vals), start);
    return vals;
}

struct Chunks {
    size_t offset;
    size_t size;
};

std::vector<Chunks> create_chunks(size_t sz) {

    auto quotient = sz / eckit::mpi::comm().size();
    auto remainder = sz % eckit::mpi::comm().size();

    std::vector<Chunks> chunks;
    for (auto rank = 0ul, offset = 0ul; rank != eckit::mpi::comm().size(); ++rank) {
        auto chunk_size = quotient + static_cast<size_t>((rank < remainder) ? 1 : 0);
        chunks.push_back({offset, chunk_size});
        offset += chunk_size;
    }
    return chunks;
}

std::vector<long> create_levlist(const std::string& ltype, size_t sz = 91, size_t start = 1) {

    std::vector<long> levels;
    if (ltype == "ml") {
        levels = sequence(sz, start);
    }
    if (ltype == "pl") {
        levels = {1, 2, 3, 5, 7, 10, 20, 30, 50, 70, 100, 150, 200, 250, 300, 400, 500, 600, 700, 850, 925, 1000};
    }
    if (ltype == "sfc") {
        levels = {1};
    }
    if (sz < levels.size()) {
        levels.resize(sz);
    }

    auto chunk = create_chunks(levels.size());

    auto beg = std::begin(levels) + chunk[eckit::mpi::comm().rank()].offset;
    auto levs = std::vector<long>{beg, beg + chunk[eckit::mpi::comm().rank()].size};

    return levs;
}

std::vector<long> valid_parameters(const eckit::Configuration& pcnf, const std::string& ltype) {
    // Read from configuration
    return (pcnf.has(ltype) ? pcnf.getLongVector(ltype) : std::vector<long>{});
}

std::vector<double> create_hashed_data(const std::string& field_id, const size_t sz) {
    std::vector<double> field(sz);

    auto ii = 0;
    generate(begin(field), end(field), [&ii, &field_id]() {
        auto hash_val = std::hash<std::string>{}(field_id + std::to_string(ii++));
        hash_val = static_cast<uint32_t>(hash_val >> 32);
        return 13.0 + 17.0 * static_cast<double>(hash_val) / static_cast<double>(std::numeric_limits<uint32_t>::max());
    });

    return field;
}

std::vector<double> create_random_data(const size_t sz) {
    std::vector<double> field;

    std::random_device rd;   // Will be used to obtain a seed for the random number engine
    std::mt19937 gen(rd());  // Standard mersenne_twister_engine seeded with rd()
    std::uniform_real_distribution<double> dis(13.0, 30.0);

    for (auto ii = 0u; ii != sz; ++ii) {
        field.push_back(dis(gen));
    }

    return field;
}

std::vector<int32_t> generate_index_map(size_t id, size_t nbclients) {
    ASSERT(nbclients > 0);
    auto chunk_size = field_size() / nbclients + ((id < field_size() % nbclients) ? 1 : 0);

    auto maps = std::vector<int32_t>(chunk_size);
    for (size_t jj = 0; jj != chunk_size; ++jj) {
        maps[jj] = static_cast<int32_t>(id + jj * nbclients);
    }
    return maps;
}

std::vector<double>& global_test_field(const std::string& field_id, const size_t sz = 0,
                                       const std::string& transport = "", const size_t list_id = 0) {
    using eckit::mpi::comm;
    std::lock_guard<std::mutex> lock{mutex()};

    static std::map<std::string, std::vector<double>> test_fields;

    if (test_fields.find(field_id) != end(test_fields)) {
        return test_fields[field_id];
    }

    if (transport == "mpi" && new_random_data_each_run()) {
        test_fields[field_id] = (root() == list_id) ? create_random_data(sz) : std::vector<double>(sz);
        comm().broadcast(test_fields[field_id], root());

        return test_fields[field_id];
    }

    test_fields[field_id] = new_random_data_each_run() ? create_random_data(sz) : create_hashed_data(field_id, sz);

    return test_fields[field_id];
}

std::vector<double> file_content(const eckit::PathName& file_path) {
    std::fstream ifs(std::string(file_path.fullName()).c_str());
    std::string str{std::istreambuf_iterator<char>(ifs), std::istreambuf_iterator<char>()};
    auto beg = reinterpret_cast<const double*>(str.data());
    std::vector<double> vec(beg, beg + str.size() / sizeof(double));
    return vec;
}

ConfigurationContext test_configuration(const std::string& type) {
    eckit::Log::debug<multio::LibMultio>() << "Transport type: " << type << std::endl;

    static std::map<std::string, std::string> configs = {{"mpi", "mpi-test-configuration"},
                                                         {"tcp", "tcp-test-configuration"},
                                                         {"thread", "thread-test-configuration"},
                                                         {"none", "no-transport-test-configuration"}};

    auto fileName = configuration_path_name("") + "test-configurations.yaml";
    return ConfigurationContext(fileName).subContext(configs.at(type));
}

}  // namespace

//----------------------------------------------------------------------------------------------------------------------

class MultioHammer final : public multio::MultioTool {
public:  // methods
    MultioHammer(int argc, char** argv);

private:
    using PeerList = std::vector<std::unique_ptr<Peer>>;

    class TestPolicy {
    public:
        TestPolicy(ConfigurationContext& configurationContext, size_t clientCount) :
            configurationContext_(configurationContext), clientCount_(clientCount) {}
        virtual ~TestPolicy() = default;

        virtual PeerList computeServerPeers() = 0;
        virtual int computeRank() = 0;
        virtual std::shared_ptr<Transport> createTransport() const = 0;

        virtual void execute(MultioHammer& hammer, const eckit::option::CmdArgs&) const { hammer.executeGeneric(); }

        virtual void checkData(MultioHammer& hammer) const { hammer.testData(); }

    protected:
        ConfigurationContext& configurationContext_;
        size_t const clientCount_;
    };

    class MPITestPolicy : public TestPolicy {
    public:
        MPITestPolicy(ConfigurationContext& configurationContext, size_t clientCount) :
            TestPolicy(configurationContext, clientCount),
            commName_(configurationContext_.config().getString("group")),
            comm_(eckit::mpi::comm(commName_.c_str())) {}

        virtual ~MPITestPolicy() = default;

        virtual PeerList computeServerPeers() override {
            auto comm_size = comm_.size();

            PeerList serverPeers;
            size_t i = clientCount_;
            while (i != comm_size) {
                serverPeers.emplace_back(new MpiPeer{commName_, i++});
            }
            return serverPeers;
        }

        virtual int computeRank() override { return comm_.rank(); }

        virtual std::shared_ptr<Transport> createTransport() const override {
            return std::shared_ptr<Transport>(TransportFactory::instance().build(
                "mpi",
                (comm_.rank() < clientCount_ ? configurationContext_.tagServer() : configurationContext_.tagClient())
                    .recast(ComponentTag::Transport)));
        };

        virtual void checkData(MultioHammer& hammer) const override {
            eckit::mpi::comm().barrier();
            if (eckit::mpi::comm().rank() != root()) {
                return;
            }

            hammer.testData();
        }

    private:
        std::string const commName_;
        eckit::mpi::Comm const& comm_;
    };

    class TCPTestPolicy : public TestPolicy {
    public:
        TCPTestPolicy(ConfigurationContext& configurationContext, size_t clientCount, int localPort) :
            TestPolicy(configurationContext, clientCount), port_(localPort), rank_(-1) {}

        virtual ~TCPTestPolicy() = default;

        virtual PeerList computeServerPeers() override {
            PeerList serverPeers;
            for (auto cfg : configurationContext_.config().getSubConfigurations("servers")) {
                auto const host = cfg.getString("host");
                auto const localhost = (host == "localhost");
                for (auto port : cfg.getUnsignedVector("ports")) {
                    rank_ = (localhost && (port_ == port)) ? serverPeers.size() + clientCount_ : rank_;
                    serverPeers.emplace_back(new TcpPeer{host, port});
                }
            }

            return serverPeers;
        }

        virtual int computeRank() override {
            if (rank_ < 0) {
                auto currentClientNumber = 0;
                for (auto cfg : configurationContext_.config().getSubConfigurations("clients")) {
                    for (auto port : cfg.getUnsignedVector("ports")) {
                        // A client can only originate from localhost so if the port matches,
                        // we use the current client number as the rank.
                        rank_ = (port_ == port) ? currentClientNumber : rank_;
                        ++currentClientNumber;
                    }
                }
            }

            return rank_;
        }

        virtual std::shared_ptr<Transport> createTransport() const override {
            configurationContext_.config().set("local_port", port_);
            return std::shared_ptr<Transport>(
                TransportFactory::instance().build("tcp", configurationContext_.recast(ComponentTag::Transport)));
        };

        virtual void checkData(MultioHammer& hammer) const override {
            // Wait for the plan to finish.
            std::this_thread::sleep_for(std::chrono::seconds(1));

            if (rank_ == 0) {
                // Test is destructive(deletes the output files), only perform the test on the first process.
                hammer.testData();
            }
        }

    private:
        int port_;
        int rank_;
    };

    class ThreadTestPolicy : public TestPolicy {
    public:
        ThreadTestPolicy(ConfigurationContext& configurationContext, size_t clientCount) :
            TestPolicy(configurationContext, clientCount) {}
        virtual ~ThreadTestPolicy() = default;

        virtual PeerList computeServerPeers() override {
            // No peers for this communcation policy
            return PeerList();
        }

        virtual int computeRank() override { return 0; }

        virtual std::shared_ptr<Transport> createTransport() const override {
            return std::shared_ptr<Transport>(
                TransportFactory::instance().build("thread", configurationContext_.recast(ComponentTag::Transport)));
        };

        virtual void execute(MultioHammer& hammer, const eckit::option::CmdArgs&) const override {
            hammer.executeThread();
        }
    };

    class PlanOnlyTestPolicy : public TestPolicy {
    public:
        PlanOnlyTestPolicy(ConfigurationContext& configurationContext, size_t clientCount) :
            TestPolicy(configurationContext, clientCount) {}
        virtual ~PlanOnlyTestPolicy() = default;

        virtual PeerList computeServerPeers() override {
            // No peers for this communcation policy
            return PeerList();
        }

        virtual int computeRank() override { return 0; }

        virtual std::shared_ptr<Transport> createTransport() const override { return nullptr; };

        virtual void execute(MultioHammer& hammer, const eckit::option::CmdArgs& args) const override {
            hammer.executePlans(args);
        }

        virtual void checkData(MultioHammer& hammer) const override {}
    };

    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl
                           << "Usage: " << tool << " [options]" << std::endl
                           << std::endl
                           << std::endl
                           << "Examples:" << std::endl
                           << "=========" << std::endl
                           << std::endl
                           << tool << " --transport=mpi --nbclients=10 --nbservers=4" << std::endl
                           << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs& args) override;

    void execute(const eckit::option::CmdArgs& args) override;

    void executePlans(const eckit::option::CmdArgs& args);
    void executeGeneric();
    void executeThread();

    void startListening(std::shared_ptr<Transport> transport);
    void sendData(const PeerList& serverPeers, std::shared_ptr<Transport> transport, const size_t client_list_id) const;

    void testData();

    std::string configPath_ = "";
    std::string transportType_ = "none";
    int port_ = 7777;
    int rank_ = 0;

    size_t clientCount_ = 1;
    size_t serverCount_ = 1;
    size_t stepCount_ = 3;
    size_t levelCount_ = 3;
    size_t paramCount_ = 3;

    long ensMember_ = 1;
    long sleep_ = 0;

    PeerList serverPeers_;
    std::shared_ptr<Transport> transport_;
    std::unique_ptr<TestPolicy> testPolicy_;

    ConfigurationContext confCtx_{eckit::LocalConfiguration(), "", ""};

    class Connection {
        std::shared_ptr<Transport> transport_;
        Peer source_;
        Peer destination_;

    public:
        Connection(std::shared_ptr<Transport> tprt, Peer src, Peer dest) :
            transport_{tprt}, source_{src}, destination_{dest} {
            transport_->send(Message{Message::Header{Message::Tag::Open, source_, destination_}});
        }

        ~Connection() { transport_->send(Message{Message::Header{Message::Tag::Close, source_, destination_}}); }

        Connection(const Connection& rhs) = delete;
        Connection(Connection&& rhs) noexcept = delete;

        Connection& operator=(const Connection& rhs) = delete;
        Connection& operator=(Connection&& rhs) noexcept = delete;
    };
};

//---------------------------------------------------------------------------------------------------------------

MultioHammer::MultioHammer(int argc, char** argv) : multio::MultioTool(argc, argv) {
    options_.push_back(new eckit::option::SimpleOption<std::string>("config", "Path to configuration"));
    options_.push_back(new eckit::option::SimpleOption<std::string>("transport", "Type of transport layer"));
    options_.push_back(new eckit::option::SimpleOption<size_t>("nbclients", "Number of clients"));
    options_.push_back(new eckit::option::SimpleOption<size_t>("nbservers", "Number of servers"));
    options_.push_back(new eckit::option::SimpleOption<size_t>("port", "TCP port"));
    options_.push_back(new eckit::option::SimpleOption<size_t>("nbparams", "Number of parameters"));
    options_.push_back(new eckit::option::SimpleOption<size_t>("nblevels", "Number of model levels"));
    options_.push_back(new eckit::option::SimpleOption<size_t>("nbsteps", "Number of output time steps"));
    options_.push_back(new eckit::option::SimpleOption<size_t>("member", "Ensemble member"));
    options_.push_back(new eckit::option::SimpleOption<long>("sleep", "Seconds of simulated work per step"));
}


void MultioHammer::init(const eckit::option::CmdArgs& args) {
    args.get("config", configPath_);
    args.get("transport", transportType_);
    args.get("port", port_);

    args.get("nbclients", clientCount_);
    args.get("nbservers", serverCount_);
    args.get("nbsteps", stepCount_);
    args.get("nblevels", levelCount_);
    args.get("nbparams", paramCount_);
    args.get("member", ensMember_);
    args.get("sleep", sleep_);

    confCtx_
        = (configPath_.empty())
            ? test_configuration(transportType_)
            : ConfigurationContext(eckit::LocalConfiguration{eckit::YAMLConfiguration{eckit::PathName{configPath_}}},
                                   configPath_, configPath_);

    confCtx_.config().set("clientCount", clientCount_);

    using PolicyBuilder = std::function<std::unique_ptr<TestPolicy>()>;
    std::map<std::string, PolicyBuilder> const policyFactory = {
        {"mpi", [&]() { return std::make_unique<MPITestPolicy>(confCtx_, clientCount_); }},
        {"tcp", [&]() { return std::make_unique<TCPTestPolicy>(confCtx_, clientCount_, port_); }},
        {"thread", [&]() { return std::make_unique<ThreadTestPolicy>(confCtx_, clientCount_); }},
        {"none", [&]() { return std::make_unique<PlanOnlyTestPolicy>(confCtx_, clientCount_); }},
    };

    transportType_ = confCtx_.config().getString("transport");

    testPolicy_ = policyFactory.at(transportType_)();

    serverPeers_ = testPolicy_->computeServerPeers();
    rank_ = testPolicy_->computeRank();
    transport_ = testPolicy_->createTransport();
}

void MultioHammer::finish(const eckit::option::CmdArgs&) {}

//---------------------------------------------------------------------------------------------------------------

void MultioHammer::startListening(std::shared_ptr<Transport> transport) {
    Listener listener(confCtx_.recast(ComponentTag::Receiver), *transport);
    listener.start();
}

void MultioHammer::sendData(const PeerList& serverPeers, std::shared_ptr<Transport> transport,
                            const size_t client_list_id) const {
    Peer client = transport->localPeer();

    // Open all servers and close them when going out of scope
    std::vector<std::unique_ptr<Connection>> connections;
    for (auto& server : serverPeers) {
        connections.emplace_back(std::make_unique<Connection>(transport, client, *server));
    }

    auto idxm = generate_index_map(client_list_id, clientCount_);
    eckit::Buffer buffer(reinterpret_cast<const char*>(idxm.data()), idxm.size() * sizeof(int32_t));
    std::unique_ptr<Domain> index_map
        = std::make_unique<Unstructured>(std::move(idxm), static_cast<long>(field_size()));

    // send partial mapping
    for (auto& server : serverPeers) {
        Metadata metadata;
        metadata.set("name", "grid-point")
            .set("category", "atms-domain-map")
            .set("globalSize", static_cast<long>(field_size()))
            .set("representation", "unstructured");

        Message msg{Message::Header{Message::Tag::Domain, client, *server, std::move(metadata)}, buffer};

        transport->send(msg);
    }

    // send messages
    for (auto step : sequence(stepCount_, 1)) {
        for (auto param : sequence(paramCount_, 1)) {
            for (auto level : sequence(levelCount_, 1)) {
                Metadata metadata;
                metadata.set("level", level).set("param", param).set("step", step);

                std::string field_id = multio::message::to_string(metadata);

                std::vector<double> field;
                auto& global_field = global_test_field(field_id, field_size(), transportType_, client_list_id);
                index_map->toLocal(global_field, field);

                // Choose server
                auto id = std::hash<std::string>{}(field_id) % serverCount_;
                ASSERT(id < serverPeers.size());

                eckit::Buffer buffer(reinterpret_cast<const char*>(field.data()), field.size() * sizeof(double));

                metadata.set("name", std::to_string(param))
                    .set("param", std::to_string(param))
                    .set("category", "model-level")
                    .set("globalSize", static_cast<long>(field_size()))
                    .set("domain", "grid-point")
                    .set("precision", "double");

                Message msg{Message::Header{Message::Tag::Field, client, *serverPeers[id], std::move(metadata)},
                            std::move(buffer)};

                transport->send(msg);
            }
        }

        // Send flush messages
        Metadata md;
        md.set("name", eckit::Translator<long, std::string>{}(step))
            .set("category", "atms-checkpoint")
            .set("trigger", "step")
            .set("domain", "grid-point");
        for (auto& server : serverPeers) {
            Message flush{Message::Header{Message::Tag::StepComplete, client, *server, Metadata{md}}};
            transport->send(flush);
        }
    }
}

//---------------------------------------------------------------------------------------------------------------

void MultioHammer::execute(const eckit::option::CmdArgs& args) {
    field_size() = 29;

    eckit::Log::info() << " *** multio-hammer config: " << confCtx_.config() << std::endl;

    testPolicy_->execute(*this, args);
    testPolicy_->checkData(*this);
}

void MultioHammer::testData() {
    for (auto step : sequence(stepCount_, 1)) {
        for (auto param : sequence(paramCount_, 1)) {
            for (auto level : sequence(levelCount_, 1)) {
                std::string file_name = std::to_string(level) + std::string("::") + std::to_string(param)
                                      + std::string("::") + std::to_string(step);
                std::string field_id = R"({"level":)" + std::to_string(level) + R"(,"param":)" + std::to_string(param)
                                     + R"(,"step":)" + std::to_string(step) + "}";
                auto expect = global_test_field(field_id);
                auto actual = file_content(file_name);

                LOG_DEBUG_LIB(LibMultio) << "field id = " << field_id << std::endl;
                LOG_DEBUG_LIB(LibMultio) << "file_name = " << file_name << std::endl;

                LOG_DEBUG_LIB(LibMultio) << "Expect = ";
                multio::util::print_buffer(expect, eckit::Log::debug<LibMultio>());
                LOG_DEBUG_LIB(LibMultio) << "\nActual = ";
                multio::util::print_buffer(actual, eckit::Log::debug<LibMultio>());
                LOG_DEBUG_LIB(LibMultio) << std::endl << std::endl;

                ASSERT(expect == actual);

                std::remove(file_name.c_str());
            }
        }
    }
}

void MultioHammer::executeGeneric() {
    auto const iAmServer = (rank_ >= clientCount_);

    if (iAmServer) {
        startListening(transport_);
    }
    else {
        sendData(serverPeers_, transport_, rank_);
    }
}

void MultioHammer::executeThread() {
    // Spawn servers
    PeerList serverPeers;
    for (size_t i = 0; i != serverCount_; ++i) {
        serverPeers.emplace_back(
            std::make_unique<ThreadPeer>(std::thread{&MultioHammer::startListening, this, transport_}));
    }

    // Spawn clients
    PeerList clientPeers;
    for (auto client : sequence(clientCount_, 0)) {
        clientPeers.emplace_back(std::make_unique<ThreadPeer>(
            std::thread{&MultioHammer::sendData, this, std::cref(serverPeers), transport_, client}));
    }
}

void MultioHammer::executePlans(const eckit::option::CmdArgs& args) {
    eckit::AutoStdFile fin(args(0));

    int err;
    codes_handle* handle = codes_handle_new_from_file(nullptr, fin, PRODUCT_GRIB, &err);
    ASSERT(handle);

    std::vector<std::unique_ptr<Plan>> plans;
    for (auto&& subCtx : confCtx_.subContexts("plans", ComponentTag::Plan)) {
        eckit::Log::debug<multio::LibMultio>() << subCtx.config() << std::endl;
        plans.emplace_back(std::make_unique<Plan>(std::move(subCtx)));
    }

    std::string expver = "xxxx";
    auto size = expver.size();
    CODES_CHECK(codes_set_string(handle, "expver", expver.c_str(), &size), nullptr);
    std::string cls = "rd";
    size = cls.size();
    CODES_CHECK(codes_set_string(handle, "class", cls.c_str(), &size), nullptr);

    const char* buf = nullptr;
    size_t sz = 0;

    CODES_CHECK(codes_set_long(handle, "number", ensMember_), nullptr);

    for (auto step : sequence(stepCount_, 1)) {
        ::sleep(sleep_);

        CODES_CHECK(codes_set_long(handle, "step", step), nullptr);

        const auto& paramList = confCtx_.config().getSubConfiguration("parameters");

        for (const auto& levtype : {"ml", "pl", "sfc"}) {
            size = std::strlen(levtype);
            CODES_CHECK(codes_set_string(handle, "levtype", levtype, &size), nullptr);

            for (auto level : create_levlist(levtype, levelCount_)) {
                CODES_CHECK(codes_set_long(handle, "level", level), nullptr);

                for (auto param : valid_parameters(paramList, levtype)) {

                    eckit::Log::debug<multio::LibMultio>()
                        << "Member: " << ensMember_ << ", step: " << step << ", levtype: " << levtype
                        << ", level: " << level << ", param: " << param << ", payload size: " << sz << std::endl;

                    CODES_CHECK(codes_set_long(handle, "paramId", param), nullptr);

                    CODES_CHECK(codes_get_message(handle, reinterpret_cast<const void**>(&buf), &sz), nullptr);

                    Message msg{Message::Header{Message::Tag::Grib, Peer{"", 0}, Peer{"", 0}}, eckit::Buffer{buf, sz}};

                    for (const auto& plan : plans) {
                        plan->process(msg);
                    }
                }
            }
        }

        Metadata md;
        md.set("name", eckit::Translator<long, std::string>()(step))
            .set("category", "atms-checkpoint")
            .set("trigger", "step")
            .set("domain", "grid-point")
            .set("precision", "double");

        Message msg{Message::Header{Message::Tag::StepComplete, Peer{}, Peer{}, Metadata{md}}};
        for (const auto& plan : plans) {
            plan->process(msg);
        }

        // This message need only be sent by one server per ENS. Some sort of synchronisation
        // between the servers will be required -- OK for multio-hammer for now.
        msg = Message{Message::Header{Message::Tag::StepNotification, Peer{}, Peer{}, std::move(md)}};
        for (const auto& plan : plans) {
            plan->process(msg);
        }
    }

    codes_handle_delete(handle);
}

//---------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    int ret;
    eckit::Timing timing_;
    {
        multio::util::ScopedTimer scTimer{timing_};
        MultioHammer tool{argc, argv};
        ret = tool.start();
    }
    eckit::Log::info() << "-- Total hammer:   " << timing_ << "s" << std::endl;
    return ret;
}
