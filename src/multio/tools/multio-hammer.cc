
#include <algorithm>
#include <fstream>
#include <random>
#include "unistd.h"

#include "eccodes.h"

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/io/StdFile.h"
#include "eckit/log/Log.h"
#include "eckit/mpi/Comm.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"

#include "metkit/grib/GribDataBlob.h"
#include "metkit/grib/GribHandle.h"

#include "multio/action/Plan.h"
#include "multio/LibMultio.h"

#include "multio/domain/Domain.h"
#include "multio/message/Message.h"
#include "multio/server/ConfigurationPath.h"
#include "multio/server/Listener.h"
#include "multio/server/MpiTransport.h"
#include "multio/server/ThreadTransport.h"
#include "multio/server/TcpTransport.h"
#include "multio/tools/MultioTool.h"
#include "multio/util/print_buffer.h"

using multio::message::Message;
using multio::message::Metadata;
using multio::domain::Domain;
using multio::domain::Unstructured;
using multio::message::Peer;
using multio::action::Plan;

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
    for(auto rank = 0ul, offset = 0ul; rank != eckit::mpi::comm().size(); ++rank) {
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
        levels = {1,   2,   3,   5,   7,   10,  20,  30,  50,  70,  100,
                  150, 200, 250, 300, 400, 500, 600, 700, 850, 925, 1000};
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
        return 13.0 + 17.0 * static_cast<double>(hash_val) /
                          static_cast<double>(std::numeric_limits<uint32_t>::max());
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
    auto chunk_size = field_size() / nbclients + ((id < field_size() % nbclients) ? 1 : 0);

    auto maps = std::vector<int32_t>(chunk_size);
    for (size_t jj = 0; jj != chunk_size; ++jj) {
        maps[jj] = static_cast<int32_t>(id + jj * nbclients);
    }
    return maps;
}

std::vector<double>& global_test_field(const std::string& field_id, const size_t sz = 0,
                                       const std::string& transport = "",
                                       const size_t list_id = 0) {
    using eckit::mpi::comm;
    std::lock_guard<std::mutex> lock{mutex()};

    static std::map<std::string, std::vector<double>> test_fields;

    if (test_fields.find(field_id) != end(test_fields)) {
        return test_fields[field_id];
    }

    if (transport == "mpi" && new_random_data_each_run()) {
        test_fields[field_id] =
            (root() == list_id) ? create_random_data(sz) : std::vector<double>(sz);
        comm().broadcast(test_fields[field_id], root());

        return test_fields[field_id];
    }

    test_fields[field_id] =
        new_random_data_each_run() ? create_random_data(sz) : create_hashed_data(field_id, sz);

    return test_fields[field_id];
}

std::vector<double> file_content(const eckit::PathName& file_path) {
    std::fstream ifs(std::string(file_path.fullName()).c_str());
    std::string str{std::istreambuf_iterator<char>(ifs), std::istreambuf_iterator<char>()};
    auto beg = reinterpret_cast<const double*>(str.data());
    std::vector<double> vec(beg, beg + str.size() / sizeof(double));
    return vec;
}

eckit::LocalConfiguration test_configuration(const std::string& type) {
    eckit::Log::debug<multio::LibMultio>() << "Transport type: " << type << std::endl;

    std::map<std::string, std::string> configs = {{"mpi", "mpi-test-configuration"},
                                                  {"tcp", "tcp-test-configuration"},
                                                  {"thread", "thread-test-configuration"},
                                                  {"none", "no-transport-test-configuration"}};

    eckit::YAMLConfiguration testConfigs{configuration_path() + "test-configurations.yaml"};
    return eckit::LocalConfiguration{testConfigs.getSubConfiguration(configs.at(type))};
}

}  // namespace

//----------------------------------------------------------------------------------------------------------------------

class MultioHammer final : public multio::MultioTool {
public:  // methods
    MultioHammer(int argc, char** argv);

private:

    using PeerList = std::vector<std::unique_ptr<Peer>>;

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
    void executeMpi();
    void executeTcp();
    void executeThread();

    void startListening(std::shared_ptr<Transport> transport);
    void spawnServers(const PeerList& serverPeers, std::shared_ptr<Transport> transport);

    void sendData(const PeerList& serverPeers, std::shared_ptr<Transport> transport,
                  const size_t client_list_id) const;
    void spawnClients(const PeerList& clientPeers, const PeerList& serverPeers,
                      std::shared_ptr<Transport> transport);

    bool skipTest();
    void testData();

    std::string configPath_ = "";
    std::string transportType_ = "none";
    int port_ = 7777;

    size_t clientCount_ = 1;
    size_t serverCount_ = 1;
    size_t stepCount_ = 3;
    size_t levelCount_ = 3;
    size_t paramCount_ = 3;

    long ensMember_ = 1;
    long sleep_ = 0;

    eckit::LocalConfiguration config_;

    class Connection {
        std::shared_ptr<Transport> transport_;
        Peer source_;
        Peer destination_;

    public:
        Connection(std::shared_ptr<Transport> tprt, Peer src, Peer dest) :
            transport_{tprt},
            source_{src},
            destination_{dest} {
            transport_->send(Message{Message::Header{Message::Tag::Open, source_, destination_}});
        }

        ~Connection() {
            transport_->send(Message{Message::Header{Message::Tag::Close, source_, destination_}});
        }

        Connection(const Connection& rhs) = delete;
        Connection(Connection&& rhs) noexcept = delete;

        Connection& operator=(const Connection& rhs) = delete;
        Connection& operator=(Connection&& rhs) noexcept = delete;
    };
};

//---------------------------------------------------------------------------------------------------------------

MultioHammer::MultioHammer(int argc, char** argv) : multio::MultioTool(argc, argv) {
    options_.push_back(
        new eckit::option::SimpleOption<std::string>("config", "Path to configuration"));
    options_.push_back(
        new eckit::option::SimpleOption<std::string>("transport", "Type of transport layer"));
    options_.push_back(new eckit::option::SimpleOption<size_t>("nbclients", "Number of clients"));
    options_.push_back(new eckit::option::SimpleOption<size_t>("nbservers", "Number of servers"));
    options_.push_back(new eckit::option::SimpleOption<size_t>("port", "TCP port"));
    options_.push_back(new eckit::option::SimpleOption<size_t>("nbparams", "Number of parameters"));
    options_.push_back(
        new eckit::option::SimpleOption<size_t>("nblevels", "Number of model levels"));
    options_.push_back(
        new eckit::option::SimpleOption<size_t>("nbsteps", "Number of output time steps"));
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

    config_ =
        (configPath_.empty())
            ? test_configuration(transportType_)
            : eckit::LocalConfiguration{eckit::YAMLConfiguration{eckit::PathName{configPath_}}};

    transportType_ = config_.getString("transport");
    if (transportType_ == "mpi") {
        auto comm_size = eckit::mpi::comm(config_.getString("group").c_str()).size();
        if (comm_size != clientCount_ + serverCount_) {
            throw eckit::SeriousBug(
                "Number of MPI ranks does not match the number of clients and servers");
        }
    }
}

void MultioHammer::finish(const eckit::option::CmdArgs&) {}

//---------------------------------------------------------------------------------------------------------------

void MultioHammer::startListening(std::shared_ptr<Transport> transport) {
    Listener listener(config_, *transport);
    listener.listen();
}

void MultioHammer::spawnServers(const PeerList& serverPeers,
                                std::shared_ptr<Transport> transport) {
    if (find_if(begin(serverPeers), end(serverPeers),
                [&transport](const std::unique_ptr<Peer>& peer) {
                    return *peer == transport->localPeer();
                }) != end(serverPeers)) {
        startListening(transport);
    }
}

void MultioHammer::sendData(const PeerList& serverPeers,
                            std::shared_ptr<Transport> transport,
                            const size_t client_list_id) const {
    Peer client = transport->localPeer();

    // Open all servers and close them when going out of scope
    std::vector<std::unique_ptr<Connection>> connections;
    for (auto& server : serverPeers) {
        connections.emplace_back(new Connection{transport, client, *server});
    }

    auto idxm = generate_index_map(client_list_id, clientCount_);
    eckit::Buffer buffer(reinterpret_cast<const char*>(idxm.data()), idxm.size() * sizeof(int32_t));
    std::unique_ptr<Domain> index_map{new Unstructured{std::move(idxm)}};

    // send partial mapping
    for (auto& server : serverPeers) {
        Message msg{Message::Header{Message::Tag::Domain, client, *server, "grid-point",
                                    "unstructured", clientCount_},
                    buffer};

        transport->send(msg);
    }

    // send messages
    for (auto step : sequence(stepCount_, 1)) {
        for (auto param : sequence(paramCount_, 1)) {
            for (auto level : sequence(levelCount_, 1)) {
                Metadata metadata;
                metadata.set("level", level);
                metadata.set("param", param);
                metadata.set("step", step);

                std::string field_id = multio::message::to_string(metadata);

                std::vector<double> field;
                auto& global_field =
                    global_test_field(field_id, field_size(), transportType_, client_list_id);
                index_map->to_local(global_field, field);

                // Choose server
                auto id = std::hash<std::string>{}(field_id) % serverCount_;
                ASSERT(id < serverPeers.size());

                eckit::Buffer buffer(reinterpret_cast<const char*>(field.data()),
                                     field.size() * sizeof(double));

                Message msg{Message::Header{Message::Tag::Field, client, *serverPeers[id],
                                            std::to_string(param), "model-level", clientCount_,
                                            field_size(), "grid-point", field_id},
                            buffer};

                transport->send(msg);
            }
        }

        // Send flush messages
        for (auto& server : serverPeers) {
            auto stepStr = eckit::Translator<long, std::string>()(step);
            Message flush{Message::Header{Message::Tag::StepComplete, client, *server, stepStr,
                                          "step", clientCount_}};
            transport->send(flush);
        }
    }
}

void MultioHammer::spawnClients(const PeerList& clientPeers,
                                const PeerList& serverPeers,
                                std::shared_ptr<Transport> transport) {
    auto it = find_if(begin(clientPeers), end(clientPeers),
                      [&transport](const std::unique_ptr<Peer>& peer) {
                          return *peer == transport->localPeer();
                      });
    if (it != end(clientPeers)) {
        auto client_id = static_cast<size_t>(std::distance(begin(clientPeers), it));
        sendData(serverPeers, transport, client_id);
    }
}

//---------------------------------------------------------------------------------------------------------------

void MultioHammer::execute(const eckit::option::CmdArgs& args) {
    field_size() = 29;

    if (transportType_ == "none") {
        executePlans(args);
    }
    if (transportType_ == "mpi") {
        executeMpi();
    }
    if (transportType_ == "tcp") {
        executeTcp();
    }
    if (transportType_ == "thread") {
        executeThread();
    }

    testData();
}

bool MultioHammer::skipTest() {
    bool doTest = false;

    if (transportType_ == "thread") {
        doTest = true;
    }

    if (transportType_ == "mpi") {
        eckit::mpi::comm().barrier();
        doTest = (eckit::mpi::comm().rank() == root());
    }

    return !doTest;
}

void MultioHammer::testData() {
    if (skipTest()) {
        return;
    }

    for (auto step : sequence(stepCount_, 1)) {
        for (auto param : sequence(paramCount_, 1)) {
            for (auto level : sequence(levelCount_, 1)) {
                std::string file_name = std::to_string(level) + std::string("::") +
                                        std::to_string(param) + std::string("::") +
                                        std::to_string(step);
                std::string field_id = R"({"level":)" + std::to_string(level) + R"(,"param":)" +
                                       std::to_string(param) + R"(,"step":)" +
                                       std::to_string(step) + "}";
                auto expect = global_test_field(field_id);
                auto actual = file_content(file_name);

                eckit::Log::info() << "field id = " << field_id << std::endl;
                eckit::Log::info() << "file_name = " << file_name << std::endl;

                eckit::Log::debug<multio::LibMultio>() << std::endl << "Expect = ";
                multio::print_buffer(expect, eckit::Log::info());
                eckit::Log::debug<multio::LibMultio>() << std::endl << "Actual = ";
                multio::print_buffer(actual, eckit::Log::info());
                eckit::Log::debug<multio::LibMultio>() << std::endl;

                ASSERT(expect == actual);

                std::remove(file_name.c_str());
            }
        }
    }
}

void MultioHammer::executeMpi() {
    std::shared_ptr<Transport> transport{TransportFactory::instance().build("mpi", config_)};

    auto comm = config_.getString("group");

    PeerList clientPeers;
    auto i = 0u;
    while (i != clientCount_) {
        clientPeers.emplace_back(new MpiPeer{comm, i++});
    }

    PeerList serverPeers;
    auto comm_size = clientCount_ + serverCount_;
    while (i != comm_size) {
        serverPeers.emplace_back(new MpiPeer{comm, i++});
    }

    spawnServers(serverPeers, transport);
    spawnClients(clientPeers, serverPeers, transport);
}

void MultioHammer::executeTcp() {
    config_.set("local_port", port_);
    std::shared_ptr<Transport> transport{TransportFactory::instance().build("tcp", config_)};

    PeerList serverPeers;
    for (auto cfg : config_.getSubConfigurations("servers")) {
        auto host = cfg.getString("host");
        for (auto port : cfg.getUnsignedVector("ports")) {
            serverPeers.emplace_back(new TcpPeer{host, port});
        }
    }

    PeerList clientPeers;
    for (auto cfg : config_.getSubConfigurations("clients")) {
        auto host = cfg.getString("host");
        for (auto port : cfg.getUnsignedVector("ports")) {
            clientPeers.emplace_back(new TcpPeer{host, port});
        }
    }

    clientCount_ = clientPeers.size();
    serverCount_ = serverPeers.size();

    spawnServers(serverPeers, transport);
    spawnClients(clientPeers, serverPeers, transport);
}

void MultioHammer::executeThread() {
    std::shared_ptr<Transport> transport{TransportFactory::instance().build("thread", config_)};

    // Spawn servers
    PeerList serverPeers;
    for (size_t i = 0; i != serverCount_; ++i) {
        serverPeers.emplace_back(
            new ThreadPeer{std::thread{&MultioHammer::startListening, this, transport}});
    }

    // Spawn clients
    PeerList clientPeers;
    for (auto client : sequence(clientCount_, 0)) {
        clientPeers.emplace_back(new ThreadPeer{
            std::thread{&MultioHammer::sendData, this, std::cref(serverPeers), transport, client}});
    }
}

void MultioHammer::executePlans(const eckit::option::CmdArgs& args) {
    eckit::AutoStdFile fin(args(0));

    int err;
    codes_handle* handle = codes_handle_new_from_file(nullptr, fin, PRODUCT_GRIB, &err);
    ASSERT(handle);

    std::vector<std::unique_ptr<Plan>> plans;
    for (const auto& cfg : config_.getSubConfigurations("plans")) {
        eckit::Log::debug<multio::LibMultio>() << cfg << std::endl;
        plans.emplace_back(new Plan(cfg));
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

        const auto& paramList = config_.getSubConfiguration("parameters");

        for (const auto& levtype : {"ml", "pl", "sfc"}) {
            size = std::strlen(levtype);
            CODES_CHECK(codes_set_string(handle, "levtype", levtype, &size), nullptr);

            for (auto level : create_levlist(levtype, levelCount_)) {
                CODES_CHECK(codes_set_long(handle, "level", level), nullptr);

                for (auto param : valid_parameters(paramList, levtype)) {
                    CODES_CHECK(codes_set_long(handle, "param", param), nullptr);

                    eckit::Log::debug<multio::LibMultio>()
                        << "Member: " << ensMember_ << ", step: " << step
                        << ", levtype: " << levtype << ", level: " << level << ", param: " << param
                        << ", payload size: " << sz << std::endl;

                    CODES_CHECK(
                        codes_get_message(handle, reinterpret_cast<const void**>(&buf), &sz),
                        nullptr);

                    Message msg{Message::Header{Message::Tag::Grib, Peer{"", 0}, Peer{"", 0}},
                                eckit::Buffer{buf, sz}};

                    for (const auto& plan : plans) {
                        plan->process(msg);
                    }
                }
            }
        }

        auto stepStr = eckit::Translator<long, std::string>()(step);

        Message msg{Message::Header{Message::Tag::StepComplete, Peer{"", 0}, Peer{"", 0}, stepStr,
                                    "step", 1}};
        for (const auto& plan : plans) {
            plan->process(msg);
        }

        // This message need only be sent by one server per ENS. Some sort of synchronisation
        // between the servers will be required -- OK for multio-hammer for now.
        msg = Message{Message::Header{Message::Tag::StepNotification, Peer{"", 0}, Peer{"", 0},
                                      stepStr, "step", 1}};
        for (const auto& plan : plans) {
            plan->process(msg);
        }
    }

    codes_handle_delete(handle);
}

//---------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    MultioHammer tool{argc, argv};
    return tool.start();
}
