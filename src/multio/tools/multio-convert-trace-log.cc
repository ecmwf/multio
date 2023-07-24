#include "multio/LibMultio.h"
#include "multio/tools/MultioTool.h"

#include "eckit/log/Log.h"
#include "eckit/option/SimpleOption.h"

#include <map>
#include <vector>

#include <fcntl.h>
#include <unistd.h>

namespace {
struct TraceData {
    uint64_t timestamp;
    uint32_t traceEventId;
};
}  // namespace

class MultioConvertTraceLog final : public multio::MultioTool {
public:  // methods
    MultioConvertTraceLog(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs& args) override;

    void execute(const eckit::option::CmdArgs& args) override;
};

MultioConvertTraceLog::MultioConvertTraceLog(int argc, char** argv) : multio::MultioTool(argc, argv) {
    options_.push_back(new eckit::option::SimpleOption<std::string>("input", "Trace file input path"));
    options_.push_back(new eckit::option::SimpleOption<std::string>("output", "Output CSV file path"));
}

void MultioConvertTraceLog::init(const eckit::option::CmdArgs& args) {}

void MultioConvertTraceLog::finish(const eckit::option::CmdArgs&) {}

void MultioConvertTraceLog::execute(const eckit::option::CmdArgs& args) {
    std::string input = "";
    args.get("input", input);

    std::string output = "sfc";
    args.get("output", output);

    const auto traceFileHandle = open(input.c_str(), O_RDONLY);

    const auto fileSize = lseek(traceFileHandle, 0, SEEK_END);
    lseek(traceFileHandle, 0, SEEK_SET);

    std::vector<uint64_t> traceData(fileSize / sizeof(uint64_t));

    read(traceFileHandle, reinterpret_cast<void*>(traceData.data()), fileSize);

    close(traceFileHandle);

    std::map<uint32_t, TraceData> starts;
    std::map<uint32_t, TraceData> ends;

    bool finished = false;
    for (auto i = 0; (i < traceData.size() / 2) && !finished; ++i) {
        const auto event = traceData[2 * i];
        if (event == 0) {
            break;
        }

        TraceData item;

        item.timestamp = traceData[2 * i + 1];
        const auto uniqueEventId = event & 0xFFFFFFFFULL;
        item.traceEventId = (event & (0xFFULL << 56)) >> 56;
        const auto started = (event & (0xFFULL << 48)) > 0;

        if (started) {
            if (starts.count(uniqueEventId) == 0) {
                starts[uniqueEventId] = std::move(item);
            }
            else {
                std::cerr << "Event with id: " << uniqueEventId << " comming from value: " << event
                          << " already found in the start data!" << std::endl;
            }
        }
        else {
            if (ends.count(uniqueEventId) == 0) {
                ends[uniqueEventId] = std::move(item);
            }
            else {
                std::cerr << "Event with id: " << uniqueEventId << " comming from value: " << event
                          << " already found in the end data!" << std::endl;
            }
        }
    }

    traceData.clear();

    const auto csvFileHandle = open(output.c_str(), O_CREAT | O_TRUNC | O_WRONLY, S_IWUSR | S_IRUSR);

    const std::string header("unique_id,event_id,start_time,end_time\r\n");
    write(csvFileHandle, reinterpret_cast<const void*>(header.c_str()), header.size());

    for (const auto& startEvent : starts) {
        try {
            const auto& correspondingEnd = ends.at(startEvent.first);

            std::ostringstream oss;
            oss << startEvent.first << "," << startEvent.second.traceEventId << "," << startEvent.second.timestamp
                << "," << correspondingEnd.timestamp << "\r\n";

            const auto line = oss.str();

            write(csvFileHandle, reinterpret_cast<const void*>(line.c_str()), line.size());
        }
        catch (const std::out_of_range& ex) {
            std::cerr << "Event with id: " << startEvent.first << " does not have an end timestamp." << std::endl;
        }
    }

    close(csvFileHandle);
}

//---------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    MultioConvertTraceLog tool(argc, argv);
    return tool.start();
}
