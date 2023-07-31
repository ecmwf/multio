#include "multio/LibMultio.h"
#include "multio/tools/MultioTool.h"

#include "eckit/log/Log.h"
#include "eckit/option/SimpleOption.h"

#include <algorithm>
#include <map>
#include <vector>

#include <fcntl.h>
#include <unistd.h>

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

    const auto csvFileHandle = open(output.c_str(), O_CREAT | O_TRUNC | O_WRONLY, S_IWUSR | S_IRUSR);

    const std::string header("unique_id,event_id,extraInfo,start_time,end_time\r\n");
    write(csvFileHandle, reinterpret_cast<const void*>(header.c_str()), header.size());

    std::map<uint32_t, uint64_t> starts;
    std::map<uint32_t, uint64_t> ends;

    const auto traceFileHandle = open(input.c_str(), O_RDONLY);

    const auto fileSize = lseek(traceFileHandle, 0, SEEK_END);
    lseek(traceFileHandle, 0, SEEK_SET);

    off_t processedSize = 0;
    while (processedSize < fileSize) {
        off_t sizeToRead = std::min(static_cast<off_t>(1024 * 1024 * 1024), (fileSize - processedSize));

        std::vector<uint64_t> traceData(sizeToRead / sizeof(uint64_t));

        read(traceFileHandle, reinterpret_cast<void*>(traceData.data()), sizeToRead);

        for (auto i = 0; (i < traceData.size() / 2); ++i) {
            const auto event = traceData[2 * i];
            if (event == 0) {
                continue;
            }

            const auto uniqueEventId = event & 0xFFFFFFFFULL;
            const auto traceEventId = (event & (0xFFULL << 56)) >> 56;
            const auto extraInfo = (event & (0xFFFFULL << 32)) >> 32;
            const auto started = (event & (0xFFULL << 48)) > 0;

            const auto timestamp = traceData[2 * i + 1];

            if (started) {
                if (ends.count(uniqueEventId) == 0) {
                    if (starts.count(uniqueEventId) == 0) {
                        starts[uniqueEventId] = timestamp;
                    }
                    else {
                        std::cerr << "Event with id: " << uniqueEventId << " comming from value: " << event
                                  << " already found in the start data!" << std::endl;
                    }
                }
                else {
                    const auto correspondingEnd = ends.at(uniqueEventId);

                    std::ostringstream oss;
                    oss << uniqueEventId << "," << traceEventId << "," << extraInfo << "," << timestamp << ","
                        << correspondingEnd << "\r\n";

                    const auto line = oss.str();

                    write(csvFileHandle, reinterpret_cast<const void*>(line.c_str()), line.size());

                    ends.erase(uniqueEventId);
                }
            }
            else {
                if (starts.count(uniqueEventId) == 0) {
                    if (ends.count(uniqueEventId) == 0) {
                        ends[uniqueEventId] = timestamp;
                    }
                    else {
                        std::cerr << "Event with id: " << uniqueEventId << " comming from value: " << event
                                  << " already found in the end data!" << std::endl;
                    }
                }
                else {
                    const auto correspondingStart = starts.at(uniqueEventId);

                    std::ostringstream oss;
                    oss << uniqueEventId << "," << traceEventId << "," << extraInfo << "," << correspondingStart << ","
                        << timestamp << "\r\n";

                    const auto line = oss.str();

                    write(csvFileHandle, reinterpret_cast<const void*>(line.c_str()), line.size());

                    starts.erase(uniqueEventId);
                }
            }
        }

        processedSize += sizeToRead;
    }

    for (auto const& ev : starts) {
        std::cerr << "Event with id: " << ev.first << " does not have a matching end event!" << std::endl;
    }

    for (auto const& ev : ends) {
        std::cerr << "Event with id: " << ev.first << " does not have a matching start event!" << std::endl;
    }

    close(traceFileHandle);
    close(csvFileHandle);
}

//---------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    MultioConvertTraceLog tool(argc, argv);
    return tool.start();
}
