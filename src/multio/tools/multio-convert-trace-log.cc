#include "multio/LibMultio.h"
#include "multio/tools/MultioTool.h"

#include "eckit/log/Log.h"
#include "eckit/option/SimpleOption.h"

#include <algorithm>
#include <memory>
#include <unordered_map>
#include <vector>

#include <fcntl.h>
#include <unistd.h>

namespace {
void writeEvent(int csvFileHandle, uint64_t uniqueEventId, uint64_t traceEventId, uint64_t extraInfo,
                uint64_t correspondingStart, uint64_t timestamp) {
    std::ostringstream oss;
    oss << uniqueEventId << "," << traceEventId << "," << extraInfo << "," << correspondingStart << "," << timestamp
        << "\r\n";

    const auto line = oss.str();

    write(csvFileHandle, reinterpret_cast<const void*>(line.c_str()), line.size());
}
}  // namespace

struct Statistics {
    uint64_t numEvents;
    uint64_t totalTime;

    Statistics() : numEvents(0), totalTime(0) {}
};

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
    options_.push_back(new eckit::option::SimpleOption<bool>("stats", "Output only statistics in CSV file"));
}

void MultioConvertTraceLog::init(const eckit::option::CmdArgs& args) {}

void MultioConvertTraceLog::finish(const eckit::option::CmdArgs&) {}

void MultioConvertTraceLog::execute(const eckit::option::CmdArgs& args) {
    std::string input = "";
    args.get("input", input);

    std::string output = "sfc";
    args.get("output", output);

    bool stats = false;
    args.get("stats", stats);

    const auto csvFileHandle = open(output.c_str(), O_CREAT | O_TRUNC | O_WRONLY, S_IWUSR | S_IRUSR);

    if (!stats) {
        const std::string header("unique_id,event_id,extraInfo,start_time,end_time\r\n");
        write(csvFileHandle, reinterpret_cast<const void*>(header.c_str()), header.size());
    }
    else {
        const std::string header("unique_id,event_id,start_time,end_time,num_events,total_time\r\n");
        write(csvFileHandle, reinterpret_cast<const void*>(header.c_str()), header.size());
    }

    std::unordered_map<uint32_t, uint64_t> starts;
    std::unordered_map<uint32_t, uint64_t> ends;
    std::unordered_map<uint32_t, std::unique_ptr<Statistics>> statistics;
    uint32_t uniqueStatId = 1;

    const auto traceFileHandle = open(input.c_str(), O_RDONLY);

    const auto fileSize = lseek(traceFileHandle, 0, SEEK_END);
    lseek(traceFileHandle, 0, SEEK_SET);

    off_t processedSize = 0;
    while (processedSize < fileSize) {
        off_t sizeToRead = std::min(static_cast<off_t>(1024 * 1024 * 1024), (fileSize - processedSize));

        std::vector<uint64_t> traceData(sizeToRead / sizeof(uint64_t));

        read(traceFileHandle, reinterpret_cast<void*>(traceData.data()), sizeToRead);

        statistics.clear();

        uint64_t minTimestamp = std::chrono::system_clock::now().time_since_epoch().count();
        uint64_t maxTimestamp = 0;

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

                    if (!stats) {
                        writeEvent(csvFileHandle, uniqueEventId, traceEventId, extraInfo, timestamp, correspondingEnd);
                    }
                    else {
                        minTimestamp = std::min(minTimestamp, std::min(timestamp, correspondingEnd));
                        maxTimestamp = std::max(maxTimestamp, std::max(timestamp, correspondingEnd));

                        if (statistics.count(traceEventId) == 0) {
                            statistics[traceEventId] = std::make_unique<Statistics>();
                        }

                        statistics.at(traceEventId)->numEvents += 1;
                        statistics.at(traceEventId)->totalTime += correspondingEnd - timestamp;
                    }

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

                    if (!stats) {
                        writeEvent(csvFileHandle, uniqueEventId, traceEventId, extraInfo, correspondingStart,
                                   timestamp);
                    }
                    else {
                        minTimestamp = std::min(minTimestamp, std::min(timestamp, correspondingStart));
                        maxTimestamp = std::max(maxTimestamp, std::max(timestamp, correspondingStart));

                        if (statistics.count(traceEventId) == 0) {
                            statistics[traceEventId] = std::make_unique<Statistics>();
                        }

                        statistics.at(traceEventId)->numEvents += 1;
                        statistics.at(traceEventId)->totalTime += timestamp - correspondingStart;
                    }

                    starts.erase(uniqueEventId);
                }
            }
        }

        if (stats) {
            for (auto const& item : statistics) {
                std::ostringstream oss;
                oss << uniqueStatId << "," << item.first << "," << minTimestamp << "," << maxTimestamp << ","
                    << item.second->numEvents << "," << item.second->totalTime << "\r\n";

                const auto line = oss.str();

                write(csvFileHandle, reinterpret_cast<const void*>(line.c_str()), line.size());

                uniqueStatId += 1;
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
