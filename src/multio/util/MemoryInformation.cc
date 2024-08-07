#include "MemoryInformation.h"

#include "eckit/exception/Exceptions.h"

#include <regex>
#include <ios>
#include <sstream>

#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>

namespace {
    using namespace multio::util;

    const std::map<InformationTypes, std::regex> extractors = {
        { InformationTypes::PeakVirtualMemory, std::regex("VmPeak:\\s*(\\d*)\\s(\\w*)") },
        { InformationTypes::VirtualMemory, std::regex("VmSize:..\\s*(\\d*)\\s(\\w*)") },
        { InformationTypes::LockedVirtualMemory, std::regex("VmLck:..\\s*(\\d*)\\s(\\w*)") },
        { InformationTypes::PinnedVirtualMemory, std::regex("VmPin:..\\s*(\\d*)\\s(\\w*)") },
        { InformationTypes::MaximumResidentMemory, std::regex("VmHWM:..\\s*(\\d*)\\s(\\w*)") },
        { InformationTypes::ResidentMemory, std::regex("VmRSS:..\\s*(\\d*)\\s(\\w*)") },
        { InformationTypes::AnonimousResidentMemory, std::regex("RssAnon:..\\s*(\\d*)\\s(\\w*)") },
        { InformationTypes::FileMappingResidentMemory, std::regex("RssFile:..\\s*(\\d*)\\s(\\w*)") },
        { InformationTypes::SharedResidentMemory, std::regex("RssShmem:..\\s*(\\d*)\\s(\\w*)") },
        { InformationTypes::DataVirtualMemory, std::regex("VmData:..\\s*(\\d*)\\s(\\w*)") },
        { InformationTypes::StackVirtualMemory, std::regex("VmStk:..\\s*(\\d*)\\s(\\w*)") },
        { InformationTypes::TextSegmentVirtualMemory, std::regex("VmExe:..\\s*(\\d*)\\s(\\w*)") },
        { InformationTypes::SharedLibraryTextVirtualMemory, std::regex("VmLib:..\\s*(\\d*)\\s(\\w*)") },
        { InformationTypes::PageTableEntryVirtualMemory, std::regex("VmPTE:..\\s*(\\d*)\\s(\\w*)") },
        { InformationTypes::SecondLevelPageTableEntryVirtualMemory, std::regex("VmPMD:..\\s*(\\d*)\\s(\\w*)") },
        { InformationTypes::SwappedOutVirtualMemory, std::regex("VmSwap:..\\s*(\\d*)\\s(\\w*)") },
        { InformationTypes::HugeTablesMemory, std::regex("HugetlbPages:..\\s*(\\d*)\\s(\\w*)") },
    };

    InformationSizeUnits toSizeUnit(const std::string& unit) {
        switch (std::toupper(unit[0])) {
        case 'K':
            return InformationSizeUnits::KiloBytes;
        case 'M':
            return InformationSizeUnits::MegaBytes;
        case 'G':
            return InformationSizeUnits::GigaBytes;
        default:
            return InformationSizeUnits::Bytes;
        }
    }

    std::map<InformationTypes, InformationItem> createMemoryInformation() {
        const auto maximumStatusStringLengthInChars = 16 * 1024;

        std::map<InformationTypes, InformationItem> information;
        int fd = -1;

        try {
            const auto path = "/proc/self/status";

            fd = open(path, O_RDONLY);
            if (fd < 0) {
                throw eckit::FailedSystemCall("open", Here());
            }

            auto mem = reinterpret_cast<char *>(alloca(maximumStatusStringLengthInChars));

            ssize_t read_bytes = read(fd, mem, maximumStatusStringLengthInChars);
            ssize_t already_read = read_bytes;
            while ((read_bytes > 0) && (already_read < maximumStatusStringLengthInChars)) {
                read_bytes = read(fd, mem + already_read, maximumStatusStringLengthInChars - already_read);
                if ((read_bytes < 0) && (errno != EAGAIN)) {
                    throw eckit::FailedSystemCall("read", Here());
                }

                already_read += read_bytes;
            }

            close(fd);
            fd = -1;

            auto statusContents = std::istringstream(mem);

            auto line = reinterpret_cast<char *>(alloca(maximumStatusStringLengthInChars));
            while (statusContents.getline(line, maximumStatusStringLengthInChars)) {
                const auto lineText = std::string(line);
                for (auto const& extractor : extractors) {
                    std::smatch match;
                    if (std::regex_search(lineText, match, extractor.second)) {
                        information.emplace(extractor.first, 
                            InformationItem { std::atoi(match[1].str().c_str()), toSizeUnit(match[2].str()) });
                    }
                }
            }
        } catch (...) {
            if (fd >= 0) {
                close(fd);
                fd = -1;
            }

            throw;
        }

        return information;
    }
}

namespace multio::util {

MemoryInformation::MemoryInformation() : memInfo_(createMemoryInformation()) {}

std::vector<InformationTypes> MemoryInformation::getAvailableKeys() const {
    std::vector<InformationTypes> keys;
    keys.reserve(memInfo_.size());

    for (const auto& item : memInfo_) {
        keys.emplace_back(item.first);
    }

    return keys;
}

}
