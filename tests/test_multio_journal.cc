
#include "TestHelpers.h"

#include "multio/DataSink.h"
#include "multio/FileSink.h"

#include "eckit/filesystem/TmpFile.h"
#include "eckit/testing/Test.h"

using namespace eckit::testing;

namespace multio {
namespace test {

CASE("test_multio_journal") {
    const eckit::PathName& file_path = eckit::TmpFile();
    eckit::LocalConfiguration config;
    config.set("path", file_path);
    std::unique_ptr<DataSink> fileSink(DataSinkFactory::instance().build("file", config));
    const char quote[] =
        "All was quiet in the deep dark wood. The mouse found a nut and the nut was good.";
    eckit::DataBlobPtr stringBlob(eckit::DataBlobFactory::build("test", quote, sizeof(quote) - 1));

    // Create journals
    Journal journal_file_sink(config, fileSink.get());
    Journal journal_no_sink(config);

    SECTION("test journals are created as closed") {
        EXPECT(!journal_no_sink.isOpen());
        EXPECT(!journal_file_sink.isOpen());
    }

    SECTION("test cannot open journal with no sink") { EXPECT_THROWS(journal_no_sink.open()); }

    SECTION("test journal only creates journal file when open") {
        EXPECT(!eckit::PathName("journal").exists());
        journal_file_sink.open();
        EXPECT(eckit::PathName("journal").exists());
    }

    SECTION("test open/close journal sets the flag correctly") {
        journal_file_sink.open();
        EXPECT(journal_file_sink.isOpen());
        journal_file_sink.close();
        EXPECT(!journal_file_sink.isOpen());
    }

    SECTION("test journal record adds write entry") {
        journal_file_sink.open();
        JournalRecord record(journal_file_sink, JournalRecord::RecordType::WriteEntry);
        record.addWriteEntry(stringBlob, fileSink->id());
        EXPECT(record.entries_.size() == 2);
    }

    SECTION("test journal replays record correctly") {
        journal_file_sink.open();
        JournalRecord record(journal_file_sink, JournalRecord::RecordType::WriteEntry);
        record.addWriteEntry(stringBlob, fileSink->id());
        EXPECT(file_content(file_path).empty());

        for (const auto& entry : record.entries_) {
            switch (entry.head_.tag_) {

            case JournalRecord::JournalEntry::Data:
                stringBlob.reset(entry.data_);
                break;

            case JournalRecord::JournalEntry::Write:
                EXPECT(stringBlob);
                fileSink->write(stringBlob);
                break;
            }
        }
        EXPECT(file_content(file_path) == std::string(quote));
    }

    eckit::PathName("journal").unlink();
}
//-----------------------------------------------------------------------------

}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return run_tests(argc, argv);
}
