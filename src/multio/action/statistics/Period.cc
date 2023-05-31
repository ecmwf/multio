
#include "Period.h"

#include <fstream>
#include <iostream>

#include "multio/LibMultio.h"

namespace multio::action {
DateTimePeriod::DateTimePeriod(const std::string& partialPath) :
    startPoint_{eckit::Date{0}, eckit::Time{0}}, endPoint_{eckit::Date{0}, eckit::Time{0}} {
    long sd;
    long st;
    long ed;
    long et;
    long of;
    long cs;
    std::ostringstream os;
    os << partialPath << "-period.bin";
    std::string fname = os.str();
    std::ifstream wf(fname, std::ios::binary);
    if (!wf) {
        std::ostringstream err;
        err << "Cannot open file :: " << fname;
        throw eckit::SeriousBug(err.str(), Here());
    }
    wf.read((char*)&sd, sizeof(long));
    wf.read((char*)&st, sizeof(long));
    wf.read((char*)&ed, sizeof(long));
    wf.read((char*)&et, sizeof(long));
    wf.read((char*)&of, sizeof(long));
    wf.read((char*)&cs, sizeof(long));
    wf.close();
    long checksum = 0;
    checksum ^= sd;
    checksum ^= st;
    checksum ^= ed;
    checksum ^= et;
    checksum ^= of;
    if (!wf.good()) {
        std::ostringstream err;
        err << "Error occurred at writing time :: " << fname;
        throw eckit::SeriousBug(err.str(), Here());
    }
    if (cs != checksum) {
        std::ostringstream err;
        err << "Error checksum not correct :: " << cs << ", " << checksum;
        throw eckit::SeriousBug(err.str(), Here());
    }
    startPoint_ = eckit::DateTime{eckit::Date{sd}, eckit::Time{st}};
    endPoint_ = eckit::DateTime{eckit::Date{ed}, eckit::Time{et}};
    return;
}

DateTimePeriod::DateTimePeriod(const eckit::DateTime& startPoint, eckit::Second duration) :
    startPoint_{startPoint}, endPoint_{startPoint_ + duration} {}

DateTimePeriod::DateTimePeriod(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint) :
    startPoint_{startPoint}, endPoint_{endPoint} {}

void DateTimePeriod::reset(const eckit::DateTime& current) {
    auto duration = endPoint_ - startPoint_;
    startPoint_ = current;
    endPoint_ = startPoint_ + duration;
}

void DateTimePeriod::reset(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint) {
    startPoint_ = startPoint;
    endPoint_ = endPoint;
}

bool DateTimePeriod::isWithin(const eckit::DateTime& dt) {
    if (startPoint_ > dt) {
        std::ostringstream os;
        os << "startPoint : " << startPoint_ << " is outside of current period " << dt << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    auto ret = (dt <= endPoint() + eckit::Second{1.0});
    LOG_DEBUG_LIB(LibMultio) << " ------ Is " << dt << " within " << *this << "? -- " << (ret ? "yes" : "no")
                             << std::endl;
    return ret;
}

long DateTimePeriod::timeSpanInSeconds() const {
    // The offset is added to fix the first timestep when no "step 0"
    // is present. Better way to handle this is through a redesign of the action
    return long(endPoint_ - startPoint_);
}

eckit::DateTime DateTimePeriod::startPoint() const {
    return startPoint_;
}

eckit::DateTime DateTimePeriod::endPoint() const {
    return endPoint_;
}

void DateTimePeriod::dump(const std::string& partialPath) const {
    std::ostringstream os;
    os << partialPath << "-period.bin";
    std::string fname = os.str();
    std::ofstream wf(fname, std::ios::binary);
    if (!wf) {
        throw eckit::SeriousBug("Cannot open file!", Here());
    }
    long dim;
    long sd = startPoint_.date().yyyymmdd();
    long st = startPoint_.time().hhmmss();
    long ed = endPoint_.date().yyyymmdd();
    long et = endPoint_.time().hhmmss();
    long checksum = 0;
    checksum ^= sd;
    checksum ^= st;
    checksum ^= ed;
    checksum ^= et;
    wf.write((char*)&sd, sizeof(long));
    wf.write((char*)&st, sizeof(long));
    wf.write((char*)&ed, sizeof(long));
    wf.write((char*)&et, sizeof(long));
    wf.write((char*)&checksum, sizeof(long));
    wf.close();
    if (!wf.good()) {
        throw eckit::SeriousBug("Error occurred at writing time!", Here());
    }
    return;
}

void DateTimePeriod::print(std::ostream& os) const {
    os << "Period(" << startPoint_ << " to " << endPoint() << ")";
}

std::ostream& operator<<(std::ostream& os, const DateTimePeriod& a) {
    a.print(os);
    return os;
}

}  // namespace multio::action
