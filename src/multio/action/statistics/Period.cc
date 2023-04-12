
#include "Period.h"

#include <fstream>
#include <iostream>

#include "multio/LibMultio.h"

namespace multio {
namespace action {
DateTimePeriod::DateTimePeriod(const std::string& partialPath) :
    startPoint_{eckit::Date{0}, eckit::Time{0}}, endPoint_{eckit::Date{0}, eckit::Time{0}}, offset_{0} {
    long sd;
    long st;
    long ed;
    long et;
    long of;
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
    wf.close();
    if (!wf.good()) {
        std::ostringstream err;
        err << "Error occurred at writing time :: " << fname;
        throw eckit::SeriousBug("Error occurred at writing time", Here());
    }
    startPoint_ = eckit::DateTime{eckit::Date{sd}, eckit::Time{st}};
    endPoint_ = eckit::DateTime{eckit::Date{ed}, eckit::Time{et}};
    offset_ = of;
    return;
}

DateTimePeriod::DateTimePeriod(const eckit::DateTime& startPoint, eckit::Second duration, long offset) :
    startPoint_{startPoint}, endPoint_{startPoint_ + duration}, offset_{offset} {}

DateTimePeriod::DateTimePeriod(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint, long offset) :
    startPoint_{startPoint}, endPoint_{endPoint}, offset_{offset} {}

DateTimePeriod::DateTimePeriod(const eckit::DateTime& startPoint, eckit::Second duration) :
    DateTimePeriod(startPoint, duration, 0L) {}

DateTimePeriod::DateTimePeriod(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint) :
    DateTimePeriod(startPoint, endPoint, 0L) {}


void DateTimePeriod::reset(const eckit::DateTime& current) {
    auto duration = endPoint_ - startPoint_;
    startPoint_ = current;
    offset_ = 0;
    endPoint_ = startPoint_ + duration;
}

void DateTimePeriod::reset(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint) {
    offset_ = 0;
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
    return long(endPoint_ - startPoint_ + offset_);
}

eckit::DateTime DateTimePeriod::startPoint() const {
    return startPoint_;
}

eckit::DateTime DateTimePeriod::endPoint() const {
    return endPoint_;
}

void DateTimePeriod::dump(const std::string& partialPath, bool noThrow) const {
    std::ostringstream os;
    os << partialPath << "-period.bin";
    std::string fname = os.str();
    std::ofstream wf(fname, std::ios::binary);
    if (!wf) {
        if (noThrow) {
            LOG_DEBUG_LIB(LibMultio) << "Cannot open dump file: fname" << std::endl;
        }
        else {
            throw eckit::SeriousBug("Cannot open file!", Here());
        }
    }
    long dim;
    // wf.read((char*)&count_, sizeof(long));
    long sd = startPoint_.date().yyyymmdd();
    long st = startPoint_.time().hhmmss();
    long ed = endPoint_.date().yyyymmdd();
    long et = endPoint_.time().hhmmss();
    wf.write((char*)&sd, sizeof(long));
    wf.write((char*)&st, sizeof(long));
    wf.write((char*)&ed, sizeof(long));
    wf.write((char*)&et, sizeof(long));
    wf.write((char*)&offset_, sizeof(long));
    wf.close();
    if (!wf.good()) {
        if (noThrow) {
            LOG_DEBUG_LIB(LibMultio) << "Error occurred at writing time: fname" << std::endl;
        }
        else {
            throw eckit::SeriousBug("Error occurred at writing time!", Here());
        }
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

}  // namespace action
}  // namespace multio
