#include "multio/action/statistics/cfg/StatisticsConfiguration.h"



namespace multio::action {


StatisticsConfiguration::StatisticsConfiguration( const message::Message& msg, const StatisticsOptions& opt ):
    opt_{opt},
    date_{0},
    time_{0},
    level_{0},
    timeStep_{0},
    stepFreq_{0},
    step_{0},
    param_{"none"},
    levType_{"none"},
    gridType_{"none"},
    precision_{"none"},
    bitmapPresent_{false},
    missingValue_{std::numeric_limits<double>::quiet_NaN()},
    key_{"unknown"},
    logPrefix_{opt_.logPrefix()}{

    // Associate local procedure pointers
    computeEpoch_    = std::bind(&StatisticsConfiguration::computeEpoch, this);
    computePrev_     = std::bind(&StatisticsConfiguration::computePrev, this);
    computeCurr_     = std::bind(&StatisticsConfiguration::computeCurr, this);
    computeNext_     = std::bind(&StatisticsConfiguration::computeNext, this);
    computeWinStart_ = std::bind(&StatisticsConfiguration::computeWinStart, this);
    computeBeginningOfHour_  = std::bind(&StatisticsConfiguration::computeBeginningOfHour, this);
    computeBeginningOfDay_   = std::bind(&StatisticsConfiguration::computeBeginningOfDay, this);
    computeBeginningOfMonth_ = std::bind(&StatisticsConfiguration::computeBeginningOfMonth, this);
    computeBeginningOfYear_  = std::bind(&StatisticsConfiguration::computeBeginningOfYear, this);

    // Extraact message metadata
    const auto& md = msg.metadata();

    // Read the metadata
    readMetadata( md );

    // Generate Key
    generateKey( md, std::to_string(std::hash<std::string>{}(msg.source())) );

    return;
};


const std::string& StatisticsConfiguration::key() const{
    return key_;
};


void StatisticsConfiguration::readMetadata( const message::Metadata& md ){

    // date_     = md.dataDate.value();
    // time_     = md.dataTime.value();
    // level_    = md.level.value();
    // timeStep_ = md.timeStep.value();
    // stepFreq_ = md.stepFrequency.value();
    // step_     = md.step.value();
    // param_    = md.param.value();


    date_      = md.getLong( opt_.dateKey() );
    time_      = md.getLong( opt_.timeKey() );
    level_     = md.getLong( opt_.levelKey() );
    timeStep_  = md.getLong( opt_.timeStepKey(), opt_.timeStep() );
    stepFreq_  = md.getLong( opt_.stepFreqKey(), opt_.stepFreq() );
    step_      = md.getLong( opt_.stepKey() );
    param_     = md.getString( opt_.paramKey() );
    levType_   = md.getString( opt_.levTypeKey() );
    gridType_  = md.getString( opt_.gridTypeKey() );
    precision_ = md.getString( opt_.precisionKey() );
    bitmapPresent_ = md.getBool( opt_.bitmapPresentKey(), false );
    missingValue_  = md.getDouble( opt_.missingValueKey(), std::numeric_limits<double>::quiet_NaN() );

    // Exit point
    return;
};


void StatisticsConfiguration::generateKey( const message::Metadata& md, const std::string& src ){

    std::ostringstream os;
    os << param_ << "-"
       << level_ << "-"
       << levType_ << "-"
       << gridType_ << "-"
       << precision_ << "-"
       << src;
    key_ = os.str();

    // Exit point
    return;
};


eckit::DateTime StatisticsConfiguration::epoch() const {
    epoch_ = computeEpoch_( );
    return epoch_;
};

eckit::DateTime StatisticsConfiguration::prev()const {
    prev_ = computePrev_( );
    return prev_;
};

eckit::DateTime StatisticsConfiguration::curr()const {
    curr_ = computeCurr_( );
    return curr_;
};

eckit::DateTime StatisticsConfiguration::next()const {
    next_ = computeNext_( );
    return next_;
};

eckit::DateTime StatisticsConfiguration::winStart()const {
    winStart_ = computeWinStart_( );
    return winStart_;
};

bool StatisticsConfiguration::beginningOfHour()const {
    auto ret = computeBeginningOfHour_( );
    return ret;
};

bool StatisticsConfiguration::beginningOfDay()const {
    auto ret = computeBeginningOfDay_( );
    return ret;
};

bool StatisticsConfiguration::beginningOfMonth()const {
    auto ret = computeBeginningOfMonth_( );
    return ret;
};

bool StatisticsConfiguration::beginningOfYear()const {
    auto ret = computeBeginningOfYear_( );
    return ret;
};




eckit::DateTime StatisticsConfiguration::computeEpoch() const {
    eckit::Date startDate{date_};
    auto hour = time_ / 10000;
    auto minute = (time_ % 10000) / 100;
    epoch_ = eckit::DateTime{startDate, eckit::Time{hour, minute, 0}};
    computeEpoch_ = std::bind(&StatisticsConfiguration::getEpoch, this);
    return epoch_;
};

eckit::DateTime StatisticsConfiguration::getEpoch() const{
    return epoch_;
};


eckit::DateTime StatisticsConfiguration::computePrev() const{
    prev_ = epoch() + static_cast<eckit::Second>(std::max((step_ - stepFreq_), 0L) * timeStep_);
    computePrev_ = std::bind(&StatisticsConfiguration::getPrev, this);
    return prev_;
};

eckit::DateTime StatisticsConfiguration::getPrev() const{
    return prev_;
};


eckit::DateTime StatisticsConfiguration::computeCurr() const{
    curr_ = epoch() + static_cast<eckit::Second>(std::max((step_), 0L) * timeStep_);
    computeCurr_ = std::bind(&StatisticsConfiguration::getCurr, this);
    return curr_;
};

eckit::DateTime StatisticsConfiguration::getCurr() const{
    return curr_;
};


eckit::DateTime StatisticsConfiguration::computeNext() const{
    next_ = epoch() + static_cast<eckit::Second>((step_ + stepFreq_) * timeStep_);
    computeNext_ = std::bind(&StatisticsConfiguration::getNext, this);
    return next_;
};

eckit::DateTime StatisticsConfiguration::getNext() const{
    return next_;
};


eckit::DateTime StatisticsConfiguration::computeWinStart() const{
    winStart_ = opt_.solver_send_initial_condition() ? curr() : prev();
    computeWinStart_ = std::bind(&StatisticsConfiguration::getWinStart, this);
    return winStart_;
};

eckit::DateTime StatisticsConfiguration::getWinStart() const{
    return winStart_;
};




bool StatisticsConfiguration::bitmapPresent() const {
  return bitmapPresent_;
};


double StatisticsConfiguration::missingValue() const {
    return missingValue_;
};


std::string StatisticsConfiguration::logPrefix() const {
    return logPrefix_;
};






bool StatisticsConfiguration::computeBeginningOfHour() const{
    eckit::DateTime now  = curr();
    long min = now.time().minutes();
    long sec = now.time().seconds();
    beginningOfHour_ = (min == 0 && sec == 0);
    computeBeginningOfHour_ = std::bind(&StatisticsConfiguration::isBeginningOfHour, this);
    return beginningOfHour_;
};

bool StatisticsConfiguration::isBeginningOfHour () const{
    return beginningOfHour_;
};

bool StatisticsConfiguration::computeBeginningOfDay() const{
    eckit::DateTime now  = curr();
    long hour = now.time().hours();
    long min = now.time().minutes();
    long sec = now.time().seconds();
    beginningOfDay_= (hour == 0 && min == 0 && sec == 0);
    computeBeginningOfDay_ = std::bind(&StatisticsConfiguration::isBeginningOfDay, this);
    return beginningOfDay_;
};

bool StatisticsConfiguration::isBeginningOfDay () const{
    return beginningOfDay_;
};

bool StatisticsConfiguration::computeBeginningOfMonth() const{
    eckit::DateTime now  = curr();
    long day = now.date().day();
    long hour = now.time().hours();
    long min = now.time().minutes();
    long sec = now.time().seconds();
    beginningOfMonth_ = (day == 1 && hour == 0 && min == 0 && sec == 0);
    computeBeginningOfMonth_ = std::bind(&StatisticsConfiguration::isBeginningOfMonth, this);
    return beginningOfMonth_;
};

bool StatisticsConfiguration::isBeginningOfMonth () const{
    return beginningOfMonth_;
};

bool StatisticsConfiguration::computeBeginningOfYear() const{
    eckit::DateTime now  = curr();
    long month = now.date().month();
    long day = now.date().day();
    long hour = now.time().hours();
    long min = now.time().minutes();
    long sec = now.time().seconds();
    beginningOfYear_ = (month == 1 && day == 1 && hour == 0 && min == 0 && sec == 0);
    computeBeginningOfYear_ = std::bind(&StatisticsConfiguration::isBeginningOfYear, this);
    return beginningOfYear_;
};

bool StatisticsConfiguration::isBeginningOfYear () const{
    return beginningOfYear_;
};


long StatisticsConfiguration::date() const { return date_; };
long StatisticsConfiguration::time() const {return time_; };
long StatisticsConfiguration::level() const { return level_; };
long StatisticsConfiguration::timeStep() const { return timeStep_; };
long StatisticsConfiguration::stepFreq() const { return stepFreq_; };
long StatisticsConfiguration::step() const { return step_; };

std::string StatisticsConfiguration::param() const { return param_; };
std::string StatisticsConfiguration::levType() const { return levType_; };
std::string StatisticsConfiguration::gridType() const { return gridType_; };
std::string StatisticsConfiguration::precision() const { return precision_; };
const StatisticsOptions& StatisticsConfiguration::options() const { return opt_; };
}