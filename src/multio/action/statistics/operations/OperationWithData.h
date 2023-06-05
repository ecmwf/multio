namespace multio::action {
template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class OperationWithData : public OperationBase {
public:
    using OperationBase::cfg_;
    using OperationBase::logHeader_;
    using OperationBase::name_;

    OperationWithData(const std::string& name, const std::string& operation, long sz, bool needRestart,
                      const MovingWindow& win, const StatisticsConfiguration& cfg) :
        OperationBase{name, operation, win, cfg},
        needRestart_{needRestart},
        values_{std::vector<T>(sz /= sizeof(T), 0.0)} {}

    OperationWithData(const std::string& name, const std::string& operation, long sz, bool needRestart,
                      const MovingWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
                      const StatisticsConfiguration& cfg) :
        OperationBase{name, operation, win, cfg},
        needRestart_{needRestart},
        values_{std::vector<T>(sz /= sizeof(T), 0.0)} {
        load(IOmanager, cfg);
        return;
    }

    void updateWindow(const void* data, long sz) override {
        std::transform(values_.begin(), values_.end(), values_.begin(), [](T v) { return static_cast<T>(0.0); });
        return;
    };

    void init(const void* data, long sz) override {
        // TODO: Used to save the first field of the window
        return;
    };

    void init() override {
        // TODO: Used to save the initialization time of the window
        return;
    };

    size_t byte_size() const override { return values_.size() * sizeof(T); };

    void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) const override {
        if (needRestart_) {
            std::vector<double> data(values_.size());
            serialize(data);
            IOmanager->writeOperation(name_, data);
            IOmanager->flush();
        }
        return;
    };

    void load(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) override {
        if (needRestart_) {
            std::vector<double> data(values_.size());
            IOmanager->readOperation(name_, data);
            deserialize(data);
        }
        return;
    };

protected:
    void serialize(std::vector<double>& data) const {
        std::transform(values_.cbegin(), values_.cend(), data.begin(), [](double v) { return static_cast<double>(v); });
        return;
    };

    void deserialize(const std::vector<double>& data) {
        std::transform(data.cbegin(), data.cend(), values_.begin(), [](T v) { return static_cast<T>(v); });
        return;
    };

    void checkSize(long sz) {
        if (values_.size() != static_cast<long>(sz / sizeof(T))) {
            throw eckit::AssertionFailed(logHeader_ + " :: Expected size: " + std::to_string(values_.size())
                                         + " -- actual size: " + std::to_string(sz));
        }
    };

    void checkTimeInterval() {
        long sec = win_.count() * cfg_.stepFreq() * cfg_.timeStep();
        if (sec == 0) {
            throw eckit::SeriousBug{logHeader_ + " :: Divide by zero", Here()};
        }
        return;
    };

    std::vector<T> values_;
    bool needRestart_;
};
}  // namespace multio::action