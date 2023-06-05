namespace multio::action {
template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class FluxAverage final : public OperationWithData<T> {
public:
    using OperationWithData<T>::name_;
    using OperationWithData<T>::cfg_;
    using OperationWithData<T>::logHeader_;
    using OperationWithData<T>::values_;
    using OperationWithData<T>::win_;
    using OperationWithData<T>::checkSize;
    using OperationWithData<T>::checkTimeInterval;

    FluxAverage(const std::string& name, long sz, const MovingWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<T>{name, "average", sz, true, win, cfg} {}

    FluxAverage(const std::string& name, long sz, const MovingWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
                const StatisticsConfiguration& cfg) :
        OperationWithData<T>{name, "average", sz, true, win, IOmanager, cfg} {};

    void compute(eckit::Buffer& buf) override {
        checkTimeInterval();
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        auto val = static_cast<T*>(buf.data());
        cfg_.haveMissingValue() ? computeWithMissing(val) : computeWithoutMissing(val);
        return;
    }

    void updateData(const void* data, long sz) override {
        checkSize(sz);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const T* val = static_cast<const T*>(data);
        std::copy(val, val + sz, values_.begin());
        return;
    }

private:
    void computeWithMissing(T* buf) {
        const double m = cfg_.missingValue();
        const double c
            = static_cast<double>(1.0) / static_cast<double>(win_.count() * cfg_.stepFreq() * cfg_.timeStep());
        std::transform(values_.begin(), values_.end(), buf, [c, m](T v) { return static_cast<T>(m == v ? m : v * c); });
        return;
    }
    void computeWithoutMissing(T* buf) {
        const double c
            = static_cast<double>(1.0) / static_cast<double>(win_.count() * cfg_.stepFreq() * cfg_.timeStep());
        std::transform(values_.begin(), values_.end(), buf, [c](T v) { return static_cast<T>(v * c); });
        return;
    }
    void print(std::ostream& os) const override { os << logHeader_; }
};
}  // namespace multio::action