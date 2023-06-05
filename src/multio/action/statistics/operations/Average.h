namespace multio::action {
template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class Average final : public OperationWithData<T> {
public:
    using OperationWithData<T>::name_;
    using OperationWithData<T>::cfg_;
    using OperationWithData<T>::logHeader_;
    using OperationWithData<T>::values_;
    using OperationWithData<T>::win_;
    using OperationWithData<T>::checkSize;
    using OperationWithData<T>::checkTimeInterval;

    Average(const std::string& name, long sz, const MovingWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<T>{name, "average", sz, true, win, cfg} {}

    Average(const std::string& name, long sz, const MovingWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
            const StatisticsConfiguration& cfg) :
        OperationWithData<T>{name, "average", sz, true, win, IOmanager, cfg} {};

    void compute(eckit::Buffer& buf) override {
        checkTimeInterval();
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        buf.copy(values_.data(), values_.size() * sizeof(T));
        return;
    }

    void updateData(const void* data, long sz) override {
        checkSize(sz);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const T* val = static_cast<const T*>(data);
        cfg_.haveMissingValue() ? updateWithMissing(val) : updateWithoutMissing(val);
        return;
    }

private:
    void updateWithoutMissing(const T* val) {
        const double c2 = icntpp(), c1 = sc(c2);
        std::transform(values_.begin(), values_.end(), val, values_.begin(),
                       [c1, c2](T v1, T v2) { return static_cast<T>(v1 * c1 + v2 * c2); });
        return;
    }
    void updateWithMissing(const T* val) {
        const double c2 = icntpp(), c1 = sc(c2), m = cfg_.missingValue();
        std::transform(values_.begin(), values_.end(), val, values_.begin(),
                       [c1, c2, m](T v1, T v2) { return static_cast<T>(m == v2 ? m : v1 * c1 + v2 * c2); });
        return;
    }
    double icntpp() const { return double(1.0) / double(win_.count()); };
    double sc(double v) const { return double(win_.count() - 1) * v; };
    void print(std::ostream& os) const override { os << logHeader_; }
};
}  // namespace multio::action