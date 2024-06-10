#pragma once

#include <cstddef>
#include <cstdint>
#include <map>
#include <mutex>
#include <vector>
#include <pair>

namespace eckit {
class LocalConfiguration;
}

namespace multio {

namespace message {
class Message;
class Metadata;
}  // namespace message

namespace domain {



class Parametrization {
public:
    Parametrization():nClients_{0},nMessages{0},merged_{false}{};

    Parametrization(const Parametrization& rhs) = delete;
    Parametrization(Parametrization&& rhs) noexcept = delete;

    Parametrization& operator=(const Parametrization& rhs) = delete;
    Parametrization& operator=(Parametrization&& rhs) noexcept = delete;

    static Parametrization& instance();

    void add(message::Message msg);

private:

    bool allPartsArrived(const message::Message& msg) const;
    void merge();

    std::map<std::string, std::pair<std::size_t,message::Message>> messages_;

    long nClients_;
    long nMessages_;
    bool merged_;
    eckit::LocalConfiguration parametrizationData_;

    mutable std::mutex mutex_;
};

}  // namespace domain
}  // namespace multio
