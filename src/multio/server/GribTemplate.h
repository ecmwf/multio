
#ifndef multio_server_GribTemplate_H
#define multio_server_GribTemplate_H

#include <mutex>
#include <string>
#include <vector>

namespace multio {
namespace server {

class Message;

class GribTemplate {
public:  // methods
    GribTemplate() = default;

    GribTemplate(const GribTemplate& rhs) = delete;
    GribTemplate(GribTemplate&& rhs) noexcept = delete;

    GribTemplate& operator=(const GribTemplate& rhs) = delete;
    GribTemplate& operator=(GribTemplate&& rhs) noexcept = delete;

    static GribTemplate& instance();

    void add(Message msg);

    void list(std::ostream&) const;

    Message const& get(const size_t sample_id) const;

private:  // members
    // std::vector<const metkit::grib::GribHandle*> templates_;
    std::vector<Message> templates_;
    mutable std::recursive_mutex mutex_;
};

}  // namespace server
}  // namespace multio

#endif
