
#ifndef multio_server_GribTemplate_H
#define multio_server_GribTemplate_H

#include <mutex>
#include <string>
#include <vector>

namespace multio {

class Message;

namespace server {

enum SampleId : unsigned
{
    GG = 0,
    GG_ML,
    SH_ML,
    SH,
    GG2// ,
    // WAM_S,
    // WAM_I
};

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

    Message const& get(const std::string& fieldType, bool isSpectral) const;

private:  // members
    // std::vector<const metkit::grib::GribHandle*> templates_;
    std::vector<Message> templates_;
    mutable std::recursive_mutex mutex_;
};

}  // namespace server
}  // namespace multio

#endif
