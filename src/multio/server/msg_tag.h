
#ifndef multio_server_msg_tag_H
#define multio_server_msg_tag_H

namespace multio {
namespace server {

enum msg_tag
{
    message_data,
    field_data,
    step_complete,
    forecast_complete,
    open,
    close
};

}  // namespace server
}  // namespace multio

#endif
