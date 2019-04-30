
#ifndef multio_attic_msg_tag_H
#define multio_attic_msg_tag_H

namespace multio {
namespace attic {

enum msg_tag
{
    message_data,
    field_data,
    step_complete,
    forecast_complete,
    open,
    close
};

}  // namespace attic
}  // namespace multio

#endif
