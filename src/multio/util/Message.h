#include "multio/message/Message.h"

namespace multio {

template <typename A, typename B>
message::Message convert_precision(message::Message&& msg) {
    const size_t N = msg.payload().size() / sizeof(A);
    eckit::Buffer buffer(N * sizeof(B));

    auto md = msg.metadata();
    md.set("globalSize", buffer.size());
    md.set("precision", std::is_same<B, double>::value ? "double" : std::is_same<B, float>::value ? "single" : NOTIMP);

    const auto* a = reinterpret_cast<const A*>(msg.payload().data());
    auto* b = reinterpret_cast<B*>(buffer.data());
    for (size_t i = 0; i < N; ++i) {
        *(b++) = static_cast<B>(*(a++));
    }

    return {message::Message::Header{msg.tag(), msg.source(), msg.destination(), std::move(md)}, std::move(buffer)};
}
}  // namespace multio