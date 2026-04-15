#pragma once

#include "multio/message/Metadata.h"

namespace multio::action::ifs2mars::detail {

bool isAtmosphereMetadata(const message::Metadata& md);
bool isWamMetadata(const message::Metadata& md);

void atmosphere2mars(const message::Metadata& in, message::Metadata& out);
void wam2mars(const message::Metadata& in, message::Metadata& out);

}  // namespace multio::action::ifs2mars::detail
