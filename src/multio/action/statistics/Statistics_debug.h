#pragma once

#define DEVELOPER_MODE 1


// Managing logging stuff
// In developer mode all possible logging information is active and redirected on std::err
// No matter if debug mode is active.
// With developer mode on and MULTIO_DEBUG=0 it is easier to spot the messages from only this action
#if DEVELOPER_MODE == 0
#define STATISTICS_OUT_STREAM LOG_DEBUG_LIB(LibMultio)
#else
#define STATISTICS_OUT_STREAM std::cerr << " + STATISTICS_ACTION: "
#endif