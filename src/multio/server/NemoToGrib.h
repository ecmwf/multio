
#ifndef multio_server_NemoToGrib_H
#define multio_server_NemoToGrib_H

#include <map>
#include <string>

using NemoKey = std::string;

struct GribData {
    long param;
    std::string gridType;
    std::string levelType;
};

class NemoToGrib {
public:
    NemoToGrib();

    const GribData& get(const NemoKey& key) const;

    std::map<NemoKey, GribData> parameters_;
};

#endif
