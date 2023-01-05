#pragma once

#include <algorithm>
#include <map>
#include <sstream>
#include <string>
#include <unordered_set>

#include "multio/maestro/MaestroMetadata.h"

namespace multio {

class CdoNamer {
public:
    CdoNamer() = default;

    std::string name(const std::map<std::string, std::string>& retrieve) const { return map_to_name(retrieve); }

    std::string name(MaestroMetadata& md) const {
        auto keys = md.keys();
        std::sort(keys.begin(), keys.end());

        std::map<std::string, std::string> m;
        for (auto& key : keys)
            m.insert({key, md.get<std::string>(key)});

        return map_to_name(m);
    }

private:
    std::string map_to_name(const std::map<std::string, std::string>& retrieve) const {
        std::map<std::string, std::string> filtered_retrieve;
        std::copy_if(retrieve.begin(), retrieve.end(), std::inserter(filtered_retrieve, filtered_retrieve.end()),
                     [this](std::pair<std::string, std::string> const& it) {
                         return ignored_keys.find(it.first) == ignored_keys.end();
                     });

        std::stringstream ss;
        auto it = filtered_retrieve.cbegin();
        ss << it->first << kv_delimiter_ << it->second;
        it++;

        for (; it != filtered_retrieve.cend(); it++)
            ss << elem_delimiter_ << it->first << kv_delimiter_ << it->second;
        return ss.str();
    }

    const std::string kv_delimiter_{":"};
    const std::string elem_delimiter_{","};
    std::unordered_set<std::string> ignored_keys{"leg"};
};

}  // namespace multio
