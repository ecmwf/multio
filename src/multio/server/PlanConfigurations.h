
#ifndef multio_server_PlanConfiguration_H
#define multio_server_PlanConfiguration_H

#include <string>

namespace multio {
namespace server {

std::string plan_configurations() {
    return R"json(
        {
          "nemo_grid" : {
            "mapping" : "range",
            "aggregation" : "indexed",
            "encoding" : "netcdf",
            "multio_sink" : "file"
          },

          "atm_grid" : {
            "mapping" : "scattered",
            "aggregation" : "indexed",
            "encoding" : "none",
            "multio_sink" : "file"
          }
        }
    )json";
}

}  // namespace server
}  // namespace multio

#endif
