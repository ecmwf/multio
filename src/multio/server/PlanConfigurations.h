
#ifndef multio_server_PlanConfiguration_H
#define multio_server_PlanConfiguration_H

#include <string>

namespace multio {
namespace server {

std::string plan_configurations() {
    return R"json(
     {
       "plans" : [
         { "name" : "ocean",
            "categories" : [ "surface", "deep" ],
            "actions" : {
              "aggregation" : "indexed",
              "encoding" : "none",
              "multio_sink" : "file"
            }
         },
         {
           "name" : "atmosphere",
           "categories" : [ "prognostic", "diagnostic" ],
           "actions" : {
             "aggregation" : "indexed",
             "encoding" : "none",
             "multio_sink" : "file"
           }
         }
       ]
     }
    )json";
}

}  // namespace server
}  // namespace multio

#endif
