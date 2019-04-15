
#ifndef multio_sandbox_PlanConfiguration_H
#define multio_sandbox_PlanConfiguration_H

#include <string>

namespace multio {
namespace sandbox {

std::string mpi_plan_configurations() {
    return R"json(
        {
           "transport" : "mpi",
           "domain" : "world",
           "plans" : [
              {
                 "name" : "ocean",
                 "actions" : {
                    "root" : {
                       "type" : "Print",
                       "stream" : "error",
                       "next" : {
                          "type" : "AppendToFile",
                          "path" : "messages.txt"
                       }
                    }
                 }
              },
              {
                 "name" : "atmosphere",
                 "actions" : {
                    "root" : {
                       "type" : "Select",
                       "categories" : [ "prognostic", "diagnostic" ],
                       "next" : {
                          "type" : "Aggregation",
                          "mapping" : "scattered",
                          "next" : {
                             "type" : "Encode",
                             "format" : "grib",
                             "next" : {
                                "type" : "Sink",
                                "datasink" : "file"
                             }
                          }
                       }
                    }
                 }
              }
           ]
        }
    )json";
}

std::string tcp_plan_configurations() {
    return R"json(
        {
           "transport" : "tcp",
           "clients" : [
              {
                 "host" : "class11",
                 "ports" : [4441, 4442, 4443, 4444, 4445]
              }
           ],
           "servers" : [
              {
                 "host" : "class11",
                 "ports" : [7771, 7772, 7773]
              }
           ],
           "plans" : [
              {
                 "name" : "atmosphere",
                 "actions" : {
                    "root" : {
                       "type" : "Select",
                       "categories" : [ "prognostic", "diagnostic" ],
                       "next" : {
                          "type" : "Aggregation",
                          "mapping" : "scattered",
                          "next" : {
                             "type" : "Encode",
                             "format" : "grib",
                             "next" : {
                                "type" : "Sink",
                                "datasink" : "file"
                             }
                          }
                       }
                    }
                 }
              }
           ]
        }
    )json";
}

}  // namespace sandbox
}  // namespace multio

#endif
