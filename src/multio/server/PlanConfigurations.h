
#ifndef multio_server_PlanConfiguration_H
#define multio_server_PlanConfiguration_H

#include <string>

namespace multio {
namespace server {

std::string thread_plan_configurations() {
    return R"json(
        {
           "transport" : "thread",
           "plans" : [
              {
                 "name" : "ocean",
                 "actions" : {
                    "root" : {
                       "type" : "Print",
                       "stream" : "debug",
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
                          "mapping" : "unstructured",
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
                       "stream" : "debug",
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
                          "mapping" : "unstructured",
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
                 "host" : "skadi",
                 "ports" : [4441, 4442, 4443, 4444, 4445]
              }
           ],
           "servers" : [
              {
                 "host" : "skadi",
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
                          "mapping" : "unstructured",
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

std::string plan_configurations(const std::string& type) {
    std::cout << "Transport type: " << type << std::endl;
    std::map<std::string, std::string> plan_configurations = {
        {"mpi", mpi_plan_configurations()},
        {"tcp", tcp_plan_configurations()},
        {"thread", thread_plan_configurations()}};

    return plan_configurations.at(type);
}

}  // namespace server
}  // namespace multio

#endif
