
#ifndef multio_sandbox_PlanConfiguration_H
#define multio_sandbox_PlanConfiguration_H

#include <string>

namespace multio {
namespace sandbox {

std::string plan_configurations() {
    return R"json(
     {
       "plans" : [
         {
            "name" : "ocean",
            "categories" : [ "surface", "deep" ],
            "root" : {
              "type" : "print",
              "next" : {
                "type" : "aggregation",
                "next" : {
                  "type" : "sink"
                }
              }
            }
         },
         {
           "name" : "atmosphere",
           "categories" : [ "prognostic", "diagnostic" ],
           "root" : {
             "type" : "print"
           }
         }
       ]
     }
    )json";

}


}  // namespace sandbox
}  // namespace multio

#endif
