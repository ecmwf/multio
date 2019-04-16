
#ifndef multio_attic_PlanConfiguration_H
#define multio_attic_PlanConfiguration_H

#include <string>

namespace multio {
namespace attic {

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


    return R"json(
     {
       "plans" : [
         { "name" : "ocean",
            "categories" : [ "surface", "deep" ],
            "root" : {
            "type" : "select",
            "next" : {
                 "type" : "IndexedAggregation",
                     "next" : {
                          "type" : "print",
                     }
                }
            }
         },
         {
           "name" : "atmosphere",
           "categories" : [ "prognostic", "diagnostic" ],
           "root" : {
           }
         }
       ]
     }
    )json";

}


}  // namespace attic
}  // namespace multio

#endif
