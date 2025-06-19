#ifndef MULTIOM_RAW_API_RULES_H
#define MULTIOM_RAW_API_RULES_H

#include "rule_api.h"
#include <string>
#include <stdexcept>

namespace {
  void* init_rules( const std::string& fname ) {
    void* handle = nullptr;
    int result = multio_grib2_rules_open( nullptr, &handle, fname.c_str(), static_cast<int>(fname.size()) );
    if (result != 0) {
      throw std::runtime_error("Failed to open rules file: " + fname);
    }
    return handle;
  }
}

class LocalConfiguration;

class EncoderRules {

  private:
    void* handle = nullptr;
  public:

    EncoderRules(const std::string& fname) : handle(init_rules(fname)) {};

    ~EncoderRules() {
      if (handle) {
        int result = multio_grib2_rules_close(&handle);
        if (result != 0) {
          throw std::runtime_error("Failed to close rules handle");
        }
      }
    };

    int search( void* mars_dict, char** tag, char** name, char** sample_name, LocalConfiguration* localConfiguration ) {
      if (!handle) {
        throw std::runtime_error("Rules handle is not initialized");
      }
      void* tmp;
      int result = multio_grib2_rules_serch(handle, mars_dict, tag, name, sample_name, &tmp );
      if (result != 0) {
        throw std::runtime_error("Failed to search rules");
      }
      localConfiguration = static_cast<LocalConfiguration*>(tmp);
      return result;
    }
};


#endif