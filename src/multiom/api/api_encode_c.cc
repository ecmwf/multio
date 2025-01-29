#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <iostream>
#include <sstream>
#include <unordered_map>
#include <vector>
#include <functional>


#include "c/api.h"
#include "eccodes.h"

namespace {
  bool hasKey(codes_handle* handle, const char* key) {
      return codes_is_defined(handle, key) != 0;
  }
  
  std::string getString(codes_handle* handle, const char* key) {
      // char keyval[1024];
      // size_t keylen = sizeof(keyval);
      // CODES_CHECK(codes_get_string(handle, key, keyval, &keylen), nullptr);
      std::string ret;
      std::size_t keylen=1024;
      // Use resize instead of reserive - will allocate enough memory and sets the size internally to the string
      ret.resize(keylen);
      // Now eccodes is writing in the buffer...
      CODES_CHECK(codes_get_string(handle, key, ret.data(), &keylen), nullptr);
      // Now a second resize will only shrink (never enlarge) the buffer and just adjust the size without modifying the buffer
      ret.resize(keylen-1);
      std::cout << "Get string " << key << " (len: " << keylen << ", stringsize: " << ret.size() << ": " << std::string(ret) <<std::endl;
      return ret;                              
  }

  long getLong(codes_handle* handle, const char* key) {
      long ret;
      CODES_CHECK(codes_get_long(handle, key, &ret), nullptr);
      return ret;                              
  }
  
  std::size_t getSize(codes_handle* handle, const char* key) {
      std::size_t ret;
      CODES_CHECK(codes_get_size(handle, key, &ret), nullptr);
      return ret;                              
  }
  
  std::vector<double> getDoubleArray(codes_handle* handle, const char* key) {
      std::vector<double> ret;
      std::size_t size = getSize(handle, key);
      ret.resize(size);
      CODES_CHECK(codes_get_double_array(handle, key, ret.data(), &size), nullptr);
      ret.resize(size);
      return ret;                              
  }
  
  std::vector<long> getLongArray(codes_handle* handle, const char* key) {
      std::vector<long> ret;
      std::size_t size = getSize(handle, key);
      ret.resize(size);
      CODES_CHECK(codes_get_long_array(handle, key, ret.data(), &size), nullptr);
      ret.resize(size);
      return ret;                              
  }
  
  
  int getAndSet(codes_handle* h, void* dict, const char* key, const char* setName=NULL, std::optional<std::string> defaultVal = {}) {
    if(hasKey(h, key)) {
        std::string val = getString(h, key);
        if (val.empty() && defaultVal) {
            val = *defaultVal;
        }
        int ret = multio_grib2_dict_set(dict, setName == NULL ? key : setName, val.data());
        if(ret != 0) return ret;
    }
    return 0;
  }
  
  template<typename T>
  std::string arrayToJSONString(const std::vector<T>& arr) {
    std::ostringstream oss;
    bool first=true;
    oss << "[";
    for(const auto& v: arr) {
        if (first) {
            first = false;
        } else {
            oss << ", ";
        }
        oss << v;
    }
    oss << "]";
    
    std::cout << "Array: " << oss.str() << std::endl;
    
    return oss.str();
  }
  
  int getAndSetDoubleArray(codes_handle* h, void* dict, const char* key, const char* setName=NULL) {
    if(hasKey(h, key)) {
        int ret = multio_grib2_dict_set(dict, setName == NULL ? key : setName, arrayToJSONString(getDoubleArray(h, key)).data());
        if(ret != 0) return ret;
    }
    return 0;
  }
  
  int getAndSetLongArray(codes_handle* h, void* dict, const char* key, const char* setName=NULL) {
    if(hasKey(h, key)) {
        int ret = multio_grib2_dict_set(dict, setName == NULL ? key : setName, arrayToJSONString(getLongArray(h, key)).data());
        if(ret != 0) return ret;
    }
    return 0;
  }
  
  using GridTypeFunction = std::function<int(codes_handle*, void*)>;
  
  int handleReducedGG(codes_handle* h, void* par_dict) {
      void* geom = NULL;
      int ret = multio_grib2_dict_create( &geom, "reduced-gg" );
      
      
      ret = getAndSet(h, geom, "truncateDegrees", "truncate-degrees");
      if(ret != 0) return ret;
      
      ret = getAndSet(h, geom, "numberOfPointsALongAMeridian", "number-of-points-along-a-meridian");
      if(ret != 0) return ret;
      
      ret = getAndSet(h, geom, "numberOfParallelsBetweenPoleAndEquator", "number-of-parallels-between-pole-and-equator");
      if(ret != 0) return ret;
      
      ret = getAndSet(h, geom, "latitudeOfFirstGridPointInDegrees", "latitude-of-first-grid-point-in-degrees");
      if(ret != 0) return ret;
      
      ret = getAndSet(h, geom, "longitudeOfFirstGridPointInDegrees", "longitude-of-first-grid-point-in-degrees");
      if(ret != 0) return ret;
      
      ret = getAndSet(h, geom, "latitudeOfLastGridPointInDegrees", "latitude-of-last-grid-point-in-degrees");
      if(ret != 0) return ret;
      
      ret = getAndSet(h, geom, "longitudeOfLastGridPointInDegrees", "longitude-of-last-grid-point-in-degrees");
      if(ret != 0) return ret;
      
      ret = getAndSetLongArray(h, geom, "pl", "pl");
      if(ret != 0) return ret;
      
      
      
      return multio_grib2_dict_set_geometry( par_dict, geom );
  }

  int handleSH(codes_handle* h, void* par_dict) {
      void* geom = NULL;
      int ret = multio_grib2_dict_create( &geom, "sh" );


      ret = getAndSet(h, geom, "pentagonalResolutionParameterJ", "pentagonal-resolution-j");
      if(ret != 0) return ret;
      
      ret = getAndSet(h, geom, "pentagonalResolutionParameterK", "pentagonal-resolution-k");
      if(ret != 0) return ret;
      
      ret = getAndSet(h, geom, "pentagonalResolutionParameterM", "pentagonal-resolution-m");
      if(ret != 0) return ret;


      return multio_grib2_dict_set_geometry( par_dict, geom );
  }
  
  int handleGridType(codes_handle* h, const std::string& gridType, void* par_dict) {
        const static std::unordered_map<std::string, GridTypeFunction> gridMap{
            { "reduced_gg", &handleReducedGG },
            { "sh", &handleSH }
          };

        const auto gridTypeFunc = gridMap.find(gridType);
        std::cout << "GridType: " << gridType << " " << (gridType == "reduced_gg") << std::endl;

        if (gridTypeFunc != gridMap.cend()) {
            return gridTypeFunc->second(h, par_dict);
        }
        
        std::cerr << "Unhandled gridType '" << gridType  << "'" << std::endl; 
        return -1;
  };
  
}

extern "C" {

int multio_grib2_encoder_encode64(void* multio_grib2, void* mars_dict, void* par_dict, double* data, size_t data_len,
                                  void** out_handle) {
  void* gribMessage;
  std::size_t numBytes;
  int ret = multio_grib2_encoder_encode_f(multio_grib2, mars_dict, par_dict, &gribMessage, &numBytes);
  if(ret !=0) return ret;
  *out_handle = codes_handle_new_from_message(NULL, gribMessage, numBytes);
  if (*out_handle == NULL) { return -1; };
  free(gribMessage); 
  return ret;
}
                                  
                                  
int multio_grib2_encoder_encode32(void* multio_grib2, void* mars_dict, void* par_dict, float* data, size_t data_len,
                                  void** out_handle) {
    std::vector<double> values{data, data+data_len};
    return multio_grib2_encoder_encode64(multio_grib2, mars_dict, par_dict, values.data(), data_len, out_handle);
}

      
      
}


