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

  using GridTypeFunction = std::function<int(codes_handle*, void*, void*)>;

  int handleReducedGG(codes_handle* h, void* mars_dict, void* par_dict) {
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
      
      ret = multio_grib2_dict_set(mars_dict, "repres", "gg");
      if(ret != 0) return ret;

      return multio_grib2_dict_set_geometry( par_dict, geom );
  }

  int handleSH(codes_handle* h, void* mars_dict, void* par_dict) {
      void* geom = NULL;
      int ret = multio_grib2_dict_create( &geom, "sh" );


      ret = getAndSet(h, geom, "pentagonalResolutionParameterJ", "pentagonal-resolution-j");
      if(ret != 0) return ret;

      ret = getAndSet(h, geom, "pentagonalResolutionParameterK", "pentagonal-resolution-k");
      if(ret != 0) return ret;

      ret = getAndSet(h, geom, "pentagonalResolutionParameterM", "pentagonal-resolution-m");
      if(ret != 0) return ret;
      
      ret = multio_grib2_dict_set(mars_dict, "repres", "sh");
      if(ret != 0) return ret;


      return multio_grib2_dict_set_geometry( par_dict, geom );
  }
  
  int handleLL(codes_handle* h, void* mars_dict, void* par_dict) {
      void* geom = NULL;
      int ret = multio_grib2_dict_create( &geom, "ll" );

      
      ret = multio_grib2_dict_set(mars_dict, "repres", "ll");
      if(ret != 0) return ret;


      return multio_grib2_dict_set_geometry( par_dict, geom );
  }

  int handleGridType(codes_handle* h, const std::string& gridType, void* mars_dict, void* par_dict) {
        const static std::unordered_map<std::string, GridTypeFunction> gridMap{
            { "reduced_gg", &handleReducedGG },
            { "regular_ll", &handleLL },
            { "sh", &handleSH },
          };

        const auto gridTypeFunc = gridMap.find(gridType);
        std::cout << "GridType: " << gridType << " " << (gridType == "reduced_gg") << std::endl;

        if (gridTypeFunc != gridMap.cend()) {
            return gridTypeFunc->second(h, mars_dict, par_dict);
        }

        std::cerr << "Unhandled gridType '" << gridType  << "'" << std::endl;
        return -1;
  };

  int handlePackingType(codes_handle* h, const std::string& packingType, void* mars_dict) {
        const static std::unordered_map<std::string, std::string> packingMap{
            { "grid_simple", "simple" },
            { "grid_complex", "complex" },
            { "spectral_complex", "complex" },
            { "ccsds", "ccsds" },
          };

        const auto packingTypeVal = packingMap.find(packingType);

        if (packingTypeVal != packingMap.cend()) {
              return multio_grib2_dict_set(mars_dict, "packing", packingTypeVal->second.c_str());
        }

        std::cerr << "Unhandled packingType '" << packingType  << "'" << std::endl;
        return -1;
  };

}

extern "C" {

int multio_grib2_encoder_extract_metadata(void* multio_grib2, void* grib, void** mars_dict, void** par_dict ) {
    int ret=0;
    codes_handle* h=NULL;

    h = (codes_handle*) grib;
    printf("extract metadat...\n");

    ret = multio_grib2_dict_create(mars_dict, "mars");
    if(ret != 0) return ret;

    ret = multio_grib2_dict_create(par_dict, "parametrization");
    if(ret != 0) return ret;

    // Handling mars keys
    ret = getAndSet(h, *mars_dict, "stream");
    if(ret != 0) return ret;

    ret = getAndSet(h, *mars_dict, "type");
    if(ret != 0) return ret;

    ret = getAndSet(h, *mars_dict, "class");
    if(ret != 0) return ret;

    if (hasKey(h, "origin")) {
        ret = getAndSet(h, *mars_dict, "origin");
        if(ret != 0) return ret;
    } else if (hasKey(h, "centre")) {
        ret = getAndSet(h, *mars_dict, "centre", "origin");
        if(ret != 0) return ret;
    }

    ret = getAndSet(h, *mars_dict, "anoffest");
    if(ret != 0) return ret;

    ret = getAndSet(h, *mars_dict, "number");
    if(ret != 0) return ret;

    ret = getAndSet(h, *mars_dict, "ident");
    if(ret != 0) return ret;

    ret = getAndSet(h, *mars_dict, "instrument");
    if(ret != 0) return ret;

    ret = getAndSet(h, *mars_dict, "channel");
    if(ret != 0) return ret;

    // TODO paramType is experimental
    ret = getAndSet(h, *mars_dict, "paramType");
    if(ret != 0) return ret;

    ret = getAndSet(h, *mars_dict, "chem");
    if(ret != 0) return ret;

    ret = getAndSet(h, *mars_dict, "paramId");
    if(ret != 0) return ret;

    ret = getAndSet(h, *mars_dict, "model");
    if(ret != 0) return ret;

    ret = getAndSet(h, *mars_dict, "levtype");
    if(ret != 0) return ret;

    ret = getAndSet(h, *mars_dict, "level", "levelist");
    if(ret != 0) return ret;

    ret = getAndSet(h, *mars_dict, "direction");
    if(ret != 0) return ret;

    ret = getAndSet(h, *mars_dict, "frequency");
    if(ret != 0) return ret;

    ret = getAndSet(h, *mars_dict, "date");
    if(ret != 0) return ret;

    if(hasKey(h,"time")) {
        ret = multio_grib2_dict_set(*mars_dict, "time", getString(h,"time").data());
        if(ret != 0) return ret;
    }

    // For some reason mars returns an empty string for step
    ret = getAndSet(h, *mars_dict, "step", "step", {"0"});
    if(ret != 0) return ret;

    ret = getAndSet(h, *mars_dict, "truncation");
    if(ret != 0) return ret;

    ret = getAndSet(h, *mars_dict, "timeproc");
    if(ret != 0) return ret;

    ret = getAndSet(h, *mars_dict, "expver");
    if(ret != 0) return ret;

    ret = getAndSet(h, *mars_dict, "grid");
    if(ret != 0) return ret;



    // Handling parametrization keys
    ret = getAndSet(h, *par_dict, "tablesversion");
    if(ret != 0) return ret;

    ret = getAndSet(h, *par_dict, "generatingprocessidentifier");
    if(ret != 0) return ret;

    ret = getAndSet(h, *par_dict, "initialstep");
    if(ret != 0) return ret;

    ret = getAndSet(h, *par_dict, "lengthoftimestepinseconds");
    if(ret != 0) return ret;

    ret = getAndSet(h, *par_dict, "lengthoftimerangeinseconds");
    if(ret != 0) return ret;

    ret = getAndSet(h, *par_dict, "valuesscalefactor");
    if(ret != 0) return ret;

    ret = getAndSet(h, *par_dict, "pv");
    if(ret != 0) return ret;

    ret = getAndSet(h, *par_dict, "numberofmissingvalues");
    if(ret != 0) return ret;

    ret = getAndSet(h, *par_dict, "valueofmissingvalues");
    if(ret != 0) return ret;

    ret = getAndSet(h, *par_dict, "systemnumber");
    if(ret != 0) return ret;

    ret = getAndSet(h, *par_dict, "methodnumber");
    if(ret != 0) return ret;

    ret = getAndSet(h, *par_dict, "typeofensembleforecast");
    if(ret != 0) return ret;

    ret = getAndSet(h, *par_dict, "numberofforecastsinensemble");
    if(ret != 0) return ret;

    ret = getAndSet(h, *par_dict, "lengthoftimewindow");
    if(ret != 0) return ret;

    ret = getAndSet(h, *par_dict, "bitspervalue");
    if(ret != 0) return ret;

    ret = getAndSet(h, *par_dict, "periodmin");
    if(ret != 0) return ret;

    ret = getAndSet(h, *par_dict, "periodmax");
    if(ret != 0) return ret;

    ret = getAndSet(h, *par_dict, "wavedirections");
    if(ret != 0) return ret;

    ret = getAndSet(h, *par_dict, "wavefrequencies");
    if(ret != 0) return ret;

    ret = getAndSet(h, *par_dict, "satelliteseries");
    if(ret != 0) return ret;

    ret = getAndSet(h, *par_dict, "scaledfactorofcentralwavenumber");
    if(ret != 0) return ret;

    ret = getAndSet(h, *par_dict, "scaledvalueofcentralwavenumber");
    if(ret != 0) return ret;

    if(hasKey(h, "setPackingType")) {
        std::string setPackingType = getString(h, "setPackingType");
        std::cout << "setPackingType: " << setPackingType << std::endl;

        ret = handlePackingType(h, setPackingType, *mars_dict);
        if(ret != 0) return ret;
    }


    if(hasKey(h, "gridType")) {
        std::string gridType = getString(h, "gridType");
        std::cout << "Grid: " << gridType << std::endl;
        
        ret = handleGridType(h, gridType, *mars_dict, *par_dict);
        if(ret != 0) return ret;
    }

    // ret = multio_grib2_dict_set(*par_dict, "geometry" geometry);
    // if(ret != 0) return ret;

    return 0;
}

}


