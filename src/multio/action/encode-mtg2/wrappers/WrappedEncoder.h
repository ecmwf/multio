#pragma once

#include <string>
#include <stdexcept>
#include "multio/util/MioGribHandle.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eccodes/grib_api.h"

using eckit::LocalConfiguration;

// explicit interface to the C API for rules management
extern "C" {
  int multio_grib2_rules_open(void* options, void** handle, const char* fname, int len);
  int multio_grib2_rules_close(void** handle);
  int multio_grib2_rules_search(void* handle, void* mars_dict, char** rule_name);
  // int multio_grib2_rules_print(void* handle, const char* output_file, int len);
  // int multio_grib2_rules_size(void* handle, int64_t* num_rules, int64_t max_linear_size, int64_t* max_levels);
}

namespace multio::action::encode_mtg2::wrappers {

// Define a type for the MarsDict ID, which is used to identify the dictionary in the C API.
// using MarsDict = multio::action::encode_mtg2::wrappers::Dict<MarsDictId>;
// using ApiOptions = multio::action::encode_mtg2::wrappers::Dict<ApiOptionsId>;
class MarsDict {
  public:
    void* raw() const {return nullptr; }; // Returns the raw pointer to the underlying C API dictionary
};

class ParametrizationDict {
  public:
    void* raw() const {return nullptr; }; // Returns the raw pointer to the underlying C API dictionary
};

class GeomotryDict {
  public:
    void* raw() const {return nullptr; }; // Returns the raw pointer to the underlying C API dictionary
};

class ApiOptions {
  public:
    void* raw() const {return nullptr; }; // Returns the raw pointer to the underlying C API dictionary
};

namespace {

  /**
    * @brief Initializes the rules handle with the specified file name and API options.
    *
    * This function opens a rules file using the MultiOM C API and returns a handle to it.
    * It throws an exception if the file cannot be opened.
    *
    * @param fname The file name containing the rules.
    * @param options The API options to be used for initializing the rules handle.
    * @return A handle to the opened rules file.
    */
  void* init_wrapped_encoder_type( const std::string& encoderTye, const ApiOptions& options ) {
    void* handle = nullptr;
    int result = multio_grib2_rules_open( options.raw(), &handle, encoderTye.c_str(), static_cast<int>(encoderTye.size()) );
    if (result != 0) {
      throw eckit::Exception("Failed to open wrapped encoder: " + encoderTye, Here());
    }
    return handle;
  }

  /**
    * @brief Initializes the rules handle with the specified file name.
    *
    * This function opens a rules file using the MultiOM C API and returns a handle to it.
    * It throws an exception if the file cannot be opened.
    *
    * @param fname The file name containing the rules.
    * @return A handle to the opened rules file.
    */
  void* init_wrapped_encoder( const ApiOptions& options) {
    void* handle = nullptr;
    int result = multio_grib2_rules_open( nullptr, &handle, "default", static_cast<int>(fname.size()) );
    if (result != 0) {
      throw eckit::Exception("Failed to open wrapped encoder default ", Here());
    }
    return handle;
  }
}



/**
  * @brief A wrapper class for handling encoder rules in MultiOM.
  *
  * This class provides an interface to search for encoder rules based on a MarsDict,
  * and retrieve the associated tag, name, sample, and local configuration.
  * It manages the lifecycle of the rules handle, ensuring proper initialization and cleanup.
  * The rules are loaded from a specified file, and the search operation retrieves
  * the rule configuration as an eckit::LocalConfiguration object.
  */
class WrappedEncoder {

  private:

    /**
      * @brief Pointer to the rules handle.
      *
      * This handle is used to interact with the underlying C API for rules management.
      * It is initialized in the constructor and cleaned up in the destructor.
      * It should not be copied or assigned directly to avoid memory leaks or dangling pointers.
      * Instead, use the provided methods to manage the rules lifecycle.
      */
    void* handle_ = nullptr;

  public:

    /**
      * @brief Constructor for WrappedEncoder.
      *
      * Initializes the rules handle with the specified file name and API options.
      * This constructor is used to create an instance of WrappedEncoder with a specific configuration.
      *
      * @param fname The file name containing the rules.
      * @param opt The API options to be used for initializing the rules handle.
      */
    WrappedEncoder(const std::string& encoderTye, const ApiOptions& opt ) : handle_(init_wrapped_encoder_type(encoderTye, opt )) {};

    /**
      * @brief Constructor for WrappedEncoder.
      *
      * Initializes the rules handle with the specified file name.
      * This constructor is used to create an instance of WrappedEncoder with a specific configuration.
      *
      * @param fname The file name containing the rules.
      */
    WrappedEncoder(const ApiOptions& opt) : handle_(init_wrapped_encoder(opt)) {};

    /**
      * @brief Destructor for WrappedEncoder.
      *
      * Cleans up the rules handle by closing it if it is initialized.
      * This ensures that resources are properly released when the object goes out of scope.
      */
    ~WrappedEncoder() {
      if (handle_) {
        int result = multio_grib2_rules_close(&handle_);
      }
      handle_ = nullptr;
    };

    /**
      * @brief Searches for a rule to build the encoder in the rules handle.
      *
      * This method searches for a rule based on the provided MarsDict and retrieves
      * the associated tag, name, sample, and local configuration.
      *
      * @param mars_dict The MarsDict to search against.
      * @param tag The tag associated with the rule.
      * @param name The name of the rule.
      * @param gribHandle The sample GribHandle associated with the rule.
      * @param localConfiguration The local configuration for the encoder.
      * @return int Returns 0 on success, or an error code on failure.
      */
    multio::util::MioGribHandle searchEncoder(
      const MarsDict&              mars_dict,
      std::string&                 tag,
      std::string&                 name,
      eckit::LocalConfiguration&   localConfiguration ) {


      if (!handle_) {
        throw eckit::Exception("Rules handle is not initialized", Here());
      }
      if (!mars_dict.raw()) {
        throw eckit::Exception("MarsDict is not initialized", Here());
      }

      // Search for the rule in the rules handle using the provided MarsDict
      char* c_rule_name = nullptr;
      int result = multio_grib2_rules_search(handle_, mars_dict.raw(), &c_rule_name );
      if (result != 0) {
        throw eckit::Exception("Failed to search rules", Here());
      }

      // Convert the char* result to a std::string
      eckit::LocalConfiguration conf{eckit::YAMLConfiguration{std::string{c_rule_name}}};

      // Free the memory allocated by the C API
      free(c_rule_name);
      c_rule_name = nullptr;

      if (conf.has("name")) {
        name = conf.getString("name");
      } else {
        throw eckit::Exception("Rule configuration does not contain 'name'", Here());
      }

      if (conf.has("tag")) {
        tag = conf.getString("tag");
      } else {
        throw eckit::Exception("Rule configuration does not contain 'tag'", Here());
      }


      if (conf.has("encoder")) {
        localConfiguration = conf.getSubConfiguration("encoder");
      } else {
        throw eckit::Exception("Rule configuration does not contain 'local_configuration'", Here());
      }

      // Load the rule into an eckit::LocalConfiguration and return it to
      return { []( const eckit::LocalConfiguration& cfg ) {
          if (cfg.has("sample")) {
            return multio::util::MioGribHandle{grib_handle_new_from_samples( nullptr, cfg.getString("sample").c_str())};
          } else {
            return multio::util::MioGribHandle{grib_handle_new_from_samples( nullptr, "sample" )};
          }
        }( conf )
      };
    };

    void searchMapper(
      const MarsDict&              mars_dict,
      eckit::LocalConfiguration&   localConfiguration ) {


      if (!handle_) {
        throw eckit::Exception("Rules handle is not initialized", Here());
      }
      if (!mars_dict.raw()) {
        throw eckit::Exception("MarsDict is not initialized", Here());
      }

      // Search for the rule in the rules handle using the provided MarsDict
      char* c_rule_name = nullptr;
      int result = multio_grib2_rules_search(handle_, mars_dict.raw(), &c_rule_name );
      if (result != 0) {
        throw eckit::Exception("Failed to search rules", Here());
      }

      // Convert the char* result to a std::string
      eckit::LocalConfiguration conf{eckit::YAMLConfiguration{std::string{c_rule_name}}};

      // Free the memory allocated by the C API
      free(c_rule_name);
      c_rule_name = nullptr;

      // Get mapper

      return;
    };

    /**
      * @brief Returns the raw handle to the rules.
      *
      * This method provides direct access to the underlying rules handle.
      * It is intended for use in low-level operations where direct access is required.
      *
      * @return void* Pointer to the raw rules handle.
      */
    void* raw() {
      return handle_;
    };

    /**
      * @brief Returns the raw handle to the rules.
      *
      * This method provides direct access to the underlying rules handle.
      * It is intended for use in low-level operations where direct access is required.
      *
      * @return const void* Pointer to the raw rules handle.
      */
    const void* raw() const {
      return handle_;
    };

    /**
      * @brief Checks if the rules handle is valid.
      *
      * This method checks whether the rules handle has been successfully initialized.
      * It returns true if the handle is not null, indicating that it is valid.
      *
      * @return bool True if the rules handle is valid, false otherwise.
      */
    bool isValid() const {
      return handle_ != nullptr;
    };

    /**
      * @brief Resets the rules handle.
      *
      * This method closes the current rules handle and sets it to null.
      * It is used to clean up resources and prepare the object for reuse.
      * If the handle is already null, it does nothing.
      *
      * @throws std::runtime_error if the handle cannot be closed successfully.
      */
    void reset() {
      if (handle_) {
        int result = multio_grib2_rules_close(&handle_);
        if (result != 0) {
          throw eckit::Exception("Failed to reset rules handle", Here());
        }
        handle_ = nullptr;
      }
    };


    WrappedEncoder(const WrappedEncoder&) = delete;

    WrappedEncoder& operator=(const WrappedEncoder&) = delete;

    WrappedEncoder(WrappedEncoder&& other) noexcept : handle_(other.handle_) {
      other.handle_ = nullptr;
    };

    WrappedEncoder& operator=(WrappedEncoder&& other) noexcept {
      if (this != &other) {
        reset();
        handle_ = other.handle_;
        other.handle_ = nullptr;
      }
      return *this;
    };

    WrappedEncoder() = default;

    WrappedEncoder& operator=(const std::string& fname) {
      reset();
      handle_ = init_rules(fname);
      return *this;
    };

    WrappedEncoder(const char* fname) : WrappedEncoder(std::string(fname)) {};

};

} // namespace multio::action::encode_mtg2::wrappers