#ifndef multio_MaestroMetadata_H
#define multio_MaestroMetadata_H

#include "eckit/config/LocalConfiguration.h"
#include "eckit/value/Value.h"

namespace multio {

  class MaestroMetadata : protected eckit::LocalConfiguration {
      void print(std::ostream& os) const {
          os << *root_ << std::endl;
      }
      friend std::ostream& operator<<(std::ostream& os, const MaestroMetadata& md) {
          md.print(os);
          return os;
      }
 
  public:
      template <typename T>
      void setValue(const std::string& key, const T& value) {
          set(key, value);
      }
 
      template <typename T>
      T get(const std::string& key) {
          T value;
          eckit::LocalConfiguration::get(key, value);
          return value;
      }
 
      std::vector<std::string> keys() {
          return eckit::LocalConfiguration::keys();
      }
  };

}  // namespace multio

#endif
