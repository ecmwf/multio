// Dictionary.hpp
#pragma once

#include <string>
#include <vector>
#include <stdexcept>
#include <iostream>
#include <variant>

extern "C" {

  // General management
  int dict_create(const char* type, void** dict);
  int dict_destroy(void* dict);
  int dict_reset(void* dict);

  // General utils functions
  int dict_get_number_of_keys(void* dict, int* count);
  int dict_get_number_of_vals(void* dict, int* count);
  int dict_get_index_from_key(void* dict, const char* key, int* index);
  int dict_get_key_len_from_index(void* dict, int id, int* len);
  int dict_get_key_name_from_index(void* dict, int id, const char* key, int len);

  int dict_key_is_valid(void* dict, int key, int* is_valid);
  int dict_val_get_native_type(void* dict, int key, int* type);
  int dict_val_has(void* dict, int key, int* has);
  int dict_val_get_rank(void* dict, int key, int* rank);
  int dict_val_get_size(void* dict, int key, int* size);
  int dict_val_is_range(void* dict, int key, int* is_range);


  int dict_set_missing(void* dict, int key);


  int dict_set_string(void* dict, int key, const char* val);
  int dict_set_string_array_copy(void* dict, int key, const char** val, int size);
  int dict_set_int64(void* dict, int key, std::int64_t val);
  int dict_set_range(void* dict, int key, std::int64_t val1, std::int64_t val2);
  int dict_set_real64(void* dict, int key, double val);
  int dict_set_int64_array_copy(void* dict, int key, const std::int64_t* val, int size);
  int dict_set_real64_array_copy(void* dict, int key, const double* val, int size);


  int dict_get_string(void* dict, int key, char* val, int len);
  int dict_get_string_length(void* dict, int key, int* len);
  int dict_get_string_length_by_id(void* dict, int key, int index, int* len);
  int dict_get_string_by_id(void* dict, int key, int index, char* val, int len);
  int dict_get_int64(void* dict, int key, std::int64_t* val);
  int dict_get_range( void* dict, int key, std::int64_t* val1, std::int64_t* val2);
  int dict_get_real64(void* dict, int key, double* val);
  int dict_get_int64_array(void* dict, int key, std::int64_t* val);
  int dict_get_real64_array(void* dict, int key, double* val);


  // General iterators management
  int dict_key_iterator_begin(void*, int* iterator);
  int dict_keyval_iterator_begin(void*, int* iterator);

  int dict_key_iterator_next(void*, int* iterator);
  int dict_keyval_iterator_next(void*, int* iterator);

  int dict_key_iterator_prev(void*, int* iterator);
  int dict_keyval_iterator_prev(void*, int* iterator);

  int dict_key_iterator_end(void*, int* iterator);
  int dict_keyval_iterator_end(void*, int* iterator);

}

class Dictionary {
public:

  explicit Dictionary(const std::string& dict_type) {
    if (dict_create(dict_type.c_str(), &dict_) != 0) {
      throw std::runtime_error("Failed to create dictionary");
    }
  }

  Dictionary(Dictionary&& other) noexcept : dict_(other.dict_) {
    other.dict_ = nullptr;
  }

  Dictionary& operator=(Dictionary&& other) noexcept {
    if (this != &other) {
      if (dict_) dict_destroy(dict_);
      dict_ = other.dict_;
      other.dict_ = nullptr;
    }
    return *this;
  }

  Dictionary(const Dictionary&) = delete;
  Dictionary& operator=(const Dictionary&) = delete;

  ~Dictionary() {
    // Don't throw in the destructor because it may be called during stack unwinding
    if (dict_ && dict_destroy(dict_) != 0) {
      std::cerr << "Failed to destroy dictionary" << std::endl;
    }
  }

  std::vector<std::string> list() const {
    int number_of_keys;
    if (dict_get_number_of_keys(dict_, &number_of_keys) != 0) {
      throw std::runtime_error("Failed to get number of keys");
    }
    std::vector<std::string> keys;
    int iterator = -1;
    while (true) {
      if (dict_keys_iterator_next(dict_, &iterator) != 0) {
        throw std::runtime_error("Failed to update iterator");
      }
      if (iterator < 0) break;

      int len = 0;
      if (dict_get_key_len_by_id(dict_, iterator, &len) != 0) {
        throw std::runtime_error("Failed to get key length");
      }

      std::string key(len+1, '\0');
      if (dict_get_key_name_by_id(dict_, iterator, key.data(), len) != 0) {
        throw std::runtime_error("Failed to get key name");
      }
      keys.emplace_back(std::move(key));
    }

    return keys;
  }

  bool key_is_valid(const std::string& key) const {
    int index = key_to_index(key);
    int is_valid;
    if (dict_key_is_valid(dict_, index, &is_valid) != 0) {
      throw std::runtime_error("Failed to check if the key is valid");
    }
    return is_valid != 0;
  }

  bool has(const std::string& key) const {
    int index = key_to_index(key);
    int result;
    if (dict_has(dict_, index, &result) != 0) {
      throw std::runtime_error("Failed to check has");
    }
    return result != 0;
  }

  int rank(const std::string& key) const {
    int index = key_to_index(key);
    int r;
    if (dict_get_rank(dict_, index, &r) != 0) {
      throw std::runtime_error("Failed to get rank");
    }
    return r;
  }

  int size(const std::string& key) const {
    int index = key_to_index(key);
    int sz;
    if (dict_get_size(dict_, index, &sz) != 0) {
      throw std::runtime_error("Failed to get size");
    }
    return sz;
  }

  int key_to_index(const std::string& key) const {
    int idx;
    if (dict_key_to_index(dict_, key.c_str(), &idx) != 0) {
      throw std::runtime_error("Failed to convert key to index");
    }
    return idx;
  }

  std::string index_to_key(int index) const {
    int len = 0;
    if (dict_get_key_len_by_id(dict_, index, &len) != 0) {
      throw std::runtime_error("Failed to get key length");
    }
    std::string key(len+1, '\0');
    if (dict_get_key_name_by_id(dict_, index, key.data(), len) != 0) {
      throw std::runtime_error("Failed to get key name");
    }
    return key;
  }

  int declared_type(const std::string& key) const {
    int index = key_to_index(key);
    int type;
    if (dict_get_native_type(dict_, index, &type) != 0) {
      throw std::runtime_error("Failed to get declared type");
    }
    return type;
  }

  void set_missing(const std::string& key) {
    int index = key_to_index(key);
    if (dict_set_missing(dict_, index) != 0) {
      throw std::runtime_error("Failed to set missing");
    }
  }

  void reset() {
    if (dict_reset(dict_) != 0) {
      throw std::runtime_error("Failed to reset dictionary");
    }
  }

  void set_string(const std::string& key, const std::string& value) {
    int index = key_to_index(key);
    if (dict_set_string(dict_, index, value.c_str()) != 0) {
      throw std::runtime_error("Failed to set string");
    }
  }

  std::string get_string(const std::string& key) const {
    int index = key_to_index(key);
    int len = 0;
    if (dict_get_string_length(dict_, index, &len) != 0) {
      throw std::runtime_error("Failed to get string length");
    }
    std::string result(len+1, '\0');
    if (dict_get_string(dict_, index, &result[0], len) != 0) {
      throw std::runtime_error("Failed to get string");
    }
    return result;
  }

  void set_string_array(const std::string& key, const std::vector<std::string>& values) {
    int index = key_to_index(key);
    std::vector<const char*> raw;
    raw.reserve(values.size());
    for (const auto& v : values) raw.push_back(v.c_str());
    if (dict_set_string_array_copy(dict_, index, raw.data(), static_cast<int>(raw.size())) != 0) {
      throw std::runtime_error("Failed to set string array");
    }
  }

  std::vector<std::string> get_string_array(const std::string& key) const {
    int index = key_to_index(key);
    int sz = 0;
    if (dict_get_size(dict_, index, &sz) != 0) {
      throw std::runtime_error("Failed to get string array size");
    }
    std::vector<std::string> values;
    for (int i = 0; i < sz; ++i) {
      int len = 0;
      if (dict_get_string_length_by_id(dict_, index, i, &len) != 0) {
        throw std::runtime_error("Failed to get string length by id");
      }
      std::string tmp(len+1, '\0');
      if (dict_get_string_by_id(dict_, index, i, &tmp[0], len) != 0) {
        throw std::runtime_error("Failed to get string by id");
      }
      values.emplace_back(std::move(tmp));
    }
    return values;
  }

  void set_int64(const std::string& key, std::int64_t value) {
    int index = key_to_index(key);
    if (dict_set_int64(dict_, index, value) != 0) {
      throw std::runtime_error("Failed to set int64");
    }
  }

  std::int64_t get_int64(const std::string& key) const {
    int index = key_to_index(key);
    std::int64_t val;
    if (dict_get_int64(dict_, index, &val) != 0) {
      throw std::runtime_error("Failed to get int64");
    }
    return val;
  }

  void set_real64(const std::string& key, double value) {
    int index = key_to_index(key);
    if (dict_set_real64(dict_, index, value) != 0) {
      throw std::runtime_error("Failed to set real64");
    }
  }

  double get_real64(const std::string& key) const {
    int index = key_to_index(key);
    double val;
    if (dict_get_real64(dict_, index, &val) != 0) {
      throw std::runtime_error("Failed to get real64");
    }
    return val;
  }

  void set_int64_array(const std::string& key, const std::vector<std::int64_t>& values) {
    int index = key_to_index(key);
    if (dict_set_int64_array_copy(dict_, index, values.data(), static_cast<int>(values.size())) != 0) {
      throw std::runtime_error("Failed to set int64 array");
    }
  }

  std::vector<std::int64_t> get_int64_array(const std::string& key) const {
    int index = key_to_index(key);
    int sz = 0;
    if (dict_get_size(dict_, index, &sz) != 0) {
      throw std::runtime_error("Failed to get int64 array size");
    }
    std::vector<std::int64_t> values(sz);
    if (dict_get_int64_array(dict_, index, values.data()) != 0) {
      throw std::runtime_error("Failed to get int64 array");
    }
    return values;
  }

  void set_real64_array(const std::string& key, const std::vector<double>& values) {
    int index = key_to_index(key);
    if (dict_set_real64_array_copy(dict_, index, values.data(), static_cast<int>(values.size())) != 0) {
      throw std::runtime_error("Failed to set real64 array");
    }
  }

  std::vector<double> get_real64_array(const std::string& key) const {
    int index = key_to_index(key);
    int sz = 0;
    if (dict_get_size(dict_, index, &sz) != 0) {
      throw std::runtime_error("Failed to get real64 array size");
    }
    std::vector<double> values(sz);
    if (dict_get_real64_array(dict_, index, values.data()) != 0) {
      throw std::runtime_error("Failed to get real64 array");
    }
    return values;
  }




  // Iterator class for iterating over all keys
  class Iterator {
  public:

    Iterator(Dictionary& dict, int iterator)
      : dict_(dict), iterator_(iterator) {}

    std::string operator*() const {
      int len = 0;
      if ( dict_get_key_len_by_id(dict_.dict_, iterator_, &len) != 0) {
        throw std::runtime_error("Failed to get key length");
      }
      std::string key(len, '\0');
      if ( dict_get_key_name_by_id(dict_.dict_, iterator_, key.data(), len) != 0) {
        throw std::runtime_error("Failed to get key name");
      }
      return key;
    }

    Iterator& operator++() {
      if ( dict_keys_iterator_next(dict_.dict_, &iterator_) != 0) {
        throw std::runtime_error("Failed to update iterator");
      }
      return *this;
    }

    bool operator!=(const Iterator& other) const {
      return iterator_ != other.iterator_;
    }

  private:

    Dictionary& dict_;
    int iterator_;

  };

  // Const Iterator class for iterating over all keys
  class ConstIterator {
  public:
    ConstIterator(const Dictionary& dict, int iterator)
      : dict_(dict), iterator_(iterator) {}

    std::string operator*() const {
      int len = 0;
      if (dict_get_key_len_by_id(dict_.dict_, iterator_, &len) != 0) {
        throw std::runtime_error("Failed to get key length");
      }
      std::string key(len, '\0');
      if (dict_get_key_name_by_id(dict_.dict_, iterator_, key.data(), len) != 0) {
        throw std::runtime_error("Failed to get key name");
      }
      return key;
    }

    ConstIterator& operator++() {
      if (dict_keys_iterator_next(dict_.dict_, &iterator_) != 0) {
        throw std::runtime_error("Failed to update iterator");
      }
      return *this;
    }

    bool operator!=(const ConstIterator& other) const {
      return iterator_ != other.iterator_;
    }

  private:
    const Dictionary& dict_;
    int iterator_;
  };



  // Begin and end methods for key iteration
  Iterator begin() {
    int iterator = -1;
    if (dict_get_number_of_keys(dict_, &number_of_keys_) != 0) {
      throw std::runtime_error("Failed to get number of keys");
    }
    return Iterator(*this, iterator);
  }

  Iterator end() {
    return Iterator(*this, -1);
  }

  ConstIterator begin() const {
    int iterator = -1;
    if (dict_get_number_of_keys(dict_, &number_of_keys_) != 0) {
      throw std::runtime_error("Failed to get number of keys");
    }
    return ConstIterator(*this, iterator);
  }

  ConstIterator end() const {
    return ConstIterator(*this, -1);
  }


private:

  void* dict_ = nullptr;
  mutable int number_of_keys_ = 0;

};

