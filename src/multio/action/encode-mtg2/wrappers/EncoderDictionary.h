#pragma once

#include <string>
#include <vector>
#include <stdexcept>
#include <cstdint>
#include <cstring>
#include <utility>
#include <type_traits>

extern "C" {
    int dict_init(void** dict, const char* type, int len);
    int dict_free(void** dict);

    int dict_set_string(const char*, int, const char*, int);
    int dict_set_long(const char*, int, int64_t);
    int dict_set_double(const char*, int, double);
    int dict_set_string_array(const char*, int, const char**, int*, int);
    int dict_set_long_array(const char*, int, int64_t*, int);
    int dict_set_double_array(const char*, int, double*, int);
    int dict_has(const char*, int);

    int dict_get_string(const char*, int, char**, int*);
    int dict_get_long(const char*, int, int64_t*);
    int dict_get_double(const char*, int, double*);
    int dict_get_string_array(const char*, int, char***, int**, int*);
    int dict_get_long_array(const char*, int, int64_t**, int*);
    int dict_get_double_array(const char*, int, double**, int*);

    int dict_iterator_next(void*, void**, char**, int*, const char**, int*);
    int dict_iterator_free(void**);
}

// Traits for mapping Ids to dictionary types
template<typename Id>
struct DictTraits;

// Example Ids
template<> struct DictTraits<struct ApiOptionsId>      { static constexpr const char* name = "api-options"; };
template<> struct DictTraits<struct MarsId>            { static constexpr const char* name = "mars"; };
template<> struct DictTraits<struct ParametrizationId> { static constexpr const char* name = "parametrization"; };

// Wrapper class template
template<typename Id>
class EncoderDictionary {
public:
    EncoderDictionary(void* options = nullptr)
    {
        const char* type = DictTraits<Id>::name;
        if (dict_init(reinterpret_cast<void**>(&dict_), type, std::strlen(type))) {
            throw std::runtime_error("Failed to init dictionary");
        }
    }

    ~EncoderDictionary() {
        if (dict_) {
            dict_free(reinterpret_cast<void**>(&dict_));
        }
    }

    EncoderDictionary(const EncoderDictionary&) = delete;
    EncoderDictionary& operator=(const EncoderDictionary&) = delete;

    EncoderDictionary(EncoderDictionary&& other) noexcept : dict_(other.dict_) { other.dict_ = nullptr; }
    EncoderDictionary& operator=(EncoderDictionary&& other) noexcept {
        if (this != &other) {
            if (dict_) dict_free(reinterpret_cast<void**>(&dict_));
            dict_ = other.dict_;
            other.dict_ = nullptr;
        }
        return *this;
    }

    // Access to raw pointer if needed
    void* raw() const { return dict_; }

    // Setters
    void set(const std::string& key, const std::string& value) {
        dict_set_string(key.c_str(), key.size(), value.c_str(), value.size());
    }
    void set(const std::string& key, int64_t value) {
        dict_set_long(key.c_str(), key.size(), value);
    }
    void set(const std::string& key, double value) {
        dict_set_double(key.c_str(), key.size(), value);
    }
    void has(const std::string& key) {
        dict_has(key.c_str(), key.size());
    }

    void set(const std::string& key, const std::vector<std::string>& values) {
        std::vector<const char*> ptrs;
        std::vector<int> lens;
        for (const auto& s : values) {
            ptrs.push_back(s.c_str());
            lens.push_back(static_cast<int>(s.size()));
        }
        dict_set_string_array(key.c_str(), key.size(), ptrs.data(), lens.data(), ptrs.size());
    }
    void set(const std::string& key, const std::vector<int64_t>& values) {
        dict_set_long_array(key.c_str(), key.size(), const_cast<int64_t*>(values.data()), values.size());
    }
    void set(const std::string& key, const std::vector<double>& values) {
        dict_set_double_array(key.c_str(), key.size(), const_cast<double*>(values.data()), values.size());
    }

    // Getters
    std::string get_string(const std::string& key) const {
        char* val = nullptr;
        int len = 0;
        dict_get_string(key.c_str(), key.size(), &val, &len);
        std::string ret(val, len);
        free(val); // Assuming the C API allocates memory for this string
        if (len < 0) {
            throw std::runtime_error("Negative length returned for string");
        }
        if (ret.size() != static_cast<size_t>(len)) {
            throw std::runtime_error("Mismatch in string length returned");
        }
        return ret;
    }

    int64_t get_long(const std::string& key) const {
        int64_t val = 0;
        dict_get_long(key.c_str(), key.size(), &val);
        return val;
    }

    double get_double(const std::string& key) const {
        double val = 0;
        dict_get_double(key.c_str(), key.size(), &val);
        return val;
    }

    std::vector<std::string> get_string_array(const std::string& key) const {
        char** val = nullptr;
        int* lens = nullptr;
        int n = 0;
        dict_get_string_array(key.c_str(), key.size(), &val, &lens, &n);
        std::vector<std::string> out;
        for (int i = 0; i < n; ++i)
            out.emplace_back(val[i], lens[i]);
        // Free the arrays returned by the C API
        if (val) {
            for (int i = 0; i < n; ++i) {
                free(val[i]); // Assuming the C API allocates memory for these strings
            }
            free(val);
        }
        if (lens) {
            free(lens);
        }
        if (n > 0 && !out.empty() && out.size() != static_cast<size_t>(n)) {
            throw std::runtime_error("Mismatch in number of strings returned");
        }
        if (n == 0) {
            out.clear(); // Ensure empty vector if no strings
        }
        if (out.size() != static_cast<size_t>(n)) {
            throw std::runtime_error("Mismatch in number of strings returned");
        }
        return out;
    }

    std::vector<int64_t> get_long_array(const std::string& key) const {
        int64_t* val = nullptr;
        int n = 0;
        dict_get_long_array(key.c_str(), key.size(), &val, &n);
        if (n < 0) {
            throw std::runtime_error("Negative size returned for long array");
        }
        std::vector<int64_t> ret(val, val + n);
        free(val); // Assuming the C API allocates memory for this array
        if (ret.size() != static_cast<size_t>(n)) {
            throw std::runtime_error("Mismatch in number of longs returned");
        }
        return ret;
    }

    std::vector<double> get_double_array(const std::string& key) const {
        double* val = nullptr;
        int n = 0;
        dict_get_double_array(key.c_str(), key.size(), &val, &n);
        std::vector<double> ret(val, val + n);
        free(val); // Assuming the C API allocates memory for this array
        if (ret.size() != static_cast<size_t>(n)) {
            throw std::runtime_error("Mismatch in number of doubles returned");
        }
        return ret;
    }

    // Iterator example
    class Iterator {
    public:
        Iterator(void* dict) : dict_(dict), iter_(nullptr), done_(false) { ++(*this); }
        ~Iterator() { if (iter_) dict_iterator_free(&iter_); }

        bool done() const { return done_; }
        std::pair<std::string, std::string> operator*() const { return current_; }
        Iterator& operator++() {
            const char* val = nullptr;
            int len = 0;
            char* key = nullptr;
            int keylen = 0;
            if (dict_iterator_next(dict_, &iter_, &key, &keylen, &val, &len) || iter_ == nullptr) {
                done_ = true;
            } else {
                current_ = {std::string(key, keylen), std::string(val, len)};
            }
            return *this;
        }

    private:
        void* dict_;
        void* iter_;
        bool done_;
        std::pair<std::string, std::string> current_;
    };

    Iterator begin() const { return Iterator(dict_); }

private:
    void* dict_ = nullptr;
};
