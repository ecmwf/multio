#pragma once


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

}