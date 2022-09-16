
#include "FailureHandling.h"
#include <algorithm>
#include <string>
#include <unordered_map>
#include "eckit/utils/StringTools.h"

namespace multio {
namespace util {

std::string toString(OnClientError tag) {
    switch (tag) {
        case OnClientError::Propagate:
            return std::string("propagate");
        case OnClientError::Recover:
            return std::string("recover");
        case OnClientError::AbortAllTransports:
            return std::string("abort-all-transports");
    }
}

std::string toString(OnServerError tag) {
    switch (tag) {
        case OnServerError::Propagate:
            return std::string("propagate");
        case OnServerError::Recover:
            return std::string("recover");
        case OnServerError::AbortTransport:
            return std::string("abort-transport");
    }
}

std::string toString(OnPlanError tag) {
    switch (tag) {
        case OnPlanError::Propagate:
            return std::string("propagate");
        case OnPlanError::Recover:
            return std::string("recover");
        case OnPlanError::AbortTransport:
            return std::string("abort-transport");
    }
}

std::string toString(OnActionError tag) {
    switch (tag) {
        case OnActionError::Propagate:
            return std::string("propagate");
    }
}

std::string toString(OnTransportError tag) {
    switch (tag) {
        case OnTransportError::Propagate:
            return std::string("propagate");
        case OnTransportError::Recover:
            return std::string("recover");
    }
}

std::string toString(OnReceiveError tag) {
    switch (tag) {
        case OnReceiveError::Propagate:
            return std::string("propagate");
    }
}

std::string toString(OnDispatchError tag) {
    switch (tag) {
        case OnDispatchError::Propagate:
            return std::string("propagate");
    }
}


namespace {
template <typename T>
std::pair<std::string, T> makeLowerCaseStringPair(T&& v) {
    return {eckit::StringTools::lower(toString(v)), std::forward<T>(v)};
}

template <typename T, typename... TS>
std::unordered_map<std::string, T> buildEnumLookUpMap_(T&& v, TS&&... ts) {
    return std::unordered_map<std::string, T> {
        makeLowerCaseStringPair(std::forward<T>(v)),
            makeLowerCaseStringPair(std::forward<TS>(ts))...
    };
}

template <typename T>
eckit::Optional<T> parseWithEnumMap_(const std::unordered_map<std::string, T>& map,
                                     const std::string& str) {
    auto it = map.find(eckit::StringTools::lower(str));
    if (it != map.end()) {
        return eckit::Optional<T>{it->second};
    }
    return eckit::Optional<T>{};
}
}  // namespace

eckit::Optional<OnClientError> parseOnClientError(const std::string& str) {
    static const std::unordered_map<std::string, OnClientError> map{
        buildEnumLookUpMap_(OnClientError::Propagate, OnClientError::Recover, OnClientError::AbortAllTransports)};
    return parseWithEnumMap_(map, str);
};
eckit::Optional<OnServerError> parseOnServerError(const std::string& str) {
    static const std::unordered_map<std::string, OnServerError> map{
        buildEnumLookUpMap_(OnServerError::Propagate, OnServerError::Recover, OnServerError::AbortTransport)};
    return parseWithEnumMap_(map, str);
};
eckit::Optional<OnPlanError> parseOnPlanError(const std::string& str) {
    static const std::unordered_map<std::string, OnPlanError> map{
        buildEnumLookUpMap_(OnPlanError::Propagate, OnPlanError::Recover, OnPlanError::AbortTransport)};
    return parseWithEnumMap_(map, str);
};
eckit::Optional<OnActionError> parseOnActionError(const std::string& str) {
    static const std::unordered_map<std::string, OnActionError> map{
        buildEnumLookUpMap_(OnActionError::Propagate)};
    return parseWithEnumMap_(map, str);
};
eckit::Optional<OnTransportError> parseOnTransportError(const std::string& str) {
    static const std::unordered_map<std::string, OnTransportError> map{
        buildEnumLookUpMap_(OnTransportError::Propagate, OnTransportError::Recover)};
    return parseWithEnumMap_(map, str);
};
eckit::Optional<OnReceiveError> parseOnReceiveError(const std::string& str) {
    static const std::unordered_map<std::string, OnReceiveError> map{
        buildEnumLookUpMap_(OnReceiveError::Propagate)};
    return parseWithEnumMap_(map, str);
};
eckit::Optional<OnDispatchError> parseOnDispatchError(const std::string& str) {
    static const std::unordered_map<std::string, OnDispatchError> map{
        buildEnumLookUpMap_(OnDispatchError::Propagate)};
    return parseWithEnumMap_(map, str);
};


}  // namespace util
}  // namespace multio
