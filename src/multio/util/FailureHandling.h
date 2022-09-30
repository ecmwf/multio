/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date Sep 2022

#ifndef multio_util_FailureHandling_H
#define multio_util_FailureHandling_H

#include <algorithm>
#include <string>
#include <unordered_map>
#include "ConfigurationContext.h"
#include "IntegerSequence.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/utils/Optional.h"
#include "eckit/utils/StringTools.h"


/** Experimental - we don't know how to best deal with errors and how we can define descriptive and generic handlers.
 *This may be a suggestion or reworked completely.
 *
 * The idea is to have a reserved keyword "on-error" in YAML configuration of different components. This key might be
 *simply one tag represented as enum, or a whole list of tags that can be applied orthogonal to each other. Each
 *component should be able to generate a handler function from this tag. Then functionality is wrapped in a try-catch
 *everything, reacting on the error and probably throw again/propagate.
 *
 * Similary behaviour between components (i.e. error propagation) should be handled on a partially generic way.
 **/

namespace multio {
namespace util {

static const unsigned PROPAGATE_ERROR = 0;
static const unsigned RECOVER_ERROR = 1;

enum class OnClientError : unsigned
{
    Propagate = PROPAGATE_ERROR,
    Recover = RECOVER_ERROR,
    AbortAllTransports,
};

enum class OnServerError : unsigned
{
    Propagate = PROPAGATE_ERROR,
    Recover = RECOVER_ERROR,
    AbortTransport,
};

enum class OnPlanError : unsigned
{
    Propagate = PROPAGATE_ERROR,
    Recover = RECOVER_ERROR,
};

enum class OnActionError : unsigned
{
    Propagate = PROPAGATE_ERROR,
    Recover = RECOVER_ERROR,
};

enum class OnTransportError : unsigned
{
    Propagate = PROPAGATE_ERROR,
    Recover = RECOVER_ERROR,  // Some transport might be able to reconnect or request information?
};

enum class OnReceiveError : unsigned
{
    Propagate = PROPAGATE_ERROR
};
enum class OnDispatchError : unsigned
{
    Propagate = PROPAGATE_ERROR
};


struct BaseErrorOptions {};

struct RetryOptions {
    int maxRetry = 0;
    int countRetries = 0;
};


std::string toString(OnClientError tag);
std::string toString(OnServerError tag);
std::string toString(OnPlanError tag);
std::string toString(OnActionError tag);
std::string toString(OnTransportError tag);
std::string toString(OnReceiveError tag);
std::string toString(OnDispatchError tag);

namespace {
template <typename T>
std::pair<std::string, T> makeLowerCaseStringPair(T&& v) {
    return {eckit::StringTools::lower(toString(v)), std::forward<T>(v)};
}

template <typename T, T... TS>
std::unordered_map<std::string, T> buildEnumLookUpMap_(integer_sequence<T, TS...>) {
    return std::unordered_map<std::string, T>{makeLowerCaseStringPair(std::forward<T>(TS))...};
}

template <typename T>
eckit::Optional<T> parseWithEnumMap_(const std::unordered_map<std::string, T>& map, const std::string& str) {
    auto it = map.find(eckit::StringTools::lower(str));
    if (it != map.end()) {
        return eckit::Optional<T>{it->second};
    }
    return eckit::Optional<T>{};
}

template <typename T, typename TagSeq>
eckit::Optional<T> parseErrorTag_(const std::string& str) {
    static const std::unordered_map<std::string, T> map{buildEnumLookUpMap_(TagSeq())};
    return parseWithEnumMap_(map, str);
}

}  // namespace


template <ComponentTag tag>
struct ComponentFailureTraits {};

template <>
struct ComponentFailureTraits<ComponentTag::Client> {
    using onErrorType = OnClientError;
    using tag_sequence = integer_sequence<onErrorType, OnClientError::Propagate, OnClientError::Recover,
                                          OnClientError::AbortAllTransports>;
    static inline eckit::Optional<onErrorType> parse(const std::string& str) {
        return parseErrorTag_<onErrorType, tag_sequence>(str);
    }
    static inline onErrorType defaultOnErrorTag() { return OnClientError::Propagate; };
    static inline std::string configKey() { return std::string("on-error"); };
    static inline std::string configOptionsKey() { return std::string("on-error-options"); };
};

template <>
struct ComponentFailureTraits<ComponentTag::Server> {
    using onErrorType = OnServerError;
    using tag_sequence =
        integer_sequence<onErrorType, OnServerError::Propagate, OnServerError::Recover, OnServerError::AbortTransport>;
    static inline eckit::Optional<onErrorType> parse(const std::string& str) {
        return parseErrorTag_<onErrorType, tag_sequence>(str);
    }
    static inline onErrorType defaultOnErrorTag() { return OnServerError::Propagate; };
    static inline std::string configKey() { return std::string("on-error"); };
    static inline std::string configOptionsKey() { return std::string("on-error-options"); };
};

template <>
struct ComponentFailureTraits<ComponentTag::Plan> {
    using onErrorType = OnPlanError;
    using tag_sequence = integer_sequence<onErrorType, OnPlanError::Propagate, OnPlanError::Recover>;
    static inline eckit::Optional<onErrorType> parse(const std::string& str) {
        return parseErrorTag_<onErrorType, tag_sequence>(str);
    }
    static inline onErrorType defaultOnErrorTag() { return OnPlanError::Propagate; };
    static inline std::string configKey() { return std::string("on-error"); };
    static inline std::string configOptionsKey() { return std::string("on-error-options"); };
};

template <>
struct ComponentFailureTraits<ComponentTag::Action> {
    using onErrorType = OnActionError;
    using tag_sequence = integer_sequence<onErrorType, OnActionError::Propagate, OnActionError::Recover>;
    static inline eckit::Optional<onErrorType> parse(const std::string& str) {
        return parseErrorTag_<onErrorType, tag_sequence>(str);
    }
    static inline onErrorType defaultOnErrorTag() { return OnActionError::Propagate; };
    static inline std::string configKey() { return std::string("on-error"); };
    static inline std::string configOptionsKey() { return std::string("on-error-options"); };
};

template <>
struct ComponentFailureTraits<ComponentTag::Transport> {
    using onErrorType = OnTransportError;
    using tag_sequence = integer_sequence<onErrorType, OnTransportError::Propagate, OnTransportError::Recover>;
    static inline eckit::Optional<onErrorType> parse(const std::string& str) {
        return parseErrorTag_<onErrorType, tag_sequence>(str);
    }
    static inline onErrorType defaultOnErrorTag() { return OnTransportError::Propagate; };
    static inline std::string configKey() { return std::string("on-error"); };
    static inline std::string configOptionsKey() { return std::string("on-error-options"); };
};

template <>
struct ComponentFailureTraits<ComponentTag::Dispatcher> {
    using onErrorType = OnDispatchError;
    using tag_sequence = integer_sequence<onErrorType, OnDispatchError::Propagate>;
    static inline eckit::Optional<onErrorType> parse(const std::string& str) {
        return parseErrorTag_<onErrorType, tag_sequence>(str);
    }
    static inline onErrorType defaultOnErrorTag() { return OnDispatchError::Propagate; };
    static inline std::string configKey() { return std::string("on-dispatch-error"); };
    static inline std::string configOptionsKey() { return std::string("on-dispatch-error-options"); };
};

template <>
struct ComponentFailureTraits<ComponentTag::Receiver> {
    using onErrorType = OnReceiveError;
    using tag_sequence = integer_sequence<onErrorType, OnReceiveError::Propagate>;
    static inline eckit::Optional<onErrorType> parse(const std::string& str) {
        return parseErrorTag_<onErrorType, tag_sequence>(str);
    }
    static inline onErrorType defaultOnErrorTag() { return OnReceiveError::Propagate; };
    static inline std::string configKey() { return std::string("on-receive-error"); };
    static inline std::string configOptionsKey() { return std::string("on-receive-error-options"); };
};


enum class FailureHandlerResponse : unsigned
{
    Rethrow = 0,
    Ignore = 1,
    Retry = 2,
};

template <ComponentTag tag, typename OnErrorType = typename ComponentFailureTraits<tag>::onErrorType>
class FailureAware {
private:
    util::LocalPeerTag peerTag_;
    eckit::Optional<OnErrorType> parsedOnErrTag_{};

public:
    FailureAware(const ConfigurationContext& confCtx) : peerTag_{confCtx.localPeerTag()} {
        ASSERT(confCtx.componentTag() == tag);
        if (confCtx.config().has(ComponentFailureTraits<tag>::configKey())) {
            parsedOnErrTag_ = ComponentFailureTraits<tag>::parse(
                confCtx.config().getString(ComponentFailureTraits<tag>::configKey()));
        };
    };

    // TODO in the future we may pass further Options and additional handling context
    virtual FailureHandlerResponse handleFailure(OnErrorType) const = 0;
    // May be overwritten or not
    virtual FailureHandlerResponse handleFailure(OnErrorType t, const std::exception& e) const {
        return handleFailure(t);
    };
    // May be overwritten or not
    virtual FailureHandlerResponse handleFailure(OnErrorType t, const eckit::Exception& e) const {
        return handleFailure(t);
    };

    FailureHandlerResponse handleFailure() const {
        return handleFailure(parsedOnErrTag_ ? *parsedOnErrTag_ : ComponentFailureTraits<tag>::defaultOnErrorTag());
    };
    FailureHandlerResponse handleFailure(const std::exception& e) const {
        return handleFailure(parsedOnErrTag_ ? *parsedOnErrTag_ : ComponentFailureTraits<tag>::defaultOnErrorTag(), e);
    };
    FailureHandlerResponse handleFailure(const eckit::Exception& e) const {
        return handleFailure(parsedOnErrTag_ ? *parsedOnErrTag_ : ComponentFailureTraits<tag>::defaultOnErrorTag(), e);
    };

    virtual ~FailureAware() = default;

protected:
    template <typename T, typename P>
    void withFailureHandling(T&& callable, P&& contextString) const {
        bool doRetry;
        do {
            doRetry = false;
            try {
                callable();
            }
            catch (...) {
                eckit::Log::error() << "[FailureAware<" << util::toString(tag) << "> "
                                    << (parsedOnErrTag_ ? util::toString(*parsedOnErrTag_)
                                                        : std::string("default handling"))
                                    << " on " << util::toString(peerTag_) << " for context: [" << std::endl
                                    << contextString() << std::endl
                                    << "]] "
                                    << " - Handling ";
                auto withHandleFailureResult = [&](FailureHandlerResponse r) {
                    switch (r) {
                        case FailureHandlerResponse::Retry: {
                            doRetry = true;
                            break;
                        }
                        case FailureHandlerResponse::Rethrow: {
                            throw;
                        }
                        default:
                            break;
                    }
                };

                try {
                    throw;
                }
                catch (const eckit::Exception& e) {
                    eckit::Log::error() << "eckit::Exception: " << e.what() << std::endl;
                    e.exceptionStack(eckit::Log::error(), true);
                    withHandleFailureResult(this->handleFailure(e));
                }
                catch (const std::exception& e) {
                    eckit::Log::error() << "std::exception: " << e.what() << std::endl;
                    withHandleFailureResult(this->handleFailure(e));
                }
                catch (...) {
                    eckit::Log::error() << "general exception" << std::endl;
                    withHandleFailureResult(this->handleFailure());
                }
            }
        } while (doRetry);
    }

    template <typename T>
    void withFailureHandling(T&& callable) const {
        withFailureHandling(std::forward<T>(callable), []() { return std::string("unknown"); });
    }
};


}  // namespace util
}  // namespace multio

#endif
