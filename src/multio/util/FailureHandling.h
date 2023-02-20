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

#pragma once


#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/utils/Optional.h"
#include "eckit/utils/StringTools.h"

#include "multio/util/ConfigurationContext.h"
#include "multio/util/IntegerSequence.h"
#include "multio/util/Translate.h"

#include <algorithm>
#include <string>
#include <unordered_map>


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


// To be extended
struct RetryOptions {
    int maxRetries = 0;
};
struct DefaultFailureOptions {
    eckit::Optional<RetryOptions> retryOptions{};
};

inline DefaultFailureOptions parseDefaultFailureOptions(const eckit::Configuration& conf) {
    DefaultFailureOptions options{};

    if (conf.has("maxRetries")) {
        options.retryOptions = eckit::Optional<RetryOptions>{RetryOptions{}};
        options.retryOptions->maxRetries = conf.getInt("maxRetries");
    }

    return options;
}

struct RetryState {
    int countRetries = 0;
};
struct DefaultFailureState {
    eckit::Optional<RetryState> retryState{};
};

}  // namespace util
}  // namespace multio


namespace eckit {

template <>
struct Translator<multio::util::OnClientError, std::string> {
    std::string operator()(multio::util::OnClientError);
};

template <>
struct Translator<multio::util::OnServerError, std::string> {
    std::string operator()(multio::util::OnServerError);
};

template <>
struct Translator<multio::util::OnPlanError, std::string> {
    std::string operator()(multio::util::OnPlanError);
};

template <>
struct Translator<multio::util::OnActionError, std::string> {
    std::string operator()(multio::util::OnActionError);
};

template <>
struct Translator<multio::util::OnTransportError, std::string> {
    std::string operator()(multio::util::OnTransportError);
};

template <>
struct Translator<multio::util::OnReceiveError, std::string> {
    std::string operator()(multio::util::OnReceiveError);
};

template <>
struct Translator<multio::util::OnDispatchError, std::string> {
    std::string operator()(multio::util::OnDispatchError);
};

}  // namespace eckit


namespace multio {
namespace util {

template <typename T>
std::pair<std::string, T> makeLowerCaseStringPair(T&& v) {
    return {eckit::StringTools::lower(translate<std::string>(v)), std::forward<T>(v)};
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


template <ComponentTag tag>
struct ComponentFailureTraits {};

template <>
struct ComponentFailureTraits<ComponentTag::Client> {
    using OnErrorType = OnClientError;
    using FailureOptions = DefaultFailureOptions;
    using FailureState = DefaultFailureState;
    using TagSequence = integer_sequence<OnErrorType, OnClientError::Propagate, OnClientError::Recover,
                                         OnClientError::AbortAllTransports>;
    static inline eckit::Optional<OnErrorType> parse(const std::string& str) {
        return parseErrorTag_<OnErrorType, TagSequence>(str);
    }
    static inline OnErrorType defaultOnErrorTag() { return OnClientError::Propagate; };
    static inline FailureOptions parseFailureOptions(const eckit::Configuration& conf) {
        return parseDefaultFailureOptions(conf);
    };
    static inline std::string configKey() { return std::string("on-error"); };
};

template <>
struct ComponentFailureTraits<ComponentTag::Server> {
    using OnErrorType = OnServerError;
    using FailureOptions = DefaultFailureOptions;
    using FailureState = DefaultFailureState;
    using TagSequence = integer_sequence<OnErrorType, OnServerError::Propagate, OnServerError::Recover,
                                         OnServerError::AbortTransport>;
    static inline eckit::Optional<OnErrorType> parse(const std::string& str) {
        return parseErrorTag_<OnErrorType, TagSequence>(str);
    }
    static inline OnErrorType defaultOnErrorTag() { return OnServerError::Propagate; };
    static inline FailureOptions parseFailureOptions(const eckit::Configuration& conf) {
        return parseDefaultFailureOptions(conf);
    };
    static inline std::string configKey() { return std::string("on-error"); };
};

template <>
struct ComponentFailureTraits<ComponentTag::Plan> {
    using OnErrorType = OnPlanError;
    using FailureOptions = DefaultFailureOptions;
    using FailureState = DefaultFailureState;
    using TagSequence = integer_sequence<OnErrorType, OnPlanError::Propagate, OnPlanError::Recover>;
    static inline eckit::Optional<OnErrorType> parse(const std::string& str) {
        return parseErrorTag_<OnErrorType, TagSequence>(str);
    }
    static inline OnErrorType defaultOnErrorTag() { return OnPlanError::Propagate; };
    static inline std::string configKey() { return std::string("on-error"); };
    static inline FailureOptions parseFailureOptions(const eckit::Configuration& conf) {
        return parseDefaultFailureOptions(conf);
    };
};

template <>
struct ComponentFailureTraits<ComponentTag::Action> {
    using OnErrorType = OnActionError;
    using FailureOptions = DefaultFailureOptions;
    using FailureState = DefaultFailureState;
    using TagSequence = integer_sequence<OnErrorType, OnActionError::Propagate, OnActionError::Recover>;
    static inline eckit::Optional<OnErrorType> parse(const std::string& str) {
        return parseErrorTag_<OnErrorType, TagSequence>(str);
    }
    static inline OnErrorType defaultOnErrorTag() { return OnActionError::Propagate; };
    static inline std::string configKey() { return std::string("on-error"); };
    static inline FailureOptions parseFailureOptions(const eckit::Configuration& conf) {
        return parseDefaultFailureOptions(conf);
    };
};

template <>
struct ComponentFailureTraits<ComponentTag::Transport> {
    using OnErrorType = OnTransportError;
    using FailureOptions = DefaultFailureOptions;
    using FailureState = DefaultFailureState;
    using TagSequence = integer_sequence<OnErrorType, OnTransportError::Propagate, OnTransportError::Recover>;
    static inline eckit::Optional<OnErrorType> parse(const std::string& str) {
        return parseErrorTag_<OnErrorType, TagSequence>(str);
    }
    static inline OnErrorType defaultOnErrorTag() { return OnTransportError::Propagate; };
    static inline std::string configKey() { return std::string("on-error"); };
    static inline FailureOptions parseFailureOptions(const eckit::Configuration& conf) {
        return parseDefaultFailureOptions(conf);
    };
};

template <>
struct ComponentFailureTraits<ComponentTag::Dispatcher> {
    using OnErrorType = OnDispatchError;
    using FailureOptions = DefaultFailureOptions;
    using FailureState = DefaultFailureState;
    using TagSequence = integer_sequence<OnErrorType, OnDispatchError::Propagate>;
    static inline eckit::Optional<OnErrorType> parse(const std::string& str) {
        return parseErrorTag_<OnErrorType, TagSequence>(str);
    }
    static inline OnErrorType defaultOnErrorTag() { return OnDispatchError::Propagate; };
    static inline std::string configKey() { return std::string("on-dispatch-error"); };
    static inline FailureOptions parseFailureOptions(const eckit::Configuration& conf) {
        return parseDefaultFailureOptions(conf);
    };
};

template <>
struct ComponentFailureTraits<ComponentTag::Receiver> {
    using OnErrorType = OnReceiveError;
    using FailureOptions = DefaultFailureOptions;
    using FailureState = DefaultFailureState;
    using TagSequence = integer_sequence<OnErrorType, OnReceiveError::Propagate>;
    static inline eckit::Optional<OnErrorType> parse(const std::string& str) {
        return parseErrorTag_<OnErrorType, TagSequence>(str);
    }
    static inline OnErrorType defaultOnErrorTag() { return OnReceiveError::Propagate; };
    static inline std::string configKey() { return std::string("on-receive-error"); };
    static inline FailureOptions parseFailureOptions(const eckit::Configuration& conf) {
        return parseDefaultFailureOptions(conf);
    };
};


enum class FailureHandlerResponse : unsigned
{
    Rethrow = 0x0,
    Ignore = 0x1,
    Retry = 0x2,
};

class FailureAwareException : public eckit::Exception {
public:
    FailureAwareException(const std::string& what, const eckit::CodeLocation& l = eckit::CodeLocation()) : eckit::Exception(what, l) {}
    
    friend std::ostream& operator<<(std::ostream& os, const FailureAwareException& dt);
};

std::ostream& operator<<(std::ostream& os, const FailureAwareException& dt);

struct FailureContext {
    std::exception_ptr eptr;
    std::string context;
    
    friend std::ostream& operator<<(std::ostream& os, const FailureContext& dt);
};

std::ostream& operator<<(std::ostream& os, const FailureContext& dt);

void printException(std::ostream& out, const std::exception& e);
void printException(std::ostream& out, const eckit::Exception& e);
void printException(std::ostream& out, const FailureAwareException& e);

void printFailureContext(std::ostream& out, const FailureContext& c);


template <ComponentTag tag>
class FailureAware {
private:
    using OnErrorType = typename ComponentFailureTraits<tag>::OnErrorType;
    using FailureOptions = typename ComponentFailureTraits<tag>::FailureOptions;
    using FailureState = typename ComponentFailureTraits<tag>::FailureState;

    util::LocalPeerTag peerTag_;
    OnErrorType parsedOnErrTag_{};
    FailureOptions options_;

public:
    FailureAware(const ConfigurationContext& confCtx) : peerTag_{confCtx.localPeerTag()} {
        ASSERT(confCtx.componentTag() == tag);
        if (confCtx.config().has(ComponentFailureTraits<tag>::configKey())) {
            auto unparsedOnErrTagMaybe = ([&]() {
                try {
                    return eckit::Optional<std::string>{
                        confCtx.config().getString(ComponentFailureTraits<tag>::configKey())};
                }
                catch (...) {
                    return eckit::Optional<std::string>{};
                }
            })();

            auto subConfigMaybe = ([&]() {
                if (unparsedOnErrTagMaybe) {
                    return eckit::Optional<eckit::LocalConfiguration>{};
                }
                else {
                    return eckit::Optional<eckit::LocalConfiguration>{
                        confCtx.config().getSubConfiguration(ComponentFailureTraits<tag>::configKey())};
                }
            }());

            std::string unparsedOnErrTag = ([&]() {
                if (unparsedOnErrTagMaybe) {
                    return *unparsedOnErrTagMaybe;
                }
                else {
                    try {
                        return subConfigMaybe->getString("type");
                    }
                    catch (...) {
                        std::ostringstream oss;
                        oss << "FailureAware configuration for component " << translate<std::string>(tag)
                            << " described by key \"" << ComponentFailureTraits<tag>::configKey()
                            << "\" is supposed to map to a string or an configuration object with key \"type\"";
                        std::throw_with_nested(FailureAwareException(oss.str(), Here()));
                    }
                }
            })();
            auto errorTagMaybe = ComponentFailureTraits<tag>::parse(unparsedOnErrTag);
            parsedOnErrTag_ = errorTagMaybe ? *errorTagMaybe : ComponentFailureTraits<tag>::defaultOnErrorTag();

            if (subConfigMaybe) {
                options_ = ComponentFailureTraits<tag>::parseFailureOptions(*subConfigMaybe);
            }
            else {
                options_ = FailureOptions{};
            }
        };
    };

    virtual FailureHandlerResponse handleFailure(OnErrorType t, const FailureContext&, FailureState&) const = 0;

    FailureHandlerResponse handleFailure(const FailureContext& e, FailureState& s) const {
        return handleFailure(parsedOnErrTag_, e, s);
    };

    virtual ~FailureAware() = default;

protected:
    template <typename T, typename P>
    void withFailureHandling(T&& callable, P&& contextString) const {
        bool doRetry;
        FailureState state;
        do {
            doRetry = false;
            try {
                callable();
            }
            catch (...) {
                std::ostringstream oss;
                oss << "FailureAware<" << translate<std::string>(tag) << "> with behaviour \""
                    << translate<std::string>(parsedOnErrTag_) << "\" on " << translate<std::string>(peerTag_)
                    << " for context: [" << std::endl
                    << contextString() << std::endl
                    << "]";

                FailureContext fctx{std::current_exception(), oss.str()};

                switch (handleFailure(fctx, state)) {
                    case FailureHandlerResponse::Retry: {
                        try {
                            std::ostringstream oss2;
                            oss2 << "Retrying after catching nested exceptions: " << std::endl
                                 << oss.str() << std::endl;
                            std::throw_with_nested(FailureAwareException(oss2.str(), Here()));
                        }
                        catch (const FailureAwareException& e) {
                            // This is supposed to be called
                            printException(eckit::Log::error(), e);
                            doRetry = true;
                        }
                        break;
                    }
                    case FailureHandlerResponse::Rethrow: {
                        std::throw_with_nested(FailureAwareException(oss.str(), Here()));
                    }
                    default:
                        try {
                            std::ostringstream oss2;
                            oss2 << "Ignoring nested exceptions: " << std::endl << oss.str() << std::endl;
                            std::throw_with_nested(FailureAwareException(oss2.str(), Here()));
                        }
                        catch (const FailureAwareException& e) {
                            // This is supposed to be called
                            printException(eckit::Log::error(), e);
                        }
                        break;
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
