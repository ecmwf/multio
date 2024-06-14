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
#include "eckit/utils/StringTools.h"

#include "multio/config/ComponentConfiguration.h"
#include "multio/util/IntegerSequence.h"

#include <algorithm>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>


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

namespace multio::util {

static const unsigned PROPAGATE_ERROR = 0;
static const unsigned RECOVER_ERROR = 1;

enum class OnClientError : unsigned
{
    Propagate = PROPAGATE_ERROR,
    Recover = RECOVER_ERROR,
    AbortTransport,
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
    std::optional<RetryOptions> retryOptions{};
};

inline DefaultFailureOptions parseDefaultFailureOptions(const eckit::Configuration& conf) {
    DefaultFailureOptions options{};

    if (conf.has("maxRetries")) {
        options.retryOptions = std::optional<RetryOptions>{RetryOptions{}};
        options.retryOptions->maxRetries = conf.getInt("maxRetries");
    }

    return options;
}

struct RetryState {
    int countRetries = 0;
};
struct DefaultFailureState {
    std::optional<RetryState> retryState{};
};

}  // namespace multio::util


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
struct Translator<multio::util::OnReceiveError, std::string> {
    std::string operator()(multio::util::OnReceiveError);
};

template <>
struct Translator<multio::util::OnDispatchError, std::string> {
    std::string operator()(multio::util::OnDispatchError);
};

}  // namespace eckit


namespace multio::util {

template <typename T>
std::pair<std::string, T> makeLowerCaseStringPair(T&& v) {
    return {eckit::StringTools::lower(eckit::translate<std::string>(v)), std::forward<T>(v)};
}

template <typename T, T... TS>
std::unordered_map<std::string, T> buildEnumLookUpMap_(util::integer_sequence<T, TS...>) {
    return std::unordered_map<std::string, T>{makeLowerCaseStringPair(std::forward<T>(TS))...};
}

template <typename T>
std::optional<T> parseWithEnumMap_(const std::unordered_map<std::string, T>& map, const std::string& str) {
    auto it = map.find(eckit::StringTools::lower(str));
    if (it != map.end()) {
        return std::optional<T>{it->second};
    }
    return std::optional<T>{};
}

template <typename T, typename TagSeq>
std::optional<T> parseErrorTag(const std::string& str) {
    static const std::unordered_map<std::string, T> map{buildEnumLookUpMap_(TagSeq())};
    return parseWithEnumMap_(map, str);
}


enum class FailureHandlerResponse : unsigned
{
    Rethrow = 0x0,
    Ignore = 0x1,
    Retry = 0x2,
};

class FailureAwareException : public eckit::Exception {
public:
    FailureAwareException(const std::string& what, const eckit::CodeLocation& l = eckit::CodeLocation()) :
        eckit::Exception(what, l) {}

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


template <typename ComponentFailureTraits>
class FailureAware {
private:
    using OnErrorType = typename ComponentFailureTraits::OnErrorType;
    using FailureOptions = typename ComponentFailureTraits::FailureOptions;
    using FailureState = typename ComponentFailureTraits::FailureState;

    config::LocalPeerTag peerTag_;
    OnErrorType parsedOnErrTag_{};
    FailureOptions options_;

public:
    FailureAware(const config::ComponentConfiguration& compConf) : peerTag_{compConf.multioConfig().localPeerTag()} {
        if (compConf.parsedConfig().has(ComponentFailureTraits::configKey())) {
            auto unparsedOnErrTagMaybe = ([&]() {
                try {
                    return std::optional<std::string>{
                        compConf.parsedConfig().getString(ComponentFailureTraits::configKey())};
                }
                catch (...) {
                    return std::optional<std::string>{};
                }
            })();

            auto subConfigMaybe = ([&]() {
                if (unparsedOnErrTagMaybe) {
                    return std::optional<eckit::LocalConfiguration>{};
                }
                else {
                    return std::optional<eckit::LocalConfiguration>{
                        compConf.parsedConfig().getSubConfiguration(ComponentFailureTraits::configKey())};
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
                        oss << "FailureAware configuration for component " << ComponentFailureTraits::componentName()
                            << " described by key \"" << ComponentFailureTraits::configKey()
                            << "\" is supposed to map to a string or an configuration object with key \"type\"";
                        std::throw_with_nested(FailureAwareException(oss.str(), Here()));
                    }
                }
            })();
            auto errorTagMaybe = ComponentFailureTraits::parse(unparsedOnErrTag);

            // Tag has been specified in the option but could not be parsed
            if (unparsedOnErrTagMaybe && !errorTagMaybe) {
                std::ostringstream oss;
                oss << "Unsupported value \"" << unparsedOnErrTag << "\" for key \""
                    << ComponentFailureTraits::configKey() << "\" for FailureAware configuration for component "
                    << ComponentFailureTraits::componentName();
                throw FailureAwareException(oss.str(), Here());
            }

            parsedOnErrTag_ = errorTagMaybe ? *errorTagMaybe : ComponentFailureTraits::defaultOnErrorTag();


            if (subConfigMaybe) {
                options_ = ComponentFailureTraits::parseFailureOptions(*subConfigMaybe);
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
                oss << "FailureAware<" << ComponentFailureTraits::componentName() << "> with behaviour \""
                    << eckit::translate<std::string>(parsedOnErrTag_) << "\" on "
                    << eckit::translate<std::string>(peerTag_) << " for context: [" << std::endl
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


}  // namespace multio::util
