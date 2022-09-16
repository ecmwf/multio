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

#include "eckit/utils/Optional.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "ConfigurationContext.h"

/** Experimental - we don't know how to best deal with errors and how we can define descriptive and generic handlers. This may be a suggestion or reworked completely.
 *
 * The idea is to have a reserved keyword "on-error" in YAML configuration of different components. This key might be simply one tag represented as enum, or a whole list of tags that can be applied orthogonal to each other.
 * Each component should be able to generate a handler function from this tag. 
 * Then functionality is wrapped in a try-catch everything, reacting on the error and probably throw again/propagate.
 *
 * Similary behaviour between components (i.e. error propagation) should be handled on a partially generic way.
 **/

namespace multio {
namespace util {


enum class OnClientError : unsigned
{
    Propagate = 0,
    Recover,
    AbortAllTransports,
};

enum class OnServerError : unsigned
{
    Propagate = 0,
    Recover,
    AbortTransport,
};

enum class OnPlanError : unsigned
{
    Propagate = 0,
    Recover,
    AbortTransport,
};

enum class OnActionError : unsigned
{
    Propagate = 0
};

enum class OnTransportError : unsigned
{
    Propagate = 0,
    Recover, // Some transport might be able to reconnect or request information?
};

enum class OnReceiveError : unsigned
{
    Propagate = 0
};
enum class OnDispatchError : unsigned
{
    Propagate = 0
};



std::string toString(OnClientError tag);
std::string toString(OnServerError tag);
std::string toString(OnPlanError tag);
std::string toString(OnActionError tag);
std::string toString(OnTransportError tag);
std::string toString(OnReceiveError tag);
std::string toString(OnDispatchError tag);

eckit::Optional<OnClientError> parseOnClientError(const std::string& str);
eckit::Optional<OnServerError> parseOnServerError(const std::string& str);
eckit::Optional<OnPlanError> parseOnPlanError(const std::string& str);
eckit::Optional<OnActionError> parseOnActionError(const std::string& str);
eckit::Optional<OnTransportError> parseOnTransportError(const std::string& str);
eckit::Optional<OnReceiveError> parseOnReceiveError(const std::string& str);
eckit::Optional<OnDispatchError> parseOnDispatchError(const std::string& str);

template<ComponentTag tag>
struct ComponentFailureTraits {};

template<>
struct ComponentFailureTraits<ComponentTag::Client> { 
    using onErrorType = OnClientError; 
    static eckit::Optional<onErrorType> parse(const std::string& str) { return parseOnClientError(str); }
    static std::string configKey() { return std::string("on-error");};
};

template<>
struct ComponentFailureTraits<ComponentTag::Server> { 
    using onErrorType = OnServerError; 
    static eckit::Optional<onErrorType> parse(const std::string& str) { return parseOnServerError(str); }
    static std::string configKey() { return std::string("on-error");};
};

template<>
struct ComponentFailureTraits<ComponentTag::Plan> { 
    using onErrorType = OnPlanError; 
    static eckit::Optional<onErrorType> parse(const std::string& str) { return parseOnPlanError(str); }
    static std::string configKey() { return std::string("on-error");};
};

template<>
struct ComponentFailureTraits<ComponentTag::Action> { 
    using onErrorType = OnActionError; 
    static eckit::Optional<onErrorType> parse(const std::string& str) { return parseOnActionError(str); }
    static std::string configKey() { return std::string("on-error");};
};

template<>
struct ComponentFailureTraits<ComponentTag::Transport> { 
    using onErrorType = OnTransportError; 
    static eckit::Optional<onErrorType> parse(const std::string& str) { return parseOnTransportError(str); }
    static std::string configKey() { return std::string("on-error");};
};

template<>
struct ComponentFailureTraits<ComponentTag::Dispatcher> { 
    using onErrorType = OnDispatchError; 
    static eckit::Optional<onErrorType> parse(const std::string& str) { return parseOnDispatchError(str); }
    static std::string configKey() { return std::string("on-dispatch-error");};
};

template<>
struct ComponentFailureTraits<ComponentTag::Receiver> { 
    using onErrorType = OnReceiveError; 
    static eckit::Optional<onErrorType> parse(const std::string& str) { return parseOnReceiveError(str); }
    static std::string configKey() { return std::string("on-receive-error");};
};



enum class FailureHandlerResponse : unsigned
{
    Rethrow = 0,
    Continue = 1,
};

template<ComponentTag tag, typename OnErrorType = typename ComponentFailureTraits<tag>::onErrorType>
class FailureAware {
    private:
        util::LocalPeerTag peerTag_;
        eckit::Optional<OnErrorType> parsedOnErrTag_{};
        
    public:
        FailureAware(const ConfigurationContext& confCtx): peerTag_{confCtx.localPeerTag()} {
            ASSERT(confCtx.componentTag() == tag);
            if (confCtx.config().has(ComponentFailureTraits<tag>::configKey())) {
                parsedOnErrTag_ = ComponentFailureTraits<tag>::parse(confCtx.config().getString(ComponentFailureTraits<tag>::configKey()));
            };
        };
        
        virtual FailureHandlerResponse handleFailure(const eckit::Optional<OnErrorType>&) =0;
        // May be overwritten or not
        virtual FailureHandlerResponse handleFailure(const eckit::Optional<OnErrorType>& t, const std::exception& e) {
            return handleFailure(t);
        };
        // May be overwritten or not
        virtual FailureHandlerResponse handleFailure(const eckit::Optional<OnErrorType>& t, const eckit::Exception& e) {
            return handleFailure(t);
        };
        
        FailureHandlerResponse handleFailure() {
            return handleFailure(parsedOnErrTag_);
        };
        FailureHandlerResponse handleFailure(const std::exception& e) {
            return handleFailure(parsedOnErrTag_, e);
        };
        FailureHandlerResponse handleFailure(const eckit::Exception& e) {
            return handleFailure(parsedOnErrTag_, e);
        };
        
        virtual ~FailureAware() = default;
        
    protected:
        template<typename T>
        void withFailureHandling(T&& callable) {
            try {
                callable();
            } catch (...) {
                eckit::Log::error() << "[FailureAware<" << util::toString(tag) << "> " << (parsedOnErrTag_ ? util::toString(*parsedOnErrTag_) : std::string("default handling")) << " on " << util::toString(peerTag_) << "] - Handling ";
                
                try {
                    throw;
                }
                catch(const eckit::Exception& e ) {
                    eckit::Log::error() << "eckit::Exception: " << e.what() << std::endl;
                    e.exceptionStack(eckit::Log::error(), true);
                    if (this->handleFailure(e) == FailureHandlerResponse::Rethrow) {
                        throw;
                    };
                }
                catch(const std::exception& e) {
                    eckit::Log::error() << "std::exception: " << e.what() << std::endl;;
                    if (this->handleFailure(e) == FailureHandlerResponse::Rethrow) {
                        throw;
                    };
                }
                catch(...) {
                    eckit::Log::error() << "general exception" << std::endl;;
                    if (this->handleFailure() == FailureHandlerResponse::Rethrow) {
                        throw;
                    };
                }
            }
        }
};


}  // namespace util
}  // namespace multio

#endif
