
#include "FailureHandling.h"

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
    }
}

std::string toString(OnActionError tag) {
    switch (tag) {
        case OnActionError::Propagate:
            return std::string("propagate");
        case OnActionError::Recover:
            return std::string("recover");
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

}  // namespace util
}  // namespace multio
