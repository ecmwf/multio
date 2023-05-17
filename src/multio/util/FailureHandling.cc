
#include "eckit/exception/Exceptions.h"

#include "FailureHandling.h"

using namespace multio::util;

std::string eckit::Translator<OnClientError, std::string>::operator()(OnClientError tag) {
    switch (tag) {
        case OnClientError::Propagate:
            return std::string("propagate");
        case OnClientError::Recover:
            return std::string("recover");
        case OnClientError::AbortAllTransports:
            return std::string("abort-all-transports");
        default:
            throw eckit::SeriousBug("Unknown OnClientError tag", Here());
    }
}

std::string eckit::Translator<OnServerError, std::string>::operator()(OnServerError tag) {
    switch (tag) {
        case OnServerError::Propagate:
            return std::string("propagate");
        case OnServerError::Recover:
            return std::string("recover");
        case OnServerError::AbortTransport:
            return std::string("abort-transport");
        default:
            throw eckit::SeriousBug("Unknown OnServerError tag", Here());
    }
}

std::string eckit::Translator<OnPlanError, std::string>::operator()(OnPlanError tag) {
    switch (tag) {
        case OnPlanError::Propagate:
            return std::string("propagate");
        case OnPlanError::Recover:
            return std::string("recover");
        default:
            throw eckit::SeriousBug("Unknown OnPlanError tag", Here());
    }
}

std::string eckit::Translator<OnActionError, std::string>::operator()(OnActionError tag) {
    switch (tag) {
        case OnActionError::Propagate:
            return std::string("propagate");
        case OnActionError::Recover:
            return std::string("recover");
        default:
            throw eckit::SeriousBug("Unknown OnActionError tag", Here());
    }
}

std::string eckit::Translator<OnTransportError, std::string>::operator()(OnTransportError tag) {
    switch (tag) {
        case OnTransportError::Propagate:
            return std::string("propagate");
        case OnTransportError::Recover:
            return std::string("recover");
        default:
            throw eckit::SeriousBug("Unknown OnTransportError tag", Here());
    }
}

std::string eckit::Translator<OnReceiveError, std::string>::operator()(OnReceiveError tag) {
    switch (tag) {
        case OnReceiveError::Propagate:
            return std::string("propagate");
        default:
            throw eckit::SeriousBug("Unknown OnReceiveError tag", Here());
    }
}

std::string eckit::Translator<OnDispatchError, std::string>::operator()(OnDispatchError tag) {
    switch (tag) {
        case OnDispatchError::Propagate:
            return std::string("propagate");
        default:
            throw eckit::SeriousBug("Unknown OnDispatchError tag", Here());
    }
}

namespace multio::util {

namespace {
inline void printExceptionHeader(std::ostream& out, const std::exception& e, int level = 0) {
    out << std::endl << "  * " << (level + 1) << ": " << e.what() << std::endl;
}

int printNestedException(std::ostream& out, const std::exception& e) {
    int level = 0;
    try {
        std::rethrow_if_nested(e);
    }
    catch (const FailureAwareException& nestedException) {
        level = printNestedException(out, nestedException);
    }
    catch (const eckit::Exception& nestedException) {
        level = printNestedException(out, nestedException);
    }
    catch (const std::exception& nestedException) {
        level = printNestedException(out, nestedException);
    }
    catch (...) {
        return level + 1;
    }
    printExceptionHeader(out, e, level);
    return level + 1;
}
}  // namespace

void printException(std::ostream& out, const std::exception& e) {
    out << std::endl;
    out << "Nested std::Exception: " << std::endl;
    printNestedException(out, e);
    out << std::endl;
}
void printException(std::ostream& out, const eckit::Exception& e) {
    out << std::endl;
    out << "Nested eckit::Exception: " << std::endl;
    printNestedException(out, e);
    out << std::endl;
    e.exceptionStack(out, true);
    out << std::endl;
    out << std::endl;
}
void printException(std::ostream& out, const FailureAwareException& e) {
    out << std::endl;
    out << "Nested FailureAwareException: " << std::endl;
    printNestedException(out, e);
    out << std::endl;
    e.exceptionStack(out, true);
    out << std::endl;
    out << std::endl;
}

void printFailureContext(std::ostream& out, const FailureContext& c) {
    if (c.eptr) {
        try {
            try {
                std::rethrow_exception(c.eptr);
            }
            catch (...) {
                std::throw_with_nested(FailureAwareException(c.context, Here()));
            }
        }
        catch (const FailureAwareException& e) {
            printException(out, e);
        }
    }
    else {
        try {
            throw FailureAwareException(c.context, Here());
        }
        catch (const FailureAwareException& e) {
            printException(out, e);
        }
    }
}

std::ostream& operator<<(std::ostream& os, const FailureAwareException& dt) {
    printException(os, dt);
    return os;
}

std::ostream& operator<<(std::ostream& os, const FailureContext& dt) {
    printFailureContext(os, dt);
    return os;
}

}  // namespace multio::util
