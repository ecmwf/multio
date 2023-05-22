/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Jan 2019

#pragma once

#include <iosfwd>
#include <map>
#include <mutex>
#include <string>

#include "eckit/exception/Exceptions.h"
#include "eckit/memory/NonCopyable.h"

#include "multio/message/Message.h"
#include "multio/transport/TransportStatistics.h"
#include "multio/util/ConfigurationContext.h"


namespace multio {
namespace transport {

using message::Message;
using message::Peer;
using util::ConfigurationContext;

using PeerList = std::vector<std::unique_ptr<message::Peer>>;

//----------------------------------------------------------------------------------------------------------------------

class Transport {
public:  // methods
    Transport(const ConfigurationContext& confCtx);
    virtual ~Transport();

    virtual void openConnections() = 0;
    virtual void closeConnections() = 0;

    virtual Message receive() = 0;

    virtual void abort() = 0;

    virtual void send(const Message& message) = 0;

    virtual void bufferedSend(const Message& message) = 0;

    virtual Peer localPeer() const = 0;

    virtual void listen();

    virtual PeerList createServerPeers() const = 0;

    const PeerList& clientPeers() const;
    const PeerList& serverPeers() const;

    virtual size_t clientCount() const;
    virtual size_t serverCount() const;

protected:
    const ConfigurationContext confCtx_;

    mutable PeerList serverPeers_;
    mutable PeerList clientPeers_;

    TransportStatistics statistics_;

    std::mutex mutex_;

private:  // methods
    bool peersMissing() const;

    virtual void createPeers() const = 0;

    virtual void print(std::ostream& os) const = 0;

    friend std::ostream& operator<<(std::ostream& os, const Transport& transport) {
        transport.print(os);
        return os;
    }
};

//----------------------------------------------------------------------------------------------------------------------

class TransportBuilderBase;

class TransportFactory : private eckit::NonCopyable {

public:
    static TransportFactory& instance();

    void add(const std::string& name, const TransportBuilderBase* builder);

    void remove(const std::string& name);

    void list(std::ostream&) const;

    std::unique_ptr<Transport> build(const std::string&, const ConfigurationContext& confCtx);

private:
    TransportFactory() = default;

    std::map<std::string, const TransportBuilderBase*> factories_;

    mutable std::recursive_mutex mutex_;
};

class TransportBuilderBase : private eckit::NonCopyable {
public:  // methods
    virtual std::unique_ptr<Transport> make(const ConfigurationContext& confCtx) const = 0;

protected:  // methods
    TransportBuilderBase(const std::string&);

    virtual ~TransportBuilderBase();

    std::string name_;
};

template <class T>
class TransportBuilder final : public TransportBuilderBase {
    std::unique_ptr<Transport> make(const ConfigurationContext& confCtx) const override { return std::make_unique<T>(confCtx); }

public:
    TransportBuilder(const std::string& name) : TransportBuilderBase(name) {}
};

//----------------------------------------------------------------------------------------------------------------------


class TransportException : public eckit::Exception {
public:
    TransportException(const std::string& reason, const eckit::CodeLocation& l = eckit::CodeLocation());
};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace transport
}  // namespace multio
