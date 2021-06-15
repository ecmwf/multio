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

#ifndef multio_server_Transport_H
#define multio_server_Transport_H

#include <iosfwd>
#include <string>
#include <map>
#include <mutex>

#include "eckit/memory/NonCopyable.h"
#include "eckit/config/Configuration.h"

#include "multio/message/Message.h"
#include "multio/server/TransportStatistics.h"

namespace multio {
namespace server {

using message::Message;
using message::Peer;

using PeerList = std::vector<std::unique_ptr<message::Peer>>;

//----------------------------------------------------------------------------------------------------------------------

class Transport {
public:  // methods

    Transport(const eckit::Configuration& config);
    virtual ~Transport() = default;

    virtual void openConnections() = 0;
    virtual void closeConnections() = 0;

    virtual Message receive() = 0;

    virtual void send(const Message& message) = 0;

    virtual void bufferedSend(const Message& message) = 0;

    virtual Peer localPeer() const = 0;

    virtual void listen();

    virtual PeerList createServerPeers() = 0;

protected:
    const eckit::LocalConfiguration config_;

    TransportStatistics statistics_;

private: // methods
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

    Transport* build(const std::string&, const eckit::Configuration& config);

private:
    TransportFactory() = default;

    std::map<std::string, const TransportBuilderBase*> factories_;

    mutable std::recursive_mutex mutex_;
};

class TransportBuilderBase : private eckit::NonCopyable {
public:  // methods
    virtual Transport* make(const eckit::Configuration& config) const = 0;

protected:  // methods
    TransportBuilderBase(const std::string&);

    virtual ~TransportBuilderBase();

    std::string name_;
};

template <class T>
class TransportBuilder final : public TransportBuilderBase {
    Transport* make(const eckit::Configuration& config) const override { return new T(config); }

public:
    TransportBuilder(const std::string& name) : TransportBuilderBase(name) {}
};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace server
}  // namespace multio

#endif
