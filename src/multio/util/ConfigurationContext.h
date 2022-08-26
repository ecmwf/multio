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

/// @date Aug 2022

#ifndef multio_util_ConfigurationContext_H
#define multio_util_ConfigurationContext_H

#include "eckit/config/LocalConfiguration.h"
#include "multio/util/ConfigurationPath.h"
#include "multio/util/IteratorMapper.h"

namespace multio {
namespace util {

class SubContextIteratorMapper;

class ConfigurationContext {
public:

    ConfigurationContext(const eckit::PathName& pathName, const eckit::PathName& fileName);
    ConfigurationContext(const eckit::LocalConfiguration& config, const eckit::PathName& pathName,
                         const eckit::PathName& fileName);
    ConfigurationContext(const eckit::LocalConfiguration& config,
                         const eckit::LocalConfiguration& globalConfig,
                         const eckit::PathName& pathName, const eckit::PathName& fileName);

    eckit::LocalConfiguration& config();
    const eckit::LocalConfiguration& config() const;
    const eckit::LocalConfiguration& globalConfig() const;
    const eckit::PathName& pathName() const;
    const eckit::PathName& fileName() const;

    using SubConfigurationContexts =
        MappedContainer<std::vector<eckit::LocalConfiguration>,
                                     SubContextIteratorMapper>;

    ConfigurationContext subContext(const std::string& subConfiguratinKey) const;
    SubConfigurationContexts subContexts(const std::string& subConfiguratinKey) const;
    ConfigurationContext recast(const eckit::LocalConfiguration& config) const;

private:
    eckit::LocalConfiguration config_;
    eckit::LocalConfiguration globalConfig_;
    eckit::PathName pathName_;
    eckit::PathName fileName_;
    
    friend class SubContextIteratorMapper;
};

class SubContextIteratorMapper{
    public:
        SubContextIteratorMapper(const ConfigurationContext& confCtx);
        SubContextIteratorMapper(ConfigurationContext&& confCtx);

        ConfigurationContext operator()(const eckit::LocalConfiguration& config) const;

    private:
        ConfigurationContext confCtx_;
};


}  // namespace util
}  // namespace multio

#endif
