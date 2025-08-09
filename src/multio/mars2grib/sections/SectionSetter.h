/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once

#include "multio/datamod/MarsMiscGeo.h"
#include "multio/util/MioGribHandle.h"

#include <functional>
#include <memory>
#include <vector>


// DRAFT - to be discussed if we really need that many setters that an organization like this one is necessary
// The encoder configuration actually already maps to different setters and their configuration.
// If they are described properly through enums, it may be easier to keep them all static
// and do an explicit table lookup via a switch case on enum


namespace multio::mars2grib::sections {

namespace dm = multio::datamod;

// using KeyInfoList = std::vector<std::reference_wrapper<const datamod::DynKeyInfo>>;

// template <auto id_>
// void addKeyInfo(KeyInfoList& l) {
//     l.push_back(std::cref(static_cast<const datamod::DynKeyInfo&>(datamod::key<id_>())));
// }


struct DynSectionSetter {
    struct Config {
        bool registerPrepare;
        bool registerAllocate;
        bool registerPreset;
        bool registerRuntime;
        bool registerCheck;
    };

    // Returns information about which methods are implemented and need to be called
    virtual Config sectionInfo() const = 0;


    // Default implementation is to do nothing
    virtual void prepare(util::MioGribHandle&, const dm::MarsRecord&, const dm::MiscRecord&,
                         const dm::Geometry&) const;
    // Default implementation is to do nothing
    virtual void allocate(util::MioGribHandle&, const dm::MarsRecord&, const dm::MiscRecord&,
                          const dm::Geometry&) const;
    // Default implementation is to do nothing
    virtual void preset(util::MioGribHandle&, const dm::MarsRecord&, const dm::MiscRecord&,
                        const dm::Geometry&) const;
    // Default implementation is to do nothing
    virtual void runtime(util::MioGribHandle&, const dm::MarsRecord&, const dm::MiscRecord&,
                         const dm::Geometry&) const;


    // Implement a check method that is throwing on inconsistencies
    virtual void check(const util::MioGribHandle&, const dm::MarsRecord&, const dm::MiscRecord&,
                       const dm::Geometry&) const;

    // // Implement a check method that is adding dynamic key information to give feed back on requirements
    // virtual void collectKeyInfo(KeyInfoList& required, KeyInfoList& optional, const dm::MarsRecord&) const;

    virtual ~DynSectionSetter() = default;
};


class SectionCollector {
public:
    // Registers a new section
    void add(std::unique_ptr<DynSectionSetter>);

    void prepare(util::MioGribHandle&, const dm::MarsRecord&, const dm::MiscRecord&,
                 const dm::Geometry&) const;
    void allocate(util::MioGribHandle&, const dm::MarsRecord&, const dm::MiscRecord&,
                  const dm::Geometry&) const;
    void preset(util::MioGribHandle&, const dm::MarsRecord&, const dm::MiscRecord&,
                const dm::Geometry&) const;
    void runtime(util::MioGribHandle&, const dm::MarsRecord&, const dm::MiscRecord&,
                 const dm::Geometry&) const;

    void check(const util::MioGribHandle&, const dm::MarsRecord&, const dm::MiscRecord&,
               const dm::Geometry&) const;

    // void collectKeyInfo(KeyInfoList& required, KeyInfoList& optional, const dm::MarsRecord&) const;

    // void writeKeyInfo(std::ostream&, const dm::MarsRecord&) const;

private:
    // Storage of all sections
    std::vector<std::unique_ptr<DynSectionSetter>> sections_;

    // References to specific setters that perform operation in orderd
    std::vector<std::reference_wrapper<const DynSectionSetter>> prepare_;
    std::vector<std::reference_wrapper<const DynSectionSetter>> allocate_;
    std::vector<std::reference_wrapper<const DynSectionSetter>> preset_;
    std::vector<std::reference_wrapper<const DynSectionSetter>> runtime_;
    std::vector<std::reference_wrapper<const DynSectionSetter>> check_;
};


}  // namespace multio::mars2grib::sections

