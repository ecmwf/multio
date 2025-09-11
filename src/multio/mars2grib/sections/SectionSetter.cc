/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */


#include "multio/mars2grib/sections/SectionSetter.h"


namespace multio::mars2grib::sections {


//-----------------------------------------------------------------------------


void DynSectionSetter::preset(metkit::codes::CodesHandle&, const dm::FullMarsRecord&, const dm::MiscRecord&,
                              const dm::Geometry&) const {}
void DynSectionSetter::runtime(metkit::codes::CodesHandle&, const dm::FullMarsRecord&, const dm::MiscRecord&,
                               const dm::Geometry&) const {}
void DynSectionSetter::check(const metkit::codes::CodesHandle&, const dm::FullMarsRecord&, const dm::MiscRecord&,
                             const dm::Geometry&) const {}


void SectionCollector::add(std::unique_ptr<DynSectionSetter> sect) {
    auto secRef = std::cref(*sections_.emplace_back(std::move(sect)).get());

    auto config = secRef.get().sectionInfo();

    if (config.registerPreset) {
        preset_.push_back(secRef);
    }
    if (config.registerRuntime) {
        runtime_.push_back(secRef);
    }
    if (config.registerCheck) {
        check_.push_back(secRef);
    }
}


void SectionCollector::preset(metkit::codes::CodesHandle& h, const dm::FullMarsRecord& mars, const dm::MiscRecord& misc,
                              const dm::Geometry& geo) const {
    for (auto secRef : preset_) {
        secRef.get().preset(h, mars, misc, geo);
    }
}

void SectionCollector::runtime(metkit::codes::CodesHandle& h, const dm::FullMarsRecord& mars,
                               const dm::MiscRecord& misc, const dm::Geometry& geo) const {
    for (auto secRef : runtime_) {
        secRef.get().runtime(h, mars, misc, geo);
    }
}

void SectionCollector::check(const metkit::codes::CodesHandle& h, const dm::FullMarsRecord& mars,
                             const dm::MiscRecord& misc, const dm::Geometry& geo) const {
    for (auto secRef : runtime_) {
        secRef.get().check(h, mars, misc, geo);
    }
}


//-----------------------------------------------------------------------------


}  // namespace multio::mars2grib::sections

