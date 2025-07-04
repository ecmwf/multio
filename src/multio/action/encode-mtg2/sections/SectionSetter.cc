/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */


#include "multio/action/encode-mtg2/sections/SectionSetter.h"
#include "multio/util/MioGribHandle.h"


namespace multio::action::sections {


//-----------------------------------------------------------------------------


void DynSectionSetter::prepare(util::MioGribHandle&, const datamod::MarsKeyValueSet&, const datamod::MiscKeyValueSet&,
                               const datamod::Geometry&) const {}
void DynSectionSetter::allocate(util::MioGribHandle&, const datamod::MarsKeyValueSet&, const datamod::MiscKeyValueSet&,
                                const datamod::Geometry&) const {}
void DynSectionSetter::preset(util::MioGribHandle&, const datamod::MarsKeyValueSet&, const datamod::MiscKeyValueSet&,
                              const datamod::Geometry&) const {}
void DynSectionSetter::runtime(util::MioGribHandle&, const datamod::MarsKeyValueSet&, const datamod::MiscKeyValueSet&,
                               const datamod::Geometry&) const {}


void SectionCollector::add(std::unique_ptr<DynSectionSetter> sect) {
    auto secRef = std::cref(*sections_.emplace_back(std::move(sect)).get());

    auto config = secRef.get().sectionInfo();

    if (config.registerPrepare) {
        prepare_.push_back(secRef);
    }
    if (config.registerAllocate) {
        allocate_.push_back(secRef);
    }
    if (config.registerPreset) {
        preset_.push_back(secRef);
    }
    if (config.registerRuntime) {
        runtime_.push_back(secRef);
    }
}

void SectionCollector::prepare(util::MioGribHandle& h, const datamod::MarsKeyValueSet& mars,
                               const datamod::MiscKeyValueSet& misc, const datamod::Geometry& geo) const {
    for (auto secRef : prepare_) {
        secRef.get().prepare(h, mars, misc, geo);
    }
}

void SectionCollector::allocate(util::MioGribHandle& h, const datamod::MarsKeyValueSet& mars,
                                const datamod::MiscKeyValueSet& misc, const datamod::Geometry& geo) const {
    for (auto secRef : allocate_) {
        secRef.get().allocate(h, mars, misc, geo);
    }
}

void SectionCollector::preset(util::MioGribHandle& h, const datamod::MarsKeyValueSet& mars,
                              const datamod::MiscKeyValueSet& misc, const datamod::Geometry& geo) const {
    for (auto secRef : preset_) {
        secRef.get().preset(h, mars, misc, geo);
    }
}

void SectionCollector::runtime(util::MioGribHandle& h, const datamod::MarsKeyValueSet& mars,
                               const datamod::MiscKeyValueSet& misc, const datamod::Geometry& geo) const {
    for (auto secRef : runtime_) {
        secRef.get().runtime(h, mars, misc, geo);
    }
}


//-----------------------------------------------------------------------------


}  // namespace multio::action::sections

