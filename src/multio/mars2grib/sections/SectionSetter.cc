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


void DynSectionSetter::prepare(metkit::codes::CodesHandle&, const dm::FullMarsRecord&, const dm::MiscRecord&,
                               const dm::Geometry&) const {}
void DynSectionSetter::allocate(metkit::codes::CodesHandle&, const dm::FullMarsRecord&, const dm::MiscRecord&,
                                const dm::Geometry&) const {}
void DynSectionSetter::preset(metkit::codes::CodesHandle&, const dm::FullMarsRecord&, const dm::MiscRecord&,
                              const dm::Geometry&) const {}
void DynSectionSetter::runtime(metkit::codes::CodesHandle&, const dm::FullMarsRecord&, const dm::MiscRecord&,
                               const dm::Geometry&) const {}
void DynSectionSetter::check(const metkit::codes::CodesHandle&, const dm::FullMarsRecord&, const dm::MiscRecord&,
                             const dm::Geometry&) const {}
// void DynSectionSetter::collectKeyInfo(KeyInfoList&, KeyInfoList&, const dm::FullMarsRecord&) const {}


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
    if (config.registerCheck) {
        check_.push_back(secRef);
    }
}

void SectionCollector::prepare(metkit::codes::CodesHandle& h, const dm::FullMarsRecord& mars,
                               const dm::MiscRecord& misc, const dm::Geometry& geo) const {
    for (auto secRef : prepare_) {
        secRef.get().prepare(h, mars, misc, geo);
    }
}

void SectionCollector::allocate(metkit::codes::CodesHandle& h, const dm::FullMarsRecord& mars,
                                const dm::MiscRecord& misc, const dm::Geometry& geo) const {
    for (auto secRef : allocate_) {
        secRef.get().allocate(h, mars, misc, geo);
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

// void SectionCollector::collectKeyInfo(KeyInfoList& req, KeyInfoList& opt, const dm::FullMarsRecord& mars) const {
//     for (auto secRef : runtime_) {
//         secRef.get().collectKeyInfo(req, opt, mars);
//     }
// }


// void SectionCollector::writeKeyInfo(std::ostream& os, const dm::FullMarsRecord& mars) const {
//     KeyInfoList req;
//     KeyInfoList opt;
//     collectKeyInfo(req, opt, mars);

//     auto printKey = [&](const auto& dynKey) {
//         os << "  - key: " << dynKey.key() << std::endl;
//         os << "    scope: " << dynKey.initScope() << std::endl;
//         auto descr = dynKey.description();
//         if (descr) {
//             os << "    description: " << *descr << std::endl;
//         }
//         os << std::endl;
//     };

//     auto printKeys = [&](const auto& l) {
//         if (l.size() == 0) {
//             os << "None";
//         }
//         else {
//             for (auto ref : l) {
//                 printKey(ref.get());
//            }
//         }
//     };

//     os << "Required keys for this mars set: " << std::endl;
//     printKeys(req);

//     os << "Optional keys for this mars set: " << std::endl;
//     printKeys(opt);
// }


//-----------------------------------------------------------------------------


}  // namespace multio::mars2grib::sections

