/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Pedro Maciel

/// @date Sep 2023

#pragma once

namespace multio::action::renumber_healpix {


class HEALPix {
public:
    explicit HEALPix(int Nside);

    int size() const { return 12 * Nside_ * Nside_; }
    int nside() const { return Nside_; }

    int nest_to_ring(int) const;
    int ring_to_nest(int) const;

private:
    const int Nside_;  // up to 2^13
    const int Npix_;
    const int Ncap_;
    const int k_;
};


}  // namespace multio::action::renumber_healpix
