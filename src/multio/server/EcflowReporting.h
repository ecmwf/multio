/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#pragma once

#include <cstdint>
#include <string>

namespace multio::server {

/// Handles ecFlow meter/label/event updates after server-side synchronisation,
/// mirroring the logic of IO_SERV_CLOSE_EC in arpifs/io_serv/io_serv_close_ec.F90.
class EcflowReporting {
public:
    /// @param tstep  Model timestep in seconds
    /// @param rank   Rank within the IO server communicator (0-based)
    EcflowReporting(double tstep, int rank);

    /// Called once per step after the server barrier.
    /// @param istep  Current model step number
    void reportStep(int istep);

private:
    void writeMeterFile(int forecastHour) const;

    double tstep_;
    int rank_;

    // Pacing state (persistent across calls, like Fortran SAVEs)
    double startTime_ = 0.0;
    double lastTime_ = 0.0;
    double paceLastStep_ = 0.0;
    int lastForecastHour_ = 0;
    bool firstCall_ = true;

    std::string meterFilePath_ = "../ioserv_meter";
    double paceWarnThreshold_ = 0.0;
};

}  // namespace multio::server
