/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "EcflowReporting.h"

#include <chrono>
#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <sstream>

#include "eckit/log/Log.h"

// ecflow-light C API
#include "ecflow/light/API.h"

namespace multio::server {

namespace {

double wallTime() {
    using clock = std::chrono::high_resolution_clock;
    return std::chrono::duration<double>(clock::now().time_since_epoch()).count();
}

}  // namespace


EcflowReporting::EcflowReporting(double tstep, int rank) : tstep_{tstep}, rank_{rank} {
    // Read the warning threshold from environment, matching PACE_WARN_THRESHOLD in IFS
    if (const char* env = std::getenv("MULTIO_PACE_WARN_THRESHOLD")) {
        paceWarnThreshold_ = std::atof(env);
    }
}

void EcflowReporting::reportStep(int istep) {
    // Only rank 0 (equivalent to MYPROC_IO == 1 in Fortran) reports to ecFlow
    if (rank_ != 0) {
        return;
    }

    // Compute forecast hour from step number and timestep (seconds → hours)
    int forecastHour = static_cast<int>(std::round(istep * tstep_ / 3600.0));

    // --- ecFlow meter update ---
    int err = ecflow_light_update_meter("step", forecastHour);
    if (err != EXIT_SUCCESS) {
        eckit::Log::error() << "EcflowReporting: failed to update ecFlow meter 'step' to " << forecastHour << std::endl;
    }
    else {
        eckit::Log::info() << "EcflowReporting: set ecFlow meter 'step' to " << forecastHour << std::endl;
    }

    // --- Write meter file ---
    writeMeterFile(forecastHour);

    // --- Pacing calculation ---
    if (firstCall_ || forecastHour == 0) {
        startTime_ = wallTime();
        lastForecastHour_ = 0;
        lastTime_ = startTime_;
        paceLastStep_ = 0.0;
        firstCall_ = false;
    }
    else {
        double currentTime = wallTime();
        double elapsed = currentTime - startTime_;
        double averagePace = forecastHour / (elapsed / 3600.0);

        double stepElapsed = currentTime - lastTime_;
        double paceThisStep = (forecastHour - lastForecastHour_) / (stepElapsed / 3600.0);

        std::ostringstream label;
        label << std::fixed << std::setprecision(2);

        if (paceWarnThreshold_ > 0.0 && averagePace < paceWarnThreshold_) {
            label << "WARNING: SLOW FORECAST ";
            eckit::Log::error() << "##IFS_WARNING: SLOW FORECAST Average: " << averagePace
                                << " Latest step: " << paceThisStep << " (" << std::showpos
                                << (paceThisStep - paceLastStep_) << ") fc hours/hour" << std::endl;

            ecflow_light_update_event("ifs_warning", 1);
        }

        label << "Average: " << averagePace << " Latest step: " << paceThisStep << " (" << std::showpos
              << (paceThisStep - paceLastStep_) << std::noshowpos << ") fc hours/hour";

        ecflow_light_update_label("Pace", label.str().c_str());

        lastTime_ = currentTime;
        lastForecastHour_ = forecastHour;
        paceLastStep_ = paceThisStep;
    }
}

void EcflowReporting::writeMeterFile(int forecastHour) const {
    std::ofstream ofs(meterFilePath_);
    if (ofs) {
        ofs << std::setw(8) << forecastHour;
    }
    else {
        eckit::Log::error() << "EcflowReporting: failed to write meter file: " << meterFilePath_ << std::endl;
    }
}

}  // namespace multio::server
