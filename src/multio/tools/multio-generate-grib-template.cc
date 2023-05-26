#include "multio/LibMultio.h"
#include "multio/tools/MultioTool.h"

#include "atlas/functionspace.h"
#include "atlas/grid.h"
#include "atlas/library.h"

#include "eccodes.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/io/StdFile.h"
#include "eckit/log/CodeLocation.h"
#include "eckit/log/Log.h"
#include "eckit/mpi/Comm.h"
#include "eckit/option/SimpleOption.h"

namespace {
    void handleCodesError(const std::string& errorPrefix, int error, const eckit::CodeLocation& codeLocation) {
        if (error) {
            std::ostringstream oss;
            oss << errorPrefix << codes_get_error_message(error);
            throw eckit::Exception(oss.str(), codeLocation);
        }
    }

    long getGribVersion(codes_handle* handle) {
        long gribVersion = 0;
        int err = codes_get_long(handle, "editionNumber", &gribVersion);
        handleCodesError("eccodes error while reading the GRIB version: ", err, Here());
        return gribVersion;
    }

    void setIndicatorOfTypeOfLevelIfNeeded(codes_handle* handle, const eckit::option::CmdArgs& args) {
        long gribVersion = getGribVersion(handle);
        if (gribVersion == 1) {
            std::string indicatorOfTypeOfLevel = "sfc";
            args.get("indicatorOfTypeOfLevel", indicatorOfTypeOfLevel);
            auto size = indicatorOfTypeOfLevel.size();
            int err = codes_set_string(handle, "indicatorOfTypeOfLevel", indicatorOfTypeOfLevel.c_str(), &size);
            handleCodesError("eccodes error while setting the indicatorOfTypeOfLevel: ", err, Here());
        }
    }

    void setReducedGGFields(codes_handle* handle, const eckit::option::CmdArgs& args) {
        auto& originalComm = eckit::mpi::comm();
        eckit::mpi::setCommDefault("self");

        std::string gridType = "none";
        args.get("grid", gridType);
        const atlas::Grid grid(gridType);

        eckit::mpi::setCommDefault(originalComm.name().c_str());

        auto structuredGrid = atlas::StructuredGrid(grid);

        auto gaussianGrid = atlas::GaussianGrid(structuredGrid);
        int err = codes_set_long(handle, "N", gaussianGrid.N());
        handleCodesError("eccodes error while setting the N value: ", err, Here());

        auto tmp = structuredGrid.nx();
        std::vector<long> pl(tmp.size(), 0);
        for (int i = 0; i < tmp.size(); ++i) {
            pl[i] = long(tmp[i]);
        }

        err = codes_set_long_array(handle, "pl", pl.data(), pl.size());
        handleCodesError("eccodes error while setting the PL array: ", err, Here());

        std::vector<double> values(grid.size(), 0.0);
        double maxLongitude = 0;
        for (const auto p : grid.xy()) {
            if (maxLongitude < p.x()) {
                maxLongitude = p.x();
            }
        }

        auto it = grid.lonlat().begin();
        err = codes_set_double(handle, "latitudeOfFirstGridPointInDegrees", (*it)[1]);
        handleCodesError("eccodes error while setting the latitudeOfFirstGridPointInDegrees: ", err, Here());
        err = codes_set_double(handle, "longitudeOfFirstGridPointInDegrees", (*it)[0]);
        handleCodesError("eccodes error while setting the longitudeOfFirstGridPointInDegrees: ", err, Here());
        it += grid.size() - 1;
        err = codes_set_double(handle, "latitudeOfLastGridPointInDegrees", (*it)[1]);
        handleCodesError("eccodes error while setting the latitudeOfLastGridPointInDegrees: ", err, Here());
        err = codes_set_double(handle, "longitudeOfLastGridPointInDegrees", (*it)[0]);
        handleCodesError("eccodes error while setting the longitudeOfLastGridPointInDegrees: ", err, Here());

        err = codes_set_double_array(handle, "values", values.data(), values.size());
        handleCodesError("eccodes error while setting the values array: ", err, Here());

        err = codes_set_double(handle, "longitudeOfLastGridPointInDegrees", maxLongitude);
        handleCodesError("eccodes error while setting the longitudeOfLastGridPointInDegrees value: ", err, Here());

        setIndicatorOfTypeOfLevelIfNeeded(handle, args);
    }

    void setSHFields(codes_handle* handle, const eckit::option::CmdArgs& args) {
        int err = codes_set_long(handle, "grib2LocalSectionPresent", 1);
        handleCodesError("eccodes error while enabling section 2 in sh sample: ", err, Here());

        long spectralTruncation = 79;
        args.get("spectralTruncation", spectralTruncation);

        err = codes_set_long(handle, "J", spectralTruncation);
        handleCodesError("eccodes error while setting J: ", err, Here());

        err = codes_set_long(handle, "K", spectralTruncation);
        handleCodesError("eccodes error while setting K: ", err, Here());

        err = codes_set_long(handle, "M", spectralTruncation);
        handleCodesError("eccodes error while setting M: ", err, Here());

        auto spectral = atlas::functionspace::Spectral(spectralTruncation);
        auto size = spectral.nb_spectral_coefficients_global();
        std::vector<double> values(size, 0.0);
        err = codes_set_double_array(handle, "values", values.data(), values.size());
        handleCodesError("eccodes error while setting the values array: ", err, Here());
    }

    void setReducedLLFields(codes_handle* handle, const eckit::option::CmdArgs& args) {
        setIndicatorOfTypeOfLevelIfNeeded(handle, args);
    }
}

void setHealpixFields(codes_handle* handle, const eckit::option::CmdArgs& args) {
    auto& originalComm = eckit::mpi::comm();
    eckit::mpi::setCommDefault("self");

    std::string gridType = "none";
    args.get("grid", gridType);
    const atlas::Grid grid(gridType);

    eckit::mpi::setCommDefault(originalComm.name().c_str());

    auto structuredGrid = atlas::StructuredGrid(grid);

    auto healPixGrid = atlas::HealpixGrid(structuredGrid);

    const auto side = healPixGrid.N();
    int err = codes_set_long(handle, "Nside", side);
    handleCodesError("eccodes error while setting the Nside value: ", err, Here());

    const auto size = 12 * side * side;
    std::vector<double> values(size, 0.0);
    err = codes_set_double_array(handle, "values", values.data(), values.size());
    handleCodesError("eccodes error while setting the values array: ", err, Here());

    double firstPoint = 45.0;
    err = codes_set_double(handle, "longitudeOfFirstGridPointInDegrees", firstPoint);
    handleCodesError("eccodes error while setting the longitudeOfFirstGridPointInDegrees value: ", err, Here());
}

}  // namespace

//----------------------------------------------------------------------------------------------------------------

class MultioGenerateGribTemplate final : public multio::MultioTool {
public:  // methods

    MultioGenerateGribTemplate(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs& args) override;

    void execute(const eckit::option::CmdArgs& args) override;
};

MultioGenerateGribTemplate::MultioGenerateGribTemplate(int argc, char** argv) : multio::MultioTool(argc, argv) {
    options_.push_back(new eckit::option::SimpleOption<std::string>("grid", "Grid type"));
    options_.push_back(new eckit::option::SimpleOption<std::string>("sample", "Grid sample path"));
    options_.push_back(new eckit::option::SimpleOption<std::string>("output", "Output grib file path"));
    options_.push_back(new eckit::option::SimpleOption<std::string>("indicatorOfTypeOfLevel", "Level type"));
    options_.push_back(new eckit::option::SimpleOption<long>("generatingProcessIdentifier", "Generating process identifier"));
    options_.push_back(new eckit::option::SimpleOption<long>("spectralTruncation", "Spectral truncation number"));
    options_.push_back(new eckit::option::SimpleOption<bool>("clearValuesOnly", "Only clear the stored values"));
}

void MultioGenerateGribTemplate::init(const eckit::option::CmdArgs& args) {
    atlas::initialize();
}

void MultioGenerateGribTemplate::finish(const eckit::option::CmdArgs&) {
    atlas::finalize();
}

void MultioGenerateGribTemplate::execute(const eckit::option::CmdArgs& args) {
    std::string gribSamplePath = "";
    args.get("sample", gribSamplePath);
    eckit::AutoStdFile fin{gribSamplePath};
    int err;
    auto sampleHandle = codes_handle_new_from_file(nullptr, fin, PRODUCT_GRIB, &err);
    handleCodesError("eccodes error while reading template file: ", err, Here());

<<<<<<< HEAD
    char buffer[255] = {0};
    size_t bufferLen{255};
    err = codes_get_string(sampleHandle, "gridType", buffer, &bufferLen);
    handleCodesError("eccodes error while reading the grid type: ", err, Here());

    using UpdateFunctionType = std::function<void(codes_handle*, const eckit::option::CmdArgs&)>;
    const std::unordered_map<std::string, UpdateFunctionType> updateFunctionMap
        = {{"reduced_gg", &setReducedGGFields}, {"sh", &setSHFields}, {"healpix", &setHealpixFields}};

    const std::string sampleGridType(buffer);
    if (updateFunctionMap.count(sampleGridType) != 0) {
        auto updateFunction = updateFunctionMap.at(sampleGridType);
        updateFunction(sampleHandle, args);
    }
    else {
        std::ostringstream oss;
        oss << "Grid type " << sampleGridType << " is currently not supported!";
        throw eckit::Exception(oss.str(), Here());
    }

    bool clearValues = false;
    args.get("clearValuesOnly", clearValues);
    if (!clearValues) {
        long generatingProcessIdentifier = 153;
        args.get("generatingProcessIdentifier", generatingProcessIdentifier);
        err = codes_set_long(sampleHandle, "generatingProcessIdentifier", generatingProcessIdentifier);
        handleCodesError("eccodes error while setting the generatingProcessIdentifier: ", err, Here());

        char buffer[255] = {0};
        size_t bufferLen{255};
        err = codes_get_string(sampleHandle, "gridType", buffer, &bufferLen);
        handleCodesError("eccodes error while reading the grid type: ", err, Here());

        using UpdateFunctionType = std::function<void(codes_handle*, const eckit::option::CmdArgs&)>;
        const std::unordered_map<std::string, UpdateFunctionType> updateFunctionMap = {
            { "reduced_gg", &setReducedGGFields },
            { "sh", &setSHFields },
            { "reduced_ll", &setReducedLLFields }
        };

        const std::string sampleGridType(buffer);
        if (updateFunctionMap.count(sampleGridType) != 0) {
            auto updateFunction = updateFunctionMap.at(sampleGridType);
            updateFunction(sampleHandle, args);
        } else {
            std::ostringstream oss;
            oss << "Grid type " << sampleGridType << " is currently not supported!";
            throw eckit::Exception(oss.str(), Here());
        }
    } else {
        size_t numValues = 0;
        err = codes_get_size(sampleHandle, "values", &numValues);
        handleCodesError("eccodes error while reading the number of values: ", err, Here());

        std::vector<double> values(numValues, 0.0);
        err = codes_set_double_array(sampleHandle, "values", values.data(), values.size());
        handleCodesError("eccodes error while setting the values array: ", err, Here());
    }

    std::string outputFilePath = "";
    args.get("output", outputFilePath);
    err = codes_write_message(sampleHandle, outputFilePath.c_str(), "wb");
    handleCodesError("eccodes error while writing output file: ", err, Here());

    err = codes_handle_delete(sampleHandle);
    handleCodesError("eccodes error while freeing the template handle: ", err, Here());
}

//---------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    MultioGenerateGribTemplate tool(argc, argv);
    return tool.start();
}
