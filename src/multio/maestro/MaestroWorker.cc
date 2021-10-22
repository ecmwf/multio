
#include "MaestroWorker.h"

#include "eckit/io/FileHandle.h"
#include "eckit/linalg/LinearAlgebra.h"

#include "mir/api/MIRComplexJob.h"
#include "mir/api/MIRJob.h"
#include "mir/input/MIRInput.h"

#include "multio/LibMultio.h"

#include "pgen/handler/Handler.h"
#include "pgen/prodgen/Fields.h"
#include "pgen/prodgen/Output.h"

using multio::LibMultio;

namespace multio {

MaestroWorker::MaestroWorker(const eckit::option::CmdArgs& args, eckit::Queue<pgen::Requirement>& queue) :
    args_{args},
    source_{args, maestroStatistics_},
    namer_{pgen::FileNamerFactory::build(args)},
    queue_{queue},
    requirement_{},
    directory_("."),
    style_{"ecmwf"},
    legendre_loader_{"file-io"},
    matrix_loader_{"file-io"},
    point_search_{"mapped-anonymous-memory"}
{
    args.get("directory", directory_);
    args.get("style", style_);
    args.get("legendre-loader", legendre_loader_);
    args.get("matrix-loader", matrix_loader_);
    args.get("point-search-trees", point_search_);

    std::string backend;
    if (args.get("backend", backend)) {
        eckit::linalg::LinearAlgebra::backend(backend);
    }
}

MaestroWorker::~MaestroWorker() {
//    maestroStatistics_.report(eckit::Log::info());
}

void MaestroWorker::process() {
//    eckit::AutoTiming process_timing(maestroStatistics_.timer_, maestroStatistics_.workerProcessTiming_);
    LOG_DEBUG_LIB(LibMultio) << "*** Hi from worker" << std::endl;
    std::string lastInputTag;
    mir::api::MIRComplexJob job;
    pgen::Fields fields;
    std::unique_ptr<mir::input::MIRInput> input;

    while (queue_.pop(requirement_) > -1) {
//        eckit::AutoTiming pop_work_timing(maestroStatistics_.timer_, maestroStatistics_.workerProcessPopWorkTiming_);
        const pgen::Handler &handler = pgen::Handler::lookup(args_, requirement_.handlers());
        const std::string inputTag = handler.inputTag(requirement_);
        LOG_DEBUG_LIB(LibMultio) << "INPUT TAG [" << inputTag << "]" << std::endl;
        if (job.empty() || (lastInputTag != inputTag)) {
            LOG_DEBUG_LIB(LibMultio) << "SET INPUT TAG [" << inputTag << "]" << std::endl;
            lastInputTag = inputTag;
            job.clear();
            fields.clear();
            try {
//                eckit::AutoTiming input_timing(maestroStatistics_.timer_, maestroStatistics_.workerProcessInputTiming_);
                LOG_DEBUG_LIB(LibMultio) << "Handle input from " << source_ << std::endl;
                input.reset(handler.input(requirement_,
                                          fields,
                                          source_,
                                          statistics_));
                if (!input) {
//                    eckit::Log::info() << "No input" << std::endl;
                    continue;
                }
            } catch (pgen::DataNotFound &e) {
//                eckit::Log::info() << "==== Error retrieving data" << std::endl;
//                eckit::Log::info() << "==== Exception  : " << e.what() << std::endl;
//                eckit::Log::info() << "==== Requirement: " << requirement_ << std::endl;
                statistics_.fieldsNotFoundCount_++;
                statistics_.failedRequirementsCount_++;
            }
        }

        std::stringstream ss;
        ss << std::this_thread::get_id();
        std::string thread_id = ss.str();

        std::string path = handler.path(*namer_, requirement_, directory_, thread_id);
        pgen::Output o{new eckit::FileHandle{path}, true /* append */};
        mir::output::MIROutput &output = o.output(handler, statistics_);

        try {
//            eckit::AutoTiming prepare_timing(maestroStatistics_.timer_, maestroStatistics_.workerProcessJobPrepareTiming_);
            std::unique_ptr<mir::api::MIRJob> mj(new mir::api::MIRJob());
            if (!handler.prepare(*mj, requirement_, *input, output, statistics_)) {
//                eckit::Log::info() << "Handler did not prepare" << std::endl;
                continue;
            }
            mj->set("style", style_);
            mj->set("legendre-loader", legendre_loader_);
            mj->set("matrix-loader", matrix_loader_);
            mj->set("point-search-trees", point_search_);

            LOG_DEBUG_LIB(LibMultio) << "Job before add ===> " << *mj <<  std::endl;
            job.add(mj.release(),
                    *input,
                    output,
                    nullptr);
            LOG_DEBUG_LIB(LibMultio) << "Job after add ===> " <<  std::endl;
        } catch (std::exception &e) {
//            eckit::Log::info() << "==== Error building job" << std::endl;
//            eckit::Log::info() << "==== Exception  : " << e.what() << std::endl;
//            eckit::Log::info() << "==== Requirement: " << requirement_ << std::endl;
            statistics_.failedRequirementsCount_++;
        }
        LOG_DEBUG_LIB(LibMultio) << "Execute job" << std::endl;
//        eckit::AutoTiming timing(statistics_.timer_, statistics_.mirTiming_);
        job.execute(statistics_.mirStatistics_);
    }

//    statistics_.report(eckit::Log::info());
//    eckit::Log::info() << "*** Worker is leaving" << std::endl;
}

void execute_worker(const eckit::option::CmdArgs& args, eckit::Queue<pgen::Requirement>& req_queue) {
    MaestroWorker worker{std::cref(args), req_queue};
    worker.process();
}

}  // namespace multio
