
#include <fstream>
#include "MaestroWorker.h"

#include "eckit/io/FileHandle.h"
#include "eckit/linalg/LinearAlgebra.h"

#include "mir/api/MIRComplexJob.h"
#include "mir/api/MIRJob.h"
#include "mir/input/MIRInput.h"

#include "multio/LibMultio.h"
#include "multio/util/ScopedTimer.h"

#include "pgen/handler/Handler.h"
#include "pgen/prodgen/Fields.h"
#include "pgen/prodgen/Output.h"

using multio::LibMultio;

namespace multio {

MaestroWorker::MaestroWorker(const eckit::option::CmdArgs& args, eckit::Queue<pgen::Requirement>& queue) :
    args_{args},
    source_{args},
    queue_{queue},
    requirement_{},
    namer_{pgen::FileNamerFactory::build(args)},
    directory_("."),
    style_{"ecmwf"},
    legendre_loader_{"file-io"},
    matrix_loader_{"file-io"},
    point_search_{"mapped-anonymous-memory"},
    dryrun_{false},
    log_file_{get_log_name(), std::fstream::app} {
    args.get("directory", directory_);
    args.get("style", style_);
    args.get("legendre-loader", legendre_loader_);
    args.get("matrix-loader", matrix_loader_);
    args.get("point-search-trees", point_search_);
    args.get("dryrun", dryrun_);

    std::string backend;
    if (args.get("backend", backend)) {
        eckit::linalg::LinearAlgebra::backend(backend);
    }
}

MaestroWorker::~MaestroWorker() {
    maestroStatistics_.report(log_file_);
}

void MaestroWorker::process() {
    util::ScopedTiming process_timing(maestroStatistics_.workerProcessTimer_, maestroStatistics_.workerProcessTiming_);
    log_file_ << "*** Hi from worker" << std::endl;

    if (dryrun_) {
        while (queue_.pop(requirement_) > -1) {
            if (not start_.elapsed_) {
                start_ = eckit::Timing(statistics_.timer());
                log_file_ << "(timing) [start_] = " << start_ << std::endl;
            }
            util::ScopedTiming pop_work_timing(maestroStatistics_.workerProcessPopWorkTimer_, maestroStatistics_.workerProcessPopWorkTiming_);
            log_file_ << requirement_ << std::endl;
            eckit::Buffer b;
            source_.retrieve(requirement_.retrieve(), b);
            log_file_ << requirement_ << "COMPLETED" << std::endl;
        }
    } else {
        std::string lastInputTag;
        mir::api::MIRComplexJob job;
        pgen::Fields fields;
        std::unique_ptr<mir::input::MIRInput> input;

        while (queue_.pop(requirement_) > -1) {
            if (not start_.elapsed_) {
                start_ = eckit::Timing(statistics_.timer());
                log_file_ << "(timing) [start_] = " << start_ << std::endl;
            }
            util::ScopedTiming pop_work_timing(maestroStatistics_.workerProcessPopWorkTimer_, maestroStatistics_.workerProcessPopWorkTiming_);
            const pgen::Handler &handler = pgen::Handler::lookup(args_, requirement_.handlers());
            const std::string inputTag = handler.inputTag(requirement_);
            log_file_ << "INPUT TAG [" << inputTag << "]" << std::endl;
            if (job.empty() || (lastInputTag != inputTag)) {
                util::ScopedTiming input_timing(maestroStatistics_.workerProcessInputTimer_, maestroStatistics_.workerProcessInputTiming_);
                log_file_ << "SET INPUT TAG [" << inputTag << "]" << std::endl;
                lastInputTag = inputTag;
                job.clear();
                fields.clear();
                try {
                    log_file_ << "Handle input from " << source_ << std::endl;
                    input.reset(handler.input(requirement_,
                                              fields,
                                              source_,
                                              statistics_));
                    if (!input) {
                        log_file_ << "No input" << std::endl;
                        continue;
                    }
                } catch (pgen::DataNotFound &e) {
                    log_file_ << "==== Error retrieving data" << std::endl;
                    log_file_ << "==== Exception  : " << e.what() << std::endl;
                    log_file_ << "==== Requirement: " << requirement_ << std::endl;
                    statistics_.fieldsNotFoundCount_++;
                    statistics_.failedRequirementsCount_++;
                }
            }

            {
                util::ScopedTiming prepare_timing(maestroStatistics_.workerProcessJobPrepareTimer_, maestroStatistics_.workerProcessJobPrepareTiming_);
                std::stringstream ss;
                ss << std::this_thread::get_id();
                std::string thread_id = ss.str();

                std::string path = handler.path(*namer_, requirement_, directory_, thread_id);
                pgen::Output o{new eckit::FileHandle{path}, true /* append */};
                mir::output::MIROutput &output = o.output(handler, statistics_);

                try {
                    std::unique_ptr<mir::api::MIRJob> mj(new mir::api::MIRJob());
                    if (!handler.prepare(*mj, requirement_, *input, output, statistics_)) {
                        log_file_ << "Handler did not prepare" << std::endl;
                        continue;
                    }
                    mj->set("style", style_);
                    mj->set("legendre-loader", legendre_loader_);
                    mj->set("matrix-loader", matrix_loader_);
                    mj->set("point-search-trees", point_search_);

                    log_file_ << "Job before add ===> " << *mj <<  std::endl;
                    job.add(mj.release(),
                            *input,
                            output,
                            nullptr);
                    log_file_ << "Job after add ===> " <<  std::endl;
                } catch (std::exception &e) {
                    log_file_ << "==== Error building job" << std::endl;
                    log_file_ << "==== Exception  : " << e.what() << std::endl;
                    log_file_ << "==== Requirement: " << requirement_ << std::endl;
                    statistics_.failedRequirementsCount_++;
                }
                log_file_ << "Execute job" << std::endl;
                util::ScopedTiming timing(statistics_.timer(), statistics_.mirTiming_);
                util::ScopedTiming mirTiming(maestroStatistics_.mirTimer_, maestroStatistics_.mirTiming_);
                job.execute(statistics_.mirStatistics_);
            }
        }
    }

    log_file_ << "(timing) [start_] = " << start_ << std::endl;
    statistics_.totalTiming_ = eckit::Timing(statistics_.timer()) - start_;
    log_file_ << "(timing) [total]  = " << statistics_.totalTiming_ << std::endl;
    statistics_.report(log_file_);
    log_file_ << "*** Worker is leaving" << std::endl;
}

void execute_worker(const eckit::option::CmdArgs& args, eckit::Queue<pgen::Requirement>& req_queue) {
    MaestroWorker worker{std::cref(args), req_queue};
    worker.process();
}

std::string MaestroWorker::get_log_name() {
    std::stringstream ss;
    ss << std::this_thread::get_id();
    return "worker_" + ss.str() + ".log";
}

}  // namespace multio
