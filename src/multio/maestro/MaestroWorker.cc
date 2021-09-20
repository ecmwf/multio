
#include "MaestroWorker.h"

#include "eckit/io/FileHandle.h"

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
    source_{args},
    namer_{pgen::FileNamerFactory::build(args)},
    queue_{queue},
    requirement_{}
    {}

MaestroWorker::~MaestroWorker() {
    maestroStatistics_.report(eckit::Log::info());
}

void MaestroWorker::process() {
    eckit::AutoTiming timing(maestroStatistics_.timer_, maestroStatistics_.workerProcessTiming_);
    LOG_DEBUG_LIB(LibMultio) << "*** Hi from worker" << std::endl;
    std::string lastInputTag;
    mir::api::MIRComplexJob job;
    pgen::Fields fields;
    std::unique_ptr<mir::input::MIRInput> input;

    while (queue_.pop(requirement_) > -1) {
        const pgen::Handler &handler = pgen::Handler::lookup(args_, requirement_.handlers());
        const std::string inputTag = handler.inputTag(requirement_);
        LOG_DEBUG_LIB(LibMultio) << "INPUT TAG [" << inputTag << "]" << std::endl;
        if (job.empty() || (lastInputTag != inputTag)) {
            LOG_DEBUG_LIB(LibMultio) << "SET INPUT TAG [" << inputTag << "]" << std::endl;
            lastInputTag = inputTag;
            job.clear();
            fields.clear();
            try {
                LOG_DEBUG_LIB(LibMultio) << "Handle input from " << source_ << std::endl;
                input.reset(handler.input(requirement_,
                                          fields,
                                          source_,
                                          statistics_));
                if (!input) {
                    eckit::Log::info() << "No input" << std::endl;
                    continue;
                }
            } catch (pgen::DataNotFound &e) {
                eckit::Log::info() << "==== Error retrieving data" << std::endl;
                eckit::Log::info() << "==== Exception  : " << e.what() << std::endl;
                eckit::Log::info() << "==== Requirement: " << requirement_ << std::endl;
                statistics_.fieldsNotFoundCount_++;
                statistics_.failedRequirementsCount_++;
            }
        }

        std::stringstream ss;
        ss << std::this_thread::get_id();
        std::string thread_id = ss.str();

        std::string path = handler.path(*namer_, requirement_, "." /* dir */, thread_id);
        pgen::Output o{new eckit::FileHandle{path}, true /* append */};
        mir::output::MIROutput &output = o.output(handler, statistics_);

        try {
            std::unique_ptr<mir::api::MIRJob> mj(new mir::api::MIRJob());
            if (!handler.prepare(*mj, requirement_, *input, output, statistics_)) {
                eckit::Log::info() << "Handler did not prepare" << std::endl;
                continue;
            }
            mj->set("style", "dissemination"); //required
            mj->set("legendre-loader", "file-io"); //optimization
            mj->set("matrix-loader", "file-io"); //optimization
            mj->set("point-search-trees", "mapped-anonymous-memory"); //can be commented out - optimization

            LOG_DEBUG_LIB(LibMultio) << "Job before add ===> " << *mj <<  std::endl;
            job.add(mj.release(),
                    *input,
                    output,
                    nullptr);
            LOG_DEBUG_LIB(LibMultio) << "Job after add ===> " <<  std::endl;
        } catch (std::exception &e) {
            eckit::Log::info() << "==== Error building job" << std::endl;
            eckit::Log::info() << "==== Exception  : " << e.what() << std::endl;
            eckit::Log::info() << "==== Requirement: " << requirement_ << std::endl;
            statistics_.failedRequirementsCount_++;
        }
        LOG_DEBUG_LIB(LibMultio) << "Execute job" << std::endl;
        eckit::AutoTiming timing(statistics_.timer_, statistics_.mirTiming_);
        job.execute(statistics_.mirStatistics_);
    }

    statistics_.report(eckit::Log::info());
    eckit::Log::info() << "*** Worker is leaving" << std::endl;
}

void execute_worker(const eckit::option::CmdArgs& args, eckit::Queue<pgen::Requirement>& req_queue) {
    MaestroWorker worker{std::cref(args), req_queue};
    worker.process();
}

}  // namespace multio
