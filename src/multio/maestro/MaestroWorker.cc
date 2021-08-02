
#include "MaestroWorker.h"

#include "eckit/io/FileHandle.h"

#include "mir/api/MIRComplexJob.h"
#include "mir/api/MIRJob.h"
#include "mir/input/MIRInput.h"

#include "pgen/handler/Handler.h"
#include "pgen/prodgen/Fields.h"
#include "pgen/prodgen/Output.h"


namespace multio {

MaestroWorker::MaestroWorker(const eckit::option::CmdArgs& args, eckit::Queue<pgen::Requirement>& queue) :
    args_{args},
    source_{args},
    namer_{pgen::FileNamerFactory::build(args)},
    queue_{queue},
    requirement_{}
    {}

void MaestroWorker::process() {
    eckit::Log::info() << "*** Hi from worker" << std::endl;
    std::string lastInputTag;
    mir::api::MIRComplexJob job;
    pgen::Fields fields;
    std::unique_ptr<mir::input::MIRInput> input;

    while (queue_.pop(requirement_) > -1) {
        const pgen::Handler &handler = pgen::Handler::lookup(args_, requirement_.handlers());
        const std::string inputTag = handler.inputTag(requirement_);
        eckit::Log::info() << "INPUT TAG [" << inputTag << "]" << std::endl;
        if (job.empty() || (lastInputTag != inputTag)) {
            eckit::Log::info() << "SET INPUT TAG [" << inputTag << "]" << std::endl;
            lastInputTag = inputTag;
            job.clear();
            fields.clear();
            try {
                eckit::Log::info() << "Handle input from " << source_ << std::endl;
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

        std::string path = handler.path(*namer_, requirement_, "." /* dir */, "id");
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

            eckit::Log::info() << "Job before add ===> " << *mj <<  std::endl;
            job.add(mj.release(),
                    *input,
                    output,
                    nullptr);
            eckit::Log::info() << "Job after add ===> " <<  std::endl;
        } catch (std::exception &e) {
            eckit::Log::info() << "==== Error building job" << std::endl;
            eckit::Log::info() << "==== Exception  : " << e.what() << std::endl;
            eckit::Log::info() << "==== Requirement: " << requirement_ << std::endl;
            statistics_.failedRequirementsCount_++;
        }
        eckit::Log::info() << "Execute job" << std::endl;
        eckit::AutoTiming timing(statistics_.timer_, statistics_.mirTiming_);
        job.execute(statistics_.mirStatistics_);
    }

    eckit::Log::info() << "*** Worker is leaving" << std::endl;
}

}  // namespace multio
