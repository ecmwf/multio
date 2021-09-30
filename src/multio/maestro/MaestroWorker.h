#ifndef multio_MaestroWorker_H
#define multio_MaestroWorker_H

#include <ostream>
#include "eckit/container/Queue.h"
#include "eckit/option/CmdArgs.h"

#include "multio/maestro/MaestroSource.h"
#include "multio/maestro/MaestroStatistics.h"

#include "pgen/prodgen/FileNamer.h"
#include "pgen/prodgen/Requirement.h"
#include "pgen/prodgen/ProdGenStatistics.h"

namespace multio {

class MaestroWorker {
public:
    MaestroWorker(const eckit::option::CmdArgs& args, eckit::Queue<pgen::Requirement>& queue);
    ~MaestroWorker();
    void process();
private:
    const eckit::option::CmdArgs& args_;
    MaestroSource source_;
    eckit::Queue<pgen::Requirement>& queue_;
    pgen::Requirement requirement_;
    pgen::ProdGenStatistics statistics_;
    std::unique_ptr<pgen::FileNamer> namer_;
    std::string directory_;
    std::string style_;
    std::string legendre_loader_;
    std::string matrix_loader_;
    std::string point_search_;
    MaestroStatistics maestroStatistics_;
};

void execute_worker(const eckit::option::CmdArgs&, eckit::Queue<pgen::Requirement>&);

}  // namespace multio

#endif  // multio_MaestroWorker_H
