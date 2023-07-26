#include "Tracer.h"

#include <chrono>
#include <sstream>

#include <fcntl.h>
#include <unistd.h>

#include "eckit/mpi/Comm.h"
namespace {
const uint32_t CHUNK_MASK = 0xFF000000;
const uint32_t CHUNK_SHIFT = 24;
const uint32_t INDEX_MASK = 0x00FFFFFF;
}  // namespace

namespace multio::util {

Tracer::Tracer(uint32_t numChunks, uint32_t chunkSize, const std::string& outputPath) :
    chunkSize_(chunkSize * 2),
    traceChunks_(numChunks, nullptr),
    currentChunkAndIndex_(0),
    availableQueue_(numChunks),
    writeQueue_(numChunks),
    outputFile_(outputPath),
    running_(true),
    traceWriterThread_() {
    for (auto i = 0; i < numChunks; ++i) {
        traceChunks_[i] = new uint64_t[chunkSize_];
        std::memset(traceChunks_[i], 0, chunkSize_ * sizeof(uint64_t));
        if (i > 0) {
            availableQueue_.push(i);
        }
    }
}

Tracer::~Tracer() {
    uint32_t chunkAndIndex = currentChunkAndIndex_.load(std::memory_order::memory_order_acquire);
    uint32_t chunk = (chunkAndIndex & CHUNK_MASK) >> CHUNK_SHIFT;

    writeQueue_.push(chunk);

    // Removing this will cause the last chunk to be ocassionally lost.
    // (race condition between write queue and running flag)
    std::this_thread::sleep_for(std::chrono::milliseconds(100));

    running_ = false;

    if (traceWriterThread_.joinable()) {
        traceWriterThread_.join();
    }
}

void Tracer::startWriterThread() {
    traceWriterThread_ = std::thread([this]() { writerThread_(); });
}

void Tracer::recordEvent(uint64_t event) {
    const uint64_t timestamp = std::chrono::high_resolution_clock::now().time_since_epoch().count();

    bool updated = false;
    uint32_t chunk = 0;
    uint32_t index = 0;
    uint32_t chunkAndIndex = currentChunkAndIndex_.load(std::memory_order::memory_order_acquire);

    do {
        chunk = (chunkAndIndex & CHUNK_MASK) >> CHUNK_SHIFT;
        index = chunkAndIndex & INDEX_MASK;

        // check if we still have place in the current chunk to log a new event
        const auto nextSize = index + 2;
        const auto switchToNextChunk = nextSize > chunkSize_;
        const auto indexNext = switchToNextChunk ? 2 : nextSize;

        auto chunkNext = chunk;
        if (switchToNextChunk) {
            // we don't have place in the current chunk to log a new event,
            // get the next available chunk from the available queue
            auto availableChunkId = availableQueue_.pop();
            while (!availableChunkId) {
                availableChunkId = availableQueue_.pop();
            }

            chunkNext = availableChunkId.value();
        }

        const auto chunkAndIndexNext = (chunkNext << CHUNK_SHIFT) | indexNext;
        updated = currentChunkAndIndex_.compare_exchange_strong(
            chunkAndIndex, chunkAndIndexNext, std::memory_order::memory_order_acq_rel, std::memory_order_acquire);

        if (switchToNextChunk) {
            // we wanted to change the chunk
            if (updated) {
                // we changed the chunk, push the completed chunk to the write queue
                writeQueue_.push(chunk);
                chunk = chunkNext;
                index = 0;
            }
            else {
                // some other thread updated the chunk counters, most likely also changing the chunk,
                // so we put the available chunk we retrieved back in the available queue
                availableQueue_.push(chunkNext);
            }
        }
    } while (!updated);

    traceChunks_[chunk][index] = event;
    traceChunks_[chunk][index + 1] = timestamp;
}

void Tracer::writerThread_() {
    const auto myRank = eckit::mpi::comm().rank();

    std::ostringstream oss;
    oss << outputFile_ << "_" << myRank;

    const auto traceFileHandle = open(oss.str().c_str(), O_CREAT | O_TRUNC | O_WRONLY, S_IWUSR | S_IRUSR);

    while (running_.load(std::memory_order_acquire)) {
        const auto finishedChunkId = writeQueue_.pop();
        if (finishedChunkId) {
            const auto id = finishedChunkId.value();

            const auto numBytesToWrite = chunkSize_ * sizeof(uint64_t);
            const auto bytes = reinterpret_cast<const void*>(traceChunks_[id]);

            write(traceFileHandle, bytes, numBytesToWrite);
            fsync(traceFileHandle);

            std::memset(traceChunks_[id], 0, chunkSize_ * sizeof(uint64_t));

            availableQueue_.push(id);
        }
        else {
            std::this_thread::sleep_for(std::chrono::milliseconds(100));
        }
    }

    close(traceFileHandle);
}

}  // namespace multio::util