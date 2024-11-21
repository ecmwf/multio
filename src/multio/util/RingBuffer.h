/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Razvan Aguridan

/// @date Jul 2023

#pragma once

#include <atomic>
#include <optional>
#include <vector>

namespace multio::util {

template <class T>
class RingBuffer {
public:
    RingBuffer(uint32_t numEntries) :
        data_(numEntries, 0), prodHead_(0), prodTail_(0), consHead_(0), consTail_(0), entries_(numEntries) {}
    ~RingBuffer() = default;

    void push(T item) {
        bool updated = false;
        uint32_t prodNext = 0;
        uint32_t prodHead = prodHead_.load(std::memory_order::memory_order_acquire);

        do {
            const uint32_t consTail = consTail_.load(std::memory_order::memory_order_acquire);

            prodNext = prodHead + 1;
            if ((prodNext - consTail) > entries_) {
                prodHead = prodHead_.load(std::memory_order::memory_order_acquire);
                continue;
            }

            updated = prodHead_.compare_exchange_strong(prodHead, prodNext, std::memory_order::memory_order_acq_rel,
                                                        std::memory_order_acquire);
        } while (!updated);

        data_[prodHead % entries_] = item;

        updated = false;
        while (!updated) {
            // WARNING: do not remove the next line.
            // It might look like a noop but the compare_exchange_strong function
            // overwrites the expected value in case it is not equal to the actual value.
            auto temp = prodHead;
            updated = prodTail_.compare_exchange_strong(temp, prodNext, std::memory_order::memory_order_acq_rel);
        }
    }

    std::optional<T> pop() {
        bool updated = false;
        uint32_t consNext = 0;
        uint32_t consHead = consHead_.load(std::memory_order::memory_order_acquire);

        do {
            const uint32_t prodTail = prodTail_.load(std::memory_order::memory_order_acquire);

            consNext = consHead_ + 1;
            if ((prodTail - consHead) <= 0) {
                return std::optional<T>();
            }

            updated = consHead_.compare_exchange_strong(consHead, consNext, std::memory_order::memory_order_acq_rel,
                                                        std::memory_order_acquire);
        } while (!updated);

        auto const data = data_[consHead % entries_];

        updated = false;
        while (!updated) {
            // WARNING: do not remove the next line.
            // It might look like a noop but the compare_exchange_strong function
            // overwrites the expected value in case it is not equal to the actual value.
            auto temp = consHead;
            updated = consTail_.compare_exchange_strong(temp, consNext, std::memory_order::memory_order_acq_rel);
        }

        return data;
    }

private:
    RingBuffer(RingBuffer const&) = delete;
    RingBuffer& operator=(RingBuffer const&) = delete;

    RingBuffer(RingBuffer const&&) = delete;
    RingBuffer& operator=(RingBuffer const&&) = delete;

    std::vector<T> data_;
    std::atomic_uint32_t prodHead_;
    std::atomic_uint32_t prodTail_;
    std::atomic_uint32_t consHead_;
    std::atomic_uint32_t consTail_;
    const uint32_t entries_;
};

}  // namespace multio::util