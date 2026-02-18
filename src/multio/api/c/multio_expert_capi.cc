#include <chrono>
#include <memory>
#include <vector>

#include "multio/server/MultIOProfiler.h"
#include "multio/server/MultIOTransportProgress.h"
#include "multio/server/MultIOReceiver.h"
#include "multio/server/MultIODispatcherRunner.h"
#include "multio/server/MultIOQueue.h"
#include "multio/server/MultIOProfilerState.h"
#include "multio/config/ComponentConfiguration.h"

extern "C" {

struct multio_queue_t {
    std::unique_ptr<multio::server::MultIOQueue> impl;
    std::unique_ptr<multio_failure_context_t> failureContext;
};

struct multio_profiler_state_t {
    std::unique_ptr<multio::server::MultIOProfilerState> impl;
};

struct multio_transport_progress_t {
    std::unique_ptr<multio::server::MultIOTransportProgress> impl;
};

struct multio_receiver_t {
    std::unique_ptr<multio::server::MultIOReceiver> impl;
};

struct multio_dispatcher_t {
    std::unique_ptr<multio::server::MultIODispatcherRunner> impl;
};

struct multio_profiler_t {
    std::unique_ptr<multio::server::MultIOProfiler> impl;
};


// API functions


// Profiler State
int multio_new_profiler_state(multio_profiler_state_t** ps, int numDispatchers) {
    return wrapApiFunction([=]() {
        *ps = new multio_profiler_state_t{
            std::make_unique<multio::server::MultIOProfilerState>(numDispatchers)};
    });
}

int multio_delete_profiler_state(multio_profiler_state_t* ps) {
    return wrapApiFunction([=]() { delete ps; });
}


// Queue
int multio_new_queue(multio_queue_t** q,
                     multio_profiler_state_t* ps,
                     int capacity) {
    return wrapApiFunction([=]() {
        *q = new multio_queue_t{
            std::make_unique<multio::server::MultIOQueue>(
                capacity, ps->impl->queue())};
    });
}

int multio_delete_queue(multio_queue_t* q) {
    return wrapApiFunction([=]() { delete q; });
}

int multio_queue_close(multio_queue_t* q) {
    return wrapApiFunction([=]() { q->impl->close(); });
}


// Transport progress
int multio_new_transport_progress(multio_transport_progress_t** tp,
                                  multio_configuration_t* cc,
                                  multio_profiler_state_t* ps) {

    return wrapApiFunction([=]() {

        ComponentConfiguration compConf(cc->parsedConfig(), *cc);

        *tp = new multio_transport_progress_t{
            std::make_unique<multio::server::MultIOTransportProgress>(
                compConf,
                ps->impl->transport())};
    }, cc);
}

int multio_transport_progress_run(multio_transport_progress_t* tp) {
    return wrapApiFunction([=]() { tp->impl->run(); });
}

int multio_transport_progress_stop(multio_transport_progress_t* tp) {
    return wrapApiFunction([=]() { tp->impl->stop(); });
}

int multio_delete_transport_progress(multio_transport_progress_t* tp) {
    return wrapApiFunction([=]() { delete tp; });
}


// Receiver
int multio_new_receiver(multio_receiver_t** r,
                        multio_configuration_t* cc,
                        multio_transport_progress_t* tp,
                        multio_queue_t* q,
                        multio_profiler_state_t* ps) {

    return wrapApiFunction([=]() {

        ComponentConfiguration compConf(cc->parsedConfig(), *cc);

        *r = new multio_receiver_t{
            std::make_unique<multio::server::MultIOReceiver>(
                compConf,
                tp->impl->transport(),
                *q->impl,
                ps->impl->receiver())};
    }, cc);
}

int multio_receiver_run(multio_receiver_t* r) {
    return wrapApiFunction([=]() { r->impl->run(); });
}

int multio_delete_receiver(multio_receiver_t* r) {
    return wrapApiFunction([=]() { delete r; });
}



// Dispatcher
int multio_new_dispatcher(multio_dispatcher_t** d,
                          multio_configuration_t* cc,
                          multio_transport_progress_t* tp,
                          multio_queue_t* q,
                          multio_profiler_state_t* ps,
                          int index) {

    return wrapApiFunction([=]() {

        ComponentConfiguration compConf(cc->parsedConfig(), *cc);

        *d = new multio_dispatcher_t{
            std::make_unique<multio::server::MultIODispatcherRunner>(
                compConf,
                *q->impl,
                tp->impl->transport(),
                ps->impl->dispatcher(index))};
    }, cc);
}

int multio_dispatcher_run(multio_dispatcher_t* d) {
    return wrapApiFunction([=]() { d->impl->run(); });
}

int multio_delete_dispatcher(multio_dispatcher_t* d) {
    return wrapApiFunction([=]() { delete d; });
}

// Queue profiler
int multio_new_profiler(multio_profiler_t** qp,
                        multio_profiler_state_t* ps,
                        multio_queue_t* q,
                        int period_ms,
                        int buckets) {

    return wrapApiFunction([=]() {

        *qp = new multio_profiler_t{
            std::make_unique<multio::server::MultIOProfiler>(
                *ps->impl,
                q->impl->capacity(),
                std::chrono::milliseconds(period_ms),
                buckets)};
    });
}

int multio_profiler_run(multio_profiler_t* qp) {
    return wrapApiFunction([=]() { qp->impl->run(); });
}

int multio_profiler_stop(multio_profiler_t* qp) {
    return wrapApiFunction([=]() { qp->impl->stop(); });
}

int multio_profiler_delete(multio_profiler_t* qp) {
    return wrapApiFunction([=]() { delete qp; });
}




}
