#include <stdio.h>
#include <stdint.h>
#include <sys/resource.h>
#include <time.h>

#if __linux__
#include <sys/sysinfo.h>

void om_get_mem_usage(uint64_t *total_memory_of_system, uint64_t *system_usage,
                   uint64_t *task_usage) {
    // Retrieve total memory of the system
    struct sysinfo info;
    sysinfo(&info);
    *total_memory_of_system = (uint64_t)info.totalram * info.mem_unit;

    // Retrieve memory usage of the current process
    struct rusage usage;
    if (getrusage(RUSAGE_SELF, &usage) == 0) {
        *system_usage = *total_memory_of_system - (uint64_t)info.freeram * info.mem_unit;
        *task_usage = (uint64_t)usage.ru_maxrss * 1024;  // Convert to bytes
    } else {
        // Error handling
        *system_usage = 0;
        *task_usage = 0;
    }
}

void om_tic(int64_t *start_ns) {
    struct timespec start_time;
    clock_gettime(CLOCK_MONOTONIC, &start_time);
    *start_ns = start_time.tv_sec * 1000000000L + start_time.tv_nsec;
}

void om_toc(int64_t start_ns, int64_t *elapsed_ns) {
    struct timespec end_time;
    clock_gettime(CLOCK_MONOTONIC, &end_time);
    *elapsed_ns = (end_time.tv_sec * 1000000000L + end_time.tv_nsec) - start_ns;
}



void om_is_little_endian( int8_t* isLittle ){
  unsigned int num = 1;
  unsigned char *c = (unsigned char*)&num;
  if ( c[0] != 0 ) {
    *isLittle = 1;
  } else {
    *isLittle = 0;
  }
  return;
}
#endif


#if __APPLE__
#include <sys/sysctl.h>

void om_get_mem_usage(uint64_t *total_memory_of_system, uint64_t *system_usage,
                   uint64_t *task_usage) {
    // Retrieve total memory of the system
    int mib[2];
    size_t length;
    mib[0] = CTL_HW;
    mib[1] = HW_MEMSIZE;
    length = sizeof(*total_memory_of_system);
    sysctl(mib, 2, total_memory_of_system, &length, NULL, 0);

    // Retrieve memory usage of the current process
    struct rusage usage;
    if (getrusage(RUSAGE_SELF, &usage) == 0) {
        *system_usage = *total_memory_of_system - (uint64_t)usage.ru_maxrss;
        *task_usage = (uint64_t)usage.ru_maxrss * 1024;  // Convert to bytes
    } else {
        // Error handling
        *system_usage = 0;
        *task_usage = 0;
    }
}


void om_tic(int64_t *start_ns) {
    struct timespec start_time;
    clock_gettime(CLOCK_UPTIME_RAW, &start_time); // Use CLOCK_UPTIME_RAW or CLOCK_UPTIME
    *start_ns = start_time.tv_sec * 1000000000L + start_time.tv_nsec;
}


void om_toc(int64_t start_ns, int64_t *elapsed_ns) {
    struct timespec end_time;
    clock_gettime(CLOCK_UPTIME_RAW, &end_time); // Use CLOCK_UPTIME_RAW or CLOCK_UPTIME
    *elapsed_ns = (end_time.tv_sec * 1000000000L + end_time.tv_nsec) - start_ns;
}

void om_is_little_endian( int8_t* isLittle ){
  unsigned int num = 1;
  unsigned char *c = (unsigned char*)&num;
  if ( c[0] != 0 ) {
    *isLittle = 1;
  } else {
    *isLittle = 0;
  }
  return;
}

#endif
