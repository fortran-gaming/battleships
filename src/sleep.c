extern int c_sleep(int*);

#ifdef _MSC_VER
// https://docs.microsoft.com/en-us/windows/win32/api/synchapi/nf-synchapi-sleep
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
// https://linux.die.net/man/3/usleep
// https://linux.die.net/man/2/nanosleep
#include <time.h>
#endif

int c_sleep(int* milliseconds){

#ifdef _MSC_VER
  Sleep(*milliseconds);
  return 0;
#else
  struct timespec t;

  t.tv_sec = *milliseconds / 1000;
  t.tv_nsec = (*milliseconds % 1000) * 1000000;

  return nanosleep(&t, NULL);
#endif

}
