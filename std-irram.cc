
#include <iRRAM/lib.h>

namespace cclerical {

iRRAM::REAL real(iRRAM::INTEGER n) { return n; }
iRRAM::REAL abs(iRRAM::REAL x) { return iRRAM::abs(x); }
bool bounded(iRRAM::REAL x, iRRAM::INTEGER n) { return (bool)bound(x, int(-n)); }

}
