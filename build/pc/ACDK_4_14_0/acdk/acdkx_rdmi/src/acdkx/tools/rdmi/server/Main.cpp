

#include <acdk.h>
#include <acdk/lang/System.h>

namespace acdkx {
namespace tools {
namespace rdmi {
namespace server {

class Main 
: extends acdk::lang::Object
{
public:
  static int acdkMain(RStringArray args)
  {

    return 0;
  }
};
}
}
}
}


int
main(int argc, char* argv[], char** envptr)
{
  return acdk::lang::System::main(acdkx::tools::rdmi::server::Main::acdkMain, argc, argv, envptr);
}

