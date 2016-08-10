#include <sstream>
#include <vector>

using namespace std;

class Obj_File
{

public:
    Obj_File(string _filename) : filename(_filename) { read_obj(); };

private:
    string filename;
    void read_obj();
    void read_v_line(string);
    void read_f_line(string);

};
