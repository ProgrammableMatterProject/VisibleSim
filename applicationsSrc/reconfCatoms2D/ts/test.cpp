#include <iostream>
#include "localTupleSpace.hpp"
#include "field.hpp"
#include <string>

using namespace std;

int main(void) {
  cout << "TS implementation test" << endl;
  LocalTupleSpace ts;

  //Tuple test(4,5);
  //Object &o = test;
  Field<string> f1(string("test")) ;
  cout << f1.get() << endl;

  Field<int> f2(4) ;
  cout << f2.get() << endl;

  //vector<Field> v;
}
