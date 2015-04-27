#include <iostream>
#include <string>
#include <typeinfo>
#include "localTupleSpace.hpp"
#include "tuple.hpp"
#include "field.hpp"
#include "namedTuple.hpp"
#include <assert.h>
#include "types.hpp"

using namespace std;

// matching: see http://stackoverflow.com/questions/18063451/get-index-of-a-tuple-elements-type

class Test {
public:
  int a;
  Test(int b) { a = b; }
  Test(const Test &t) {  cout << "ici(const &t)" << endl;a = t.a;} 
  Test(Test &t) { cout << "ici(&t)" << endl; a = t.a;}
  Test() {cout << "ici()" << endl; a=42;}
};

class TestD : public Test {
public:
  int a;
  TestD(int b) : Test() { a = b; }
  TestD(const TestD &t): Test(t) {a = t.a;}
  TestD(): Test() {a=42;}
};

int main(void) {
  cout << "TS implementation test" << endl;
  Tuple t1(string("test"), 1, 3);
  Tuple t2 = t1;
  assert(t1 == t2);
  if (t1 == t2) { cout << "t1 == t2" << endl; }
  
  t1.add(string("fin"));
  assert(t1 != t2);
  if (t1 != t2) { cout << "t1 != t2" << endl; }
  t1.erase(3);
  assert(t1 == t2);
  if (t1 == t2) { cout << "t1 == t2" << endl; }
  t1.print();
  t2.print();

  //Tuple tupleTemplate (TYPE_STRING, TYPE_INT, TYPE_INT);
  //Tuple t3(typeid(int));
  //cout << t1.get<string>(3) << endl;
  //t1.set(3,string("debut"));
  //cout << t1.get<string>(3) << endl;
}
