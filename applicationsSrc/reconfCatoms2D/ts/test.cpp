#include <iostream>
#include <string>
#include <typeinfo>
#include "localTupleSpace.hpp"
#include "tuple.hpp"
#include "field.hpp"
#include "namedTuple.hpp"
#include "contextTuple.hpp"
#include <assert.h>
#include "type.hpp"

using namespace std;

// matching: see http://stackoverflow.com/questions/18063451/get-index-of-a-tuple-elements-type

class Test {
public:
  int a;
  Test(int b) { a = b; }
  Test(const Test &t) {  cout << "ici(const &t)" << endl;a = t.a;} 
  Test(Test &t) { cout << "ici(&t)" << endl; a = t.a;}
  Test() {cout << "ici()" << endl; a=42;}
  
  bool operator ==(const Test &t) const {
    return a == t.a;
  }    
  
  friend std::ostream& operator<<(std::ostream& os, const Test &t) {
    os << t.a;
    return os;
  }
  
};

class TestD : public Test {
public:
  int a;
  TestD(int b) : Test() { a = b; }
  TestD(const TestD &t): Test(t) {a = t.a;}
  TestD(): Test() {a=42;}  

  bool operator ==(const TestD &t) const {
    return a == t.a;
  }
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


  Test test1;
  Test test2;

  if (test1 == test2) { cout << "ok test1 == test2" << endl;}

  Tuple t4(5, Test(),3);
  Tuple t3(TYPE(int),TYPE(Test),3);
  
  t4.print();
  t3.print();

  cout << "matching test" << endl;
  if(t4.match(t3)) {
    cout << "ok: t3 matches t4" << endl;
  } else {
    cout << "error: t3 doesn't match t4" << endl;
  }
  
  if(t3.match(t4)) {
    cout << "ok: t3 matches t4" << endl;
  } else {
    cout << "error: t3 doesn't match t4" << endl;
  }
  //Tuple tupleTemplate (TYPE_STRING, TYPE_INT, TYPE_INT);
  //Tuple t3(typeid(int));
  //cout << t1.get<string>(3) << endl;
  //t1.set(3,string("debut"));
  //cout << t1.get<string>(3) << endl;

  LocalTupleSpace tuples;

  tuples.out(new Tuple(string("aaa"), 5, 12.5));

  Tuple query(string("aaa"), TYPE(int), 12.5);
  
  Tuple *res = tuples.inp(query);
  assert(res != NULL);
  cout << *res << endl;

  res = tuples.inp(query);
  assert(res == NULL);

  ContextTuple ctuple(Coordinate(2,5), string("test"));
  cout << ctuple << endl;
}
