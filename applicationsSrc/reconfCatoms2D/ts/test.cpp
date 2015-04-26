#include <iostream>
#include "localTupleSpace.hpp"
#include "field.hpp"
#include <string>
#include <vector>

#include <tuple>

using namespace std;

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


class any
{
private:
  //const std::type_info& type;

  struct base {
    virtual ~base() {}
    virtual base* clone() const = 0;
    virtual const std::type_info& getType() = 0;
  };
  template <typename T>
  struct data: base {
    data(T const& value): value_(value) {}
    data<T>* clone() const { return new data<T>(*this); }
    // base* clone() const
    T value_;
    const std::type_info& getType() {return typeid(value_);}
  };
  base* ptr_;
public:

  template <typename T> any(T const& value): ptr_(new data<T>(value)) {}
  any(any const& other): ptr_(other.ptr_->clone()) {}
  any& operator= (any const& other) {
    any(other).swap(*this);
    return *this;
  }
  ~any() { delete this->ptr_; }
  void swap(any& other) { std::swap(ptr_, other.ptr_); }

  template <typename T>
  T& get() {
   return dynamic_cast<data<T>&>(*this->ptr_).value_;
  }

  const std::type_info& getType() { return ptr_->getType(); }
      //typeid((*this->ptr_).value_) };
};

class myTestTuple{
  vector<any> fields;
public:

  //template<typename T, typename ... Types>
  //myTestTuple(T f, Types ... rest) {
  template<typename ... Types>
  myTestTuple(Types ... fs) {
    fields =  {fs...};
    //std::vector<any> vec = {fs...};
  //For (unsigned int i = 0; i < vec.size(); i++) {

  //}
    //fields = vec;
    //fields = {fs...}; 
  }

  ~myTestTuple() {};

  const std::type_info& getElemType(int i) {
    return fields[i].getType();
  }

  void print() {
    cout << "Tuple: ";
    for (unsigned i = 0; i < fields.size(); i++) {
      cout << getElemType(i).name() << " ";
      cout << " ";
    }
    cout << endl;
  }

  template <typename T>
  T& getElem(int i) {
    return fields[i].get<T>();
  }
};

template<typename ... Types>
class myTupleTest2 {
public:
  tuple<Types...> t;

  myTupleTest2(Types...v) {
    t = make_tuple(v...);
  }

  myTupleTest2(const myTupleTest2 &t1) {
    t = t1.t;
  }

  ~myTupleTest2() {

  }

};


int main(void) {
  cout << "TS implementation test" << endl;
  myTupleTest2<int> t2(5);

  auto t = std::make_tuple(1, "Foo", 3.14);
  std::cout << "(" << std::get<0>(t) << ", " << std::get<1>(t)
  	    << ", " << std::get<2>(t) << ")\n";

}

#if 0
  LocalTupleSpace ts;
  
  Test test;

  myTestTuple t1(4,18,12.5,'a',test);
  myTestTuple t2 = t1;
  t1.print();
  t2.print();

  cout << "2nd: " << t1.getElem<double>(2) << endl;
}
#endif



#if 0
  //Tuple test(4,5);
  //Object &o = test;
  Field<string> f1(string("test")) ;
  cout << f1.get() << endl;

  Field<int> f2(4) ;
  cout << f2.get() << endl;

  Field<Test> f3(Test(3));
  Field<Test> f5(Test(5));
  cout << f3.get().a << endl;
  //vector<Field> v;

  Test t1(4);
  Test t2(10);

  const std::type_info& ti1 = typeid(t1);
  
  cout << "Test t1 type name: " << typeid(t1).name() << ", hashcode: " << typeid(t1).hash_code() << endl;
  cout << "Test t2 type name: " <<  typeid(t2).name() << ", hashcode: " << typeid(t2).hash_code() << endl;
  cout << "Test Test type name: " << typeid(Test).name() <<  ", hashcode: " << typeid(Test).hash_code() << endl;

  cout << "Test typeid type: " << typeid(ti1).name() <<  ", hashcode: " << typeid(ti1).hash_code() << endl;

  cout << "Test typeid type: " << typeid(typeid(Test)).name() <<  ", hashcode: " << typeid(typeid(Test)).hash_code() << endl;

  cout << "Test typeid type: " << typeid(typeid(int)).name() <<  ", hashcode: " << typeid(typeid(int)).hash_code() << endl;
  cout << "Test typeid type: " << typeid(typeid(double*)).name() <<  ", hashcode: " << typeid(typeid(double*)).hash_code() << endl;

  any a0(17);
  any a1(3.14);
  cout << "a0 " << a0.getType().name() << endl;
  cout << "a1 " << a1.getType().name() << endl;
  try { a0.get<double>(); } catch (...) { cout << "exception" << endl;}
  a0 = a1;
  std::cout << a0.get<double>() << "\n";



#endif
