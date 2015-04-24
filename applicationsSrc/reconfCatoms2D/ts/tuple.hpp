#ifndef TUPLE_H_
#define TUPLE_H_

class Tuple {
private:
  int key;
  int data;
public:
  Tuple(int k, int d);
  Tuple(const Tuple& t);
  ~Tuple();

  int getKey();
  int getData();
};

#endif
