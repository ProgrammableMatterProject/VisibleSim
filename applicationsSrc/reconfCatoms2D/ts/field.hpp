#ifndef FIELD_H_
#define FIELD_H_


template <typename T> 
class Field {
protected:
  T value;
public:
  Field(T v) { value = v; }
  Field(const Field &f) {value = f.value;}
  T get() const {return value;}
  void set(const T& v) {value=v;}  
};

//template<class T> const T& Field::get() const
//{ return dynamic_cast<const Field<T>&>(*this).get(); }
//template<class T, class U> void Field::set(const U& v)
//{ return dynamic_cast<Field<T>&>(*this).set(v); }

#endif
