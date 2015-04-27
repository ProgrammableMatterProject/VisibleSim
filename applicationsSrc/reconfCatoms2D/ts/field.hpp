#ifndef FIELD_H_
#define FIELD_H_

#include <typeinfo>
#include <utility>
#include <algorithm>
// inspired from:
// http://stackoverflow.com/questions/13461869/c-push-multiple-types-onto-vector

class Field
{
private:
  struct base {
    virtual ~base() {}
    virtual base* clone() const = 0;
    virtual const std::type_info& getType() = 0;
    virtual bool equal(base* b) = 0;
  };

  template <typename T>
  struct data: base {
    data(T const& value): value_(value) {}
    data<T>* clone() const { return new data<T>(*this); }
    
    T value_;

    const std::type_info& getType() {return typeid(value_);}

    bool equal (base *b) { 
      if (getType() != b->getType() ) {return false;}
      return dynamic_cast<data<T>&>(*b).value_ == value_;
    }
  };
  base* ptr_;

public:

  Field() {ptr_ = NULL;};
  template <typename T> Field(T const& value): ptr_(new data<T>(value)) {}
  Field(Field const& other): ptr_(other.ptr_->clone()) {}
  Field& operator= (Field const& other) {
    Field(other).swap(*this);
    return *this;
  }
  ~Field() { delete this->ptr_; }
  
  void swap(Field& other) { std::swap(ptr_, other.ptr_); }

  template <typename T>
  T& get() {return dynamic_cast<data<T>&>(*this->ptr_).value_;}
  const std::type_info& getType() { return ptr_->getType();}
  
  bool equal(const Field &rhs) const {return this->ptr_->equal(rhs.ptr_);}

};

inline bool operator==(const Field& lhs, const Field& rhs) { return lhs.equal(rhs);}
    
#endif
