#ifndef FIELD_H_
#define FIELD_H_

#include <typeinfo>
#include <utility>
#include <algorithm>
#include "type.hpp"
#include <iostream>

// inspired from:
// http://stackoverflow.com/questions/13461869/c-push-multiple-types-onto-vector

class Field
{
private:
  struct base {
    virtual ~base() {}
    virtual base* clone() const = 0;
    virtual const std::type_info& getType() const = 0;
    virtual bool equal(base* b) const = 0;
    virtual bool match(base *b) const = 0; 
    
    virtual std::ostream& output(std::ostream& os) const = 0;
  };

  template <typename T>
  struct data: base {
    data(T const& value): value_(value) {}
    data<T>* clone() const { return new data<T>(*this); }
    
    T value_;

    const std::type_info& getType() const {return typeid(value_);}

    bool equal (base *b) const { 
      if (getType() != b->getType() ) {return false;}
      return dynamic_cast<data<T>&>(*b).value_ == value_;
    }

    bool match (base *b) const {
      if (IS_FORMAL(*this)) {
	const Type &t1 = (const Type&) value_;
	const Type &t2 = Type(b->getType());
        return t1 == t2;
      }
      if (IS_FORMAL(*b)) {
	const Type &t1 = dynamic_cast<data<Type>&>(*b).value_;
	const Type t2 = Type(getType());
	return t1 == t2;
      }
      return equal(b);
    }
    
    std::ostream& output(std::ostream& os) const{
      os << getType().name() << "=" << value_;
      return os;
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
  T& get() const {return dynamic_cast<data<T>&>(*this->ptr_).value_;}
  const std::type_info& getType() const { return ptr_->getType();}
  
  bool equal(const Field &rhs) const {return this->ptr_->equal(rhs.ptr_);}
  bool match(const Field &rhs) const {return this->ptr_->match(rhs.ptr_);}

  friend std::ostream& operator<<(std::ostream& os, const Field &f) { 
    return f.ptr_->output(os);
  }
};

inline bool operator==(const Field& lhs, const Field& rhs) { return lhs.equal(rhs);}
    
#endif
