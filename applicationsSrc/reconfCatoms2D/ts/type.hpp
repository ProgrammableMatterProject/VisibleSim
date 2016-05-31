#ifndef TYPE_H_
#define TYPE_H_

#include <typeinfo>
#include <string>

#define TYPE(x) Type(typeid(x))
#define IS_FORMAL(x) ((x).getType() == typeid(Type))

class Type {
 protected:
  const std::type_info& typeInfo;
 public:
  Type(const std::type_info& t);

  bool equal(const Type& t) const;
  bool operator ==(const Type &t) const;
  bool operator !=(const Type &t) const;
  friend std::ostream& operator<<(std::ostream& os, const Type &t);

  std::string name() const;
};

#endif
