
#ifndef EXCEPTIONS_H__
#define EXCEPTIONS_H__

#include <string>

namespace BaseSimulator {

class VisibleSimException : public std::exception {
protected:
    std::string m_msg;
public:
    VisibleSimException()
        : m_msg(std::string("An unknown simulator exception has occured\n")) {}
    VisibleSimException(const std::string &msg)
        : m_msg(msg) {}
    
    virtual const char* what() const throw() {
        return m_msg.c_str();
    }
};


//!< An exception for marking functions as not implemented
class NotImplementedException : public VisibleSimException {
public:
    NotImplementedException()
        : VisibleSimException(std::string("Feature not yet implemented.")) {}
    NotImplementedException(const std::string &featureName)
        : VisibleSimException(std::string("Feature not yet implemented: ") + featureName) {}        
};

} // namespace BaseSimulator

#endif // EXCEPTIONS_H__
