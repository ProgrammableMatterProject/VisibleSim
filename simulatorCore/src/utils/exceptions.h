#ifndef EXCEPTIONS_H__
#define EXCEPTIONS_H__

#include <string>

#include "utils.h"
#include "color.h"
#include "math/cell3DPosition.h"

namespace BaseSimulator {

class VisibleSimException : public std::exception {
protected:
    std::string m_msg;
public:
    VisibleSimException()
        : VisibleSimException(std::string("An unknown simulator exception has occured\n")) {}
    VisibleSimException(const std::string &msg)
        : VisibleSimException(msg, std::string("VisibleSim")) {}
    VisibleSimException(const std::string &msg, const std::string &module) {
        stringstream ss;
        ss << TermColor::BRed << "error (" << module << "): " << TermColor::Reset;
        ss << msg;
        m_msg = ss.str();
    }

    virtual const char* what() const throw() override {
        return (m_msg.c_str());
        // + std::string("\n") + std::string(utils::Backtrace(6))).c_str();
    }
};

//<! @brief Exception thrown if an error as occured during parsing
class ParsingException : public VisibleSimException {
public:
    ParsingException() : VisibleSimException(std::string("An unknown error occured during configuration file parsing\n."), std::string("config")) {}
    ParsingException(const std::string &reason)
        : VisibleSimException(reason, std::string("config")) {}
};

//<! @brief Exception thrown if an error as occured during command line parsing
class CLIParsingError : public VisibleSimException {
public:
    CLIParsingError() : VisibleSimException(std::string("An unknown error occured during command line argument parsing\n.")) {}
    CLIParsingError(const std::string &reason)
        : VisibleSimException(reason, std::string("CLI")) {}
};

//!< An exception for marking functions as not implemented
class NotImplementedException : public VisibleSimException {
public:
    NotImplementedException()
        : VisibleSimException(std::string("Feature not yet implemented.")) {}
    NotImplementedException(const std::string &featureName)
        : VisibleSimException(std::string("Feature not yet implemented: " + featureName)) {}
};

//!< An exception for notifying invalid uses of functions
class InvalidArgumentException : public VisibleSimException {
public:
    InvalidArgumentException()
        : VisibleSimException(std::string("Invalid argument supplied to function")) {}
    InvalidArgumentException(const std::string &function_name)
        : VisibleSimException(std::string("Invalid argument supplied to function: ")
                              + function_name) {}
    InvalidArgumentException(const std::string &function_name, const std::string &arg_name)
        : VisibleSimException(std::string("Invalid argument supplied to function: ")
                              + function_name + std::string(" -- arg: ") + arg_name) {}
};

class OutOfLatticeInsertionException : public VisibleSimException {
public:
    OutOfLatticeInsertionException(const Cell3DPosition& p)
        : VisibleSimException(std::string("Module insertion out of the grid at ")
                              + TermColor::BWhite + p.to_string() + TermColor::Reset,
                              std::string("Lattice")) {}
};

class DoubleInsertionException : public VisibleSimException {
public:
    DoubleInsertionException(const Cell3DPosition& p)
        : VisibleSimException(std::string("Module insertion on a on non-empty cell at ")
                              + TermColor::BWhite + p.to_string() + TermColor::Reset,
                              std::string("Lattice")) {}
};

class InvalidDimensionsException : public VisibleSimException {
public:
    InvalidDimensionsException(const Cell3DPosition& size)
        : VisibleSimException(
            std::string("Lattice size in any direction cannot be negative or null: ")
            + TermColor::BWhite + size.to_string() + TermColor::Reset,
            std::string("Lattice")) {
    }
};

} // namespace BaseSimulator

#endif // EXCEPTIONS_H__
