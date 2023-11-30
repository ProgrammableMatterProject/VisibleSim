/*
 * @file blockCode.h
 * @date 22 mars 2013
 * @author dom, bpiranda
 * @brief Defines a single instance of the distributed program that is executed independantly by each module
 */

#ifndef BLOCKCODE_H_
#define BLOCKCODE_H_

#include <cinttypes>
#include <memory>
#include <map>

#include "../utils/trace.h"
#include "../grid/target.h"
#include "../deps/TinyXML/tinyxml.h"
class Event;
typedef std::shared_ptr<Event> EventPtr;

class Message;
class HandleableMessage;
class P2PNetworkInterface;

namespace BaseSimulator {

    class Scheduler;
    class Lattice;
    class BuildingBlock;
    class BlockCode;

    typedef std::function<void(BlockCode *, std::shared_ptr<Message>, P2PNetworkInterface *)> eventFunc;
    typedef std::function<void(std::shared_ptr<Message>, P2PNetworkInterface *)> eventFunc2;

/**
 * @brief A distributed user program, will be executed by each module
 */
    class BlockCode {
    public:
        class InterfaceNotConnectedException : public VisibleSimException {
        public:
            InterfaceNotConnectedException(BlockCode *bc, const Message *msg,
                                           const P2PNetworkInterface *itf);
        };

    private:
        int sendMessageToAllNeighbors(const char *msgString, Message *msg, Time t0, Time dt, int nexcept, va_list args);

    public:
        BuildingBlock *hostBlock;   //!< The block to which this instance of the user program belongs
        Time availabilityDate = 0; //!< If the host is busy, the date at which it will be available
        std::multimap<int, eventFunc2> eventFuncMap2; //!< container of function pointers to message handlers, indexed by message typeID

        Scheduler *scheduler; //!< pointer to the single instance of scheduler of the simulation
        Lattice *lattice;  //!< pointer to the single instance of lattice of the simulation
        ConsoleStream console;  //!< pointer to the single instance of ConsoleStream of the simulation
        static Target *target; //!< pointer shared by all blockCodes to the current target configuration

        Cell3DPosition motionDest; //!< Only used for motion export for animations

/**
 * @brief BlockCode constructor
 * @param host The block on which this instance of the blockCode will be executed
 * @warning DERIVED BLOCKCODE MUST ALWAYS CHECK FOR host == NULL, AND RETURN IF THAT'S THE CASE. THAT'S FOR CLI PARSING THROUGH DUMMY BLOCKCODE, WHICH USES BlockCode with a NULL host.
 */
        BlockCode(BuildingBlock *host);

/**
 * @brief BlockCode destructor
 */
        virtual ~BlockCode();

/**
 * @brief shortcut to directly get the id of the module
 * @return the id of the module
 */
        bID getId() const;

/**
 * @brief shortcut to directly set the color of the module
 * @param c : the color to set
 */
        void setColor(const Color &c);

/**
 * @brief shortcut to directly set the color of the module
 * @param idColor : number of the color
 */
        void setColor(int idColor);

/**
 * @brief shortcut to directly set the color of the module
 * @param v : visibility state of the module
 */
        void setVisible(bool v);

 /**
 * @brief Get the current color of a module
 * @return the current color
 */
        Color getColor() const;

/**
 * @brief Provides the list of connected interfaces to the current module
 *
 */
        map<short,P2PNetworkInterface *> getAllConnectedInterfaces();

/**
 * @brief Provides the user with a pointer to the configuration file parser, which can be used to read additional user information from it. Has to be overriden in the child class.
 * @param config : pointer to the TiXmlDocument representing the configuration file, all information related to VisibleSim's core have already been parsed
 *
 * Called from BuildingBlock constructor, only once.
 */
        virtual void parseUserElements(TiXmlDocument *config) {}

        /**
         * @brief Provides the user with a pointer to the configuration file parser, which can be used to read additional user information from each block config. Has to be overriden in the child class.
         * @param config : pointer to the TiXmlElement representing the block configuration file, all information related to concerned block have already been parsed
         *
         */
        virtual void parseUserBlockElements(TiXmlElement *config) {}

/**
 * User-implemented command line reader that can handle all user-defined
 *  command line arguments not caught by the simulator
 * @note call is made from CommandLine::read (commandLine.h)
 */
        virtual bool parseUserCommandLineArgument(int &argc, char **argv[]) {
            return false; // default, did not parse any argument
        };

/**
 * @brief Handler for all events received by the host block
 */
        virtual void processLocalEvent(EventPtr pev);

/**
 * @brief This function is called on startup of the blockCode,
 it can be used to perform initial configuration of the host or this instance of the program
*/
        virtual void startup() = 0;

/**
 * @brief Initialization function called by startup(), used for scheduling initial events and performing additional configuration if needed.
 */
        virtual void init() {};

//virtual bool getAttribute(const string &att,ostringstream &sout) { sout << "no debugging"; return false; };
        virtual void addDebugAttributes(Scheduler *scheduler) {};

        /**
         * @brief This function is called when a module is tapped by the user. Prints a message to the console by default.
         Can be overloaded in the user blockCode
         * @param face face that has been tapped */
        virtual void onTap(int face);

        /**
         * @brief Loads the next target from the configuration file into the target attribute
         *  by calling Target::loadNextTarget()
         * @return true if a target has been loaded, false otherwise (No target remaining in config file)
         */
        static bool loadNextTarget();

        /**
         * @brief Add a new message handler to the block code, for message with message type type
         * @param type ID of the message for which a handler needs to be registered
         * @param eventFunc the message handling function as a std::function
         * @note see https://en.cppreference.com/w/cpp/utility/functional/function#Member_functions
         * example: addMessageEventFunc2(BROADCAST_MSG, std::bind(&SimpleColorCode::myBroadcastFunc, this, std::placeholders::_1, std::placeholders::_2)); */
        void addMessageEventFunc2(int type, eventFunc2);

        /**
         * @brief Send message to all connected interface interfaces, except those in the variadic parameters ignore list.
         *        Sending time randomly drawn as follow: tt = now + t0 + (rand * dt), where rand is either {0, 1}
         * @param msg message to be sent
         * @param t0 time of transmission
         * @param dt delta time between two transmissions
         * @param nexcept number of interfaces to ignore
         * @param ... variadic parameters: pointer to the nexcept interfaces to ignore
         * @return Number of messages effectively sent
         */
        int sendMessageToAllNeighbors(Message *msg, Time t0, Time dt, int nexcept, ...);

        /**
         * @copydoc BlockCode::sendMessageToAllNeighbors
         * Identical to sendMessageToAllNeighbors, but prints msgString to the console when the message is sent
         * @param msgString string of the message to be printed when sent
         */
        int sendMessageToAllNeighbors(const char *msgString, Message *msg,
                                      Time t0, Time dt, int nexcept, ...);

        /**
         * @brief Send message to interface dest at time t0 + [0,1]dt
         * @param msg message to be sent (will print the handleable message's name)
         * @param dest destination interface.
         * @param t0 time to wait before sending
         * @param dt potential delay in sending time */
        virtual int sendMessage(HandleableMessage *msg, P2PNetworkInterface *dest,
                                Time t0, Time dt);

        /**
         * @brief Send message to interface dest at time t0 + [0,1]dt
         * @param msg message to be sent
         * @param dest destination interface.
         * @param t0 time to wait before sending
         * @param dt potential delay in sending time */
        int sendMessage(Message *msg, P2PNetworkInterface *dest,
                        Time t0, Time dt);

        /**
         * @copydoc BlockCode::sendMessage
         * @param msgString string to be printed to the console upon sending */
        int sendMessage(const char *msgString, Message *msg, P2PNetworkInterface *dest,
                        Time t0, Time dt);

        /**
         * Callback function called at the end of the motion of a module
         */
        virtual void onMotionEnd() {};

        /**
         * User-implemented debug function that gets called when a module is selected in the GUI
         * @note call is made from GlBlock::getInfo() as it is convenient there. Adding an actual GUI button could more convenient and less "hacky"
         */
        virtual void onBlockSelected() {};

        /**
         * User-implemented debug function that gets called when a VS_ASSERT is triggered
         * @note call is made from utils::assert_handler()
         */
        virtual void onAssertTriggered() {};

        /**
         * User-implemented keyboard handler function that gets called when
         *  a key press event could not be caught by openglViewer
         * @param c key code that was pressed (see openglViewer.cpp)
         * @param x location of the pointer on the x axis
         * @param y location of the pointer on the y axis
         * @note call is made from GlutContext::keyboardFunc (openglViewer.h)
         */
        virtual void onUserKeyPressed(unsigned char c, int x, int y) {};

        /**
         * @brief User-implemented keyboard handler function that gets called when
         *  a special key press event is produced
         * @param c key code that was pressed (see openglViewer.cpp)
         * @param x location of the pointer on the x axis
         * @param y location of the pointer on the y axis
         * @note call is made from GlutContext::SpecialFunc (openglViewer.h)
         */
        virtual void onUserArrowKeyPressed(unsigned char c, int x, int y) {};

        /**
         * @brief Called by world during GL drawing phase, can be used by a user
         *  to draw custom Gl content into the simulated world
         * @note call is made from World::GlDraw
         */
        virtual void onGlDraw() {};

        /**
         * @brief Called by openglviewer during interface drawing phase, can be used by a user
         *  to draw a custom Gl string onto the bottom-left corner of the GUI
         * @note call is made from OpenGlViewer::drawFunc
         * @return a string (can be multi-line with `\n`) to display on the GUI
         */
        virtual string onInterfaceDraw();

        /**
         * @brief Callback function called when an interruption event occurs
         */
        virtual void onInterruptionEvent(shared_ptr<Event> event) {};

/**
         * @brief Callback function called when an neighbor is added or removed
         * @param face: the connected face number
         * @param action: EVENT_ADD_NEIGHBOR or EVENT_REMOVE_NEIGHBOR
         */
        virtual void onNeighborChanged(uint64_t face, int action) {};

        virtual void onEndOfSimulation() {
            cout << "-----------------------------\nEnd of simulation" << endl;
        }
    };

} // BaseSimulator namespace

#endif /* BLOCKCODE_H_ */
