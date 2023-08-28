/*
 * simulator.cpp
 *
 *  Created on: 22 mars 2013
 *      Author: dom
 */

#include "simulator.h"

#include <algorithm>
#include <climits>
#include <unordered_set>

#include "../math/cell3DPosition.h"
#include "../utils/trace.h"
#include "../utils/utils.h"
#include "../utils/global.h"
#include "../events/cppScheduler.h"
#include "../gui/openglViewer.h"
#include "../grid/target.h"
#include "../csg/csg.h"
#include "../csg/csgParser.h"
#include "../replay/replayExporter.h"

using namespace std;

namespace BaseSimulator {

    Simulator *simulator = nullptr;

    Simulator *Simulator::simulator = nullptr;

    bool Simulator::regrTesting = false; // No regression testing by default

/********************************************************************************************/
    Cell3DPosition Simulator::extractCell3DPositionFromString(string str) {
        auto pos1 = str.find_first_of('('),
                pos2 = str.find_last_of(')');
        if (pos1 != string::npos && pos2 != string::npos && pos1 != pos2) {
            str = str.substr(pos1 + 1, pos2 - pos1 - 1);
        }
        pos1 = str.find_first_of(',');
        pos2 = str.find_last_of(',');
        if (pos1 != string::npos && pos2 != string::npos && pos1 != pos2) {
            return {short(stoi(str.substr(0, pos1))),
                    short(stoi(str.substr(pos1 + 1, pos2 - pos1 - 1))),
                    short(stoi(str.substr(pos2 + 1, str.length() - pos1 - 1)))};
        } else return {0, 0, 0};
    }

    pair<int, int> Simulator::extract2DpointFromString(string str) {
        auto pos1 = str.find_first_of('('),
                pos2 = str.find_last_of(')');
        if (pos1 != string::npos && pos2 != string::npos && pos1 != pos2) {
            str = str.substr(pos1 + 1, pos2 - pos1 - 1);
        }
        pos1 = str.find_first_of(",x");
        if (pos1 != string::npos) {
            return {stoi(str.substr(0, pos1)),
                    stoi(str.substr(pos1 + 1, str.length() - pos1 - 1))};
        } else return {0, 0};
    }

    Vector3D Simulator::extractVector3DFromString(string str) {
        auto pos1 = str.find_first_of('('),
                pos2 = str.find_last_of(')');
        if (pos1 != string::npos && pos2 != string::npos && pos1 != pos2) {
            str = str.substr(pos1 + 1, pos2 - pos1 - 1);
        }
        pos1 = str.find_first_of(',');
        pos2 = str.find_last_of(',');
        if (pos1 != string::npos && pos2 != string::npos && pos1 != pos2) {
            return {stof(str.substr(0, pos1)), stof(str.substr(pos1 + 1, pos2 - pos1 - 1)),
                    stof(str.substr(pos2 + 1, str.length() - pos1 - 1))};
        } else return {0.0f, 0.0f, 0.0f};
    }

    Color Simulator::extractColorFromString(string str) {
        // format R,G,B
        auto pos1 = str.find_first_of(','),
                pos2 = str.find_last_of(',');
        if (pos1 != string::npos && pos2 != string::npos && pos1 != pos2) {
            return {stoi(str.substr(0, pos1)),
                    stoi(str.substr(pos1 + 1, pos2 - pos1 - 1)),
                    stoi(str.substr(pos2 + 1, str.length() - pos1 - 1))};
        }

        // format 0xffffff or #ffffff
        bool formatOk = (str.substr(0, 2) == "0x");
        if (!formatOk) {
            size_t pos = str.find_first_of('#');
            if (pos != string::npos) {
                str.replace(pos, 1, "0x");
                formatOk = true;
            }
        }
        if (formatOk) {
            unsigned int c = stoul(str, nullptr, 16);
            return {int(c / 65536), int((c / 256) % 256), int(c % 256)};
        } else {
            // search index of the name of the color in the list
            int index = 0;
            toUppercase(str);
            while (index < NB_COLORS && str != ColorNames[index]) {
                index++;
            }
            return (index != NB_COLORS ? Colors[index] : BLACK);
        }
    }

    bool Simulator::extractBoolFromString(string str) {
        toLowercase(str);
        return (str == "true" || str == "yes" || str == "ok" || str == "1" || str == "show" || str == "on");
    }

/********************************************************************************************/

    Simulator::Simulator(int argc, char *argv[], BlockCodeBuilder _bcb) : bcb(_bcb), cmdLine(argc, argv, _bcb) {
#ifdef DEBUG_OBJECT_LIFECYCLE
        OUTPUT << TermColor::LifecycleColor << "Simulator constructor" << TermColor::Reset << endl;
#endif

        // Ensure that only one instance of simulator is running at once
        if (simulator == nullptr) {
            simulator = this;
            BaseSimulator::simulator = simulator;
        } else {
            ERRPUT << TermColor::ErrorColor << "Only one Simulator instance can be created, aborting !"
                   << TermColor::Reset << endl;
            exit(EXIT_FAILURE);
        }

        // Ensure that the configuration file exists and is well-formed

        string confFileName = cmdLine.getConfigFile();

        xmlDoc = new TiXmlDocument(confFileName.c_str());
        bool isLoaded = xmlDoc->LoadFile();

        random_device rd;
        mt19937 gen(rd());
        uniform_int_distribution<> dis(1, INT_MAX); // [1,intmax]
        if (cmdLine.isSimulationSeedSet()) {
            seed = cmdLine.getSimulationSeed();
        } else {
            // Set random seed
            seed = dis(gen);
        }

        generator = uintRNG((ruint) seed);

        cerr << TermColor::BWhite << "Simulation Seed: " << seed << TermColor::Reset << endl;

        if (!isLoaded) {
            cerr << "error: Could not load configuration file: " << confFileName << endl;
            exit(EXIT_FAILURE);
        }
    }

    Simulator::~Simulator() {
#ifdef DEBUG_OBJECT_LIFECYCLE
        OUTPUT << TermColor::LifecycleColor  << "Simulator destructor" << TermColor::Reset << endl;
#endif
        delete xmlDoc;
        if (ReplayExporter::isReplayEnabled())
            delete ReplayExporter::getInstance();

        deleteWorld();
    }

    void Simulator::deleteSimulator() {
        delete simulator;
        simulator = nullptr;
    }

    void Simulator::loadScheduler(int schedulerMaxDate) {
        int sl = cmdLine.getSchedulerLength();
        int sm = cmdLine.getSchedulerMode();

        CPPScheduler::createScheduler();
        scheduler = getScheduler();

        // Set the scheduler execution mode on start, if enabled
        if (sm != CMD_LINE_UNDEFINED) {
            if (!GlutContext::GUIisEnabled && sm == SCHEDULER_MODE_REALTIME) {
                cerr << "error: Realtime mode cannot be used when in terminal mode" << endl;
                exit(EXIT_FAILURE);
            }
            if (GlutContext::GUIisEnabled) {
                scheduler->setSchedulerMode(sm);
                scheduler->setAutoStart(true);
            }
        }

        if (!GlutContext::GUIisEnabled) {
            // If GUI disabled, and no mode specified, set fastest mode by default (Normally REALTIME)
            scheduler->setSchedulerMode(SCHEDULER_MODE_FASTEST);
        }

        // Set the scheduler termination mode
        scheduler->setSchedulerLength(sl);
        scheduler->setAutoStop(cmdLine.getSchedulerAutoStop());

        if (sl == SCHEDULER_LENGTH_BOUNDED) {
            scheduler->setMaximumDate(cmdLine.getMaximumDate());
        }
    }

    void Simulator::parseConfiguration(int argc, char *argv[]) {
        try {
            TiXmlNode *xmlWorldNode = nullptr;
            auto xmlVSNode = xmlDoc->FirstChild("vs");
#ifdef DEBUG_CONF_PARSING
            OUTPUT << "VS :" << (xmlVSNode ? "OK" : "NO") << endl;
#endif
            if (xmlVSNode) {
                cout << "vs tag: ok" << endl;
                xmlWorldNode = parseVS(xmlVSNode, argc, argv);
            } else {
                cout << "vs tag: no" << endl;
                xmlWorldNode = parseWorld(xmlDoc, argc, argv);
            }

            if (xmlWorldNode) {
#ifdef DEBUG_CONF_PARSING
                OUTPUT << "Configuration file successfully loaded "
                       << TermColor::Reset << endl;
#endif
            } else {
                ERRPUT << TermColor::ErrorColor << "error: Could not find root 'world' element in configuration file"
                       << TermColor::Reset << endl;
                exit(EXIT_FAILURE);
            }

            // Configure the simulation world
            initializeIDPool();

            // Instantiate and configure the Scheduler
            loadScheduler(schedulerMaxDate);

            // Parse and configure the remaining items
            parseBlockList();
            parseObstacles(xmlWorldNode);
            parseCustomizations(xmlWorldNode);
        } catch (ParsingException const &e) {
            cerr << e.what();
            exit(EXIT_FAILURE);
        }
    }

    Simulator::IDScheme Simulator::determineIDScheme() {
        TiXmlElement *element = xmlBlockListNode->ToElement();
        const char *attr = element->Attribute("ids");
        if (attr) {
            string str(attr);
            toUppercase(str);
            if (str.compare("MANUAL") == 0)
                return MANUAL;
            else if (str.compare("ORDERED") == 0)
                return ORDERED;
            else if (str.compare("RANDOM") == 0)
                return RANDOM;
            else {
                stringstream error;
                error << "unknown ID distribution scheme in configuration file: "
                      << str << "\n";
                error << "\texpected values: [ORDERED, MANUAL, RANDOM]" << "\n";
                throw ParsingException(error.str());
            }
        }

        int a,b;
    switch (a) {
        case 3 : b=2;
            break;
        default : b=6;
    }
        return ORDERED;
    }

// Seed for ID generation:
// USES: idseed blocklist XML attribute if specified,
//       OR otherwise, simulation seed if specified,
//       OR otherwise, a random seed
    int Simulator::parseRandomIdSeed() {
        TiXmlElement *element = xmlBlockListNode->ToElement();
        const char *attr = element->Attribute("idseed");
        if (attr) {                // READ Seed
            try {
                string str(attr);
                return stoi(str);
            } catch (const std::invalid_argument &e) {
                stringstream error;
                error << "invalid seed attribute value in configuration file: " << attr << "\n";
                throw ParsingException(error.str());
            }
        } else { // No seed, generate distribution with random seed or cmd line seed
            return cmdLine.isSimulationSeedSet() ? cmdLine.getSimulationSeed() : -1;
        }
    }

    bID Simulator::parseRandomStep() {
        TiXmlElement *element = xmlBlockListNode->ToElement();
        const char *attr = element->Attribute("idStep");
        if (!attr) {
            attr = element->Attribute("step");
            if (attr) {
                cerr << "Warning [step] attribute deprecated, use [idStep] instead!" << endl;
            }
        }
        if (attr) {                // READ Step
            try {
                string str(attr);
                return stol(str);
            } catch (const std::invalid_argument &e) {
                stringstream error;
                error << "invalid step attribute value in configuration file: " << attr << "\n";
                throw ParsingException(error.str());
            }
        } else {                // No step, generate distribution with step of one
            return 1;
        }
    }

//!< std::iota does not support a step for filling the container.
//!< Hence, we use this template wrapper to overload the ++ operator
//!< cf: http://stackoverflow.com/a/34545507/3582770
    template<class T>
    struct IotaWrapper {
        typedef T type;

        type value;
        type step;

        IotaWrapper() = delete;

        IotaWrapper(const type &n, const type &s) : value(n), step(s) {};

        operator type() { return value; }

        IotaWrapper &operator++() {
            value += step;
            return *this;
        }
    };

    void Simulator::generateRandomIDs(const int n, const int idSeed, const int step) {
        // Fill vector with n numbers from 0 and with a distance of step to each other
        IotaWrapper<bID> inc(1, step);
        IDPool = vector<bID>(n);
        std::iota(begin(IDPool), end(IDPool), inc);

        // Seed for ID generation:
        // USES: idseed blocklist XML attribute if specified,
        //       OR otherwise, simulation seed if specified,
        //       OR otherwise, a random seed

        // Properly seed random number generator
        std::mt19937 gen;
        if (idSeed == -1) {
            OUTPUT << "Generating fully random contiguous ID distribution" << endl;
            gen = std::mt19937(seed);
            cerr << TermColor::BWhite << "ID Seed: " << seed << TermColor::Reset << endl;
        } else {
            OUTPUT << "Generating random contiguous ID distribution with seed: "
                   << idSeed << endl;
            gen = std::mt19937(idSeed);
            cerr << TermColor::BWhite << "ID Seed: " << idSeed << TermColor::Reset << endl;
        }

        // Shuffle the elements using the rng
        std::shuffle(begin(IDPool), end(IDPool), gen);
    }

    bID Simulator::countNumberOfModules() {
        TiXmlElement *element;
        const char *attr;
        bID moduleCount = 0;

        // Count modules from block elements
        for (TiXmlNode *child = xmlBlockListNode->FirstChild("block"); child; child = child->NextSibling("block"))
            moduleCount++;

        // Count modules from blocksLine elements
        for (TiXmlNode *child = xmlBlockListNode->FirstChild("blocksLine"); child; child = child->NextSibling(
                "blocksLine")) {
            element = child->ToElement();
            attr = element->Attribute("values");
            if (attr) {
                string str(attr);
                int n = str.length();
                for (int i = 0; i < n; i++) {
                    if (str[i] == '1')
                        moduleCount++;
                }
            }
        }

        // cout << "count" << endl;
        // Count modules from blockBox elements
        for (TiXmlNode *child = xmlBlockListNode->FirstChild("blockBox"); child; child = child->NextSibling(
                "blockBox")) {
            Cell3DPosition boxOrigin(0, 0, 0);
            element = child->ToElement();
            attr = element->Attribute("boxOrigin");
            if (attr) {
                auto pos = extractCell3DPositionFromString(attr);
                boxOrigin.pt[0] = max(short(0), pos[0]);
                boxOrigin.pt[1] = max(short(0), pos[1]);
                boxOrigin.pt[2] = max(short(0), pos[2]);
            }
            Cell3DPosition boxDest(world->lattice->gridSize[0] - 1,
                                   world->lattice->gridSize[1] - 1,
                                   world->lattice->gridSize[2] - 1);
            attr = element->Attribute("boxSize");
            if (attr) {
                auto pos = extractCell3DPositionFromString(attr);
                boxDest.pt[0] = min(world->lattice->gridSize[0] - 1, boxOrigin.pt[0] + pos[0]);
                boxDest.pt[1] = min(world->lattice->gridSize[1] - 1, boxOrigin.pt[1] + pos[1]);
                boxDest.pt[2] = min(world->lattice->gridSize[2] - 1, boxOrigin.pt[2] + pos[2]);
#ifdef DEBUG_CONF_PARSING
                cout << "boxDest" << endl;
#endif
            }

            moduleCount = (boxDest[0] - boxOrigin[0]) * (boxDest[1] - boxOrigin[1]) * (boxDest[2] - boxOrigin[2]);

        }
        // cout << "count=" << moduleCount << endl;

        return moduleCount;
    }

    void Simulator::initializeIDPool() {
        // Simulator.xmlBlockListNode has to be initialized at this point!

        // Determine what assignment model to use, and initialize IDPool according to it
        ids = determineIDScheme();

        // DO NOT initialize IDPool if ORDERED,
        //  it is unecessary and requires additional computation
        //  (Additional reading of the configuration file)
        if (ids == ORDERED) return;

        // Count number of modules in configuration file
        bID numModules = countNumberOfModules();
        cerr << "There are " << numModules << " modules in the configuration" << endl;

        switch (ids) {
            case ORDERED:
                // Fill IDPool with ID {1..N}
                // IDPool = vector<bID>(numModules);
                // std::iota(begin(IDPool), end(IDPool), 1);

                // DO NOT initialize IDPool if ORDERED,
                //  it is unecessary and requires additional computation
                //  (Additional reading of the configuration file)
                break;
            case MANUAL: {
                bID id;
                TiXmlElement *element;
                const char *attr;
                unordered_set<int> dupCheck;            // Set containing all previously assigned IDs, used to check for duplicates
                for (TiXmlNode *child = xmlBlockListNode->FirstChild("block"); child; child = child->NextSibling(
                        "block")) {
                    element = child->ToElement();
                    attr = element->Attribute("id");
                    if (attr) {
                        try {
                            string str(attr);
                            id = stoull(str); // id in range [0, 2^64 - 1]
                        } catch (const std::invalid_argument &e) {
                            stringstream error;
                            error << "invalid id attribute value in configuration file: "
                                  << attr << "\n";
                            throw ParsingException(error.str());
                        } catch (const std::out_of_range &e) {
                            stringstream error;
                            error << "out of range id attribute value in configuration file: "
                                  << attr << "\n";
                            throw ParsingException(error.str());
                        }
                    } else {
                        stringstream error;
                        error << "missing id attribute for block node in configuration file while in MANUAL mode"
                              << "\n";
                        throw ParsingException(error.str());
                    }

                    // Ensure unicity of the ID, by inserting id to the set and checking that insertion took place
                    //  (insertion does not take place if there is a duplicate, and false is returned)
                    if (dupCheck.insert(id).second)
                        IDPool.push_back(id);
                    else {
                        stringstream error;
                        error << "duplicate id attribute " << id
                              << " for block node in configuration file while in MANUAL mode" << "\n";
                        throw ParsingException(error.str());
                    }
                }

            }
                break;
            case RANDOM:
                generateRandomIDs(numModules, parseRandomIdSeed(), parseRandomStep());
                break;
        } // switch
    }

    bool Simulator::parseVisuals(TiXmlNode *parent) {
        TiXmlNode *xmlVisualsNode = parent->FirstChild("visuals");
#ifdef DEBUG_CONF_PARSING
        OUTPUT << "visuals node: " << (xmlVisualsNode ? "OK" : "NO") << endl;
#endif
        if (!xmlVisualsNode) return false;
        TiXmlNode *windowNode = xmlVisualsNode->FirstChild("window");
        if (windowNode) {
#ifdef DEBUG_CONF_PARSING
            OUTPUT << "window node: OK" << endl;
#endif
            TiXmlElement *windowElement = windowNode->ToElement();
            auto attr = windowElement->Attribute("size");
            if (attr) {
                auto res = extract2DpointFromString(attr);
                // TODO: tester full
                GlutContext::initialScreenWidth = res.first;
                GlutContext::initialScreenHeight = res.second;
                GlutContext::screenWidth = GlutContext::initialScreenWidth;
                GlutContext::screenHeight = GlutContext::initialScreenHeight;
#ifdef DEBUG_CONF_PARSING
                OUTPUT << "window size: " << GlutContext::initialScreenWidth << "x" << GlutContext::initialScreenHeight
                       << endl;
#endif
            }

            attr = windowElement->Attribute("backgroundColor");
            if (attr) {
                auto color = extractColorFromString(attr);
                GlutContext::bgColor[0] = color[0] / 255.0;
                GlutContext::bgColor[1] = color[1] / 255.0;
                GlutContext::bgColor[2] = color[2] / 255.0;
#ifdef DEBUG_CONF_PARSING
                OUTPUT << "backgroundColor: " << int(GlutContext::bgColor[0] * 255) << ","
                       << int(GlutContext::bgColor[1] * 255) << "," << int(GlutContext::bgColor[2] * 255) << endl;
#endif
//             cout << str << "=" << GlutContext::bgColor[0] << "," << GlutContext::bgColor[1] << "," << GlutContext::bgColor[2] << endl;
            }

            attr = windowElement->Attribute("gradientColor");
            if (attr) {
                auto color = extractColorFromString(attr);
                GlutContext::hasGradientBackground = true;
                GlutContext::bgColor2[0] = color[0] / 255.0;
                GlutContext::bgColor2[1] = color[1] / 255.0;
                GlutContext::bgColor2[2] = color[2] / 255.0;
#ifdef DEBUG_CONF_PARSING
                OUTPUT << "gradientColor: " << int(GlutContext::bgColor2[0] * 255) << ","
                       << int(GlutContext::bgColor2[1] * 255) << "," << int(GlutContext::bgColor2[2] * 255) << endl;
#endif
            }
        } else {
#ifdef DEBUG_CONF_PARSING
            OUTPUT << "window node: NO" << endl;
#endif
        }

        TiXmlNode *renderNode = xmlVisualsNode->FirstChild("render");
        if (renderNode) {
#ifdef DEBUG_CONF_PARSING
            OUTPUT << "render node: OK" << endl;
#endif
            TiXmlElement *renderElement = renderNode->ToElement();
            auto attr = renderElement->Attribute("shadows");
            if (attr) {
                GlutContext::enableShadows = extractBoolFromString(attr);
#ifdef DEBUG_CONF_PARSING
                OUTPUT << "shadow: " << (GlutContext::enableShadows ? "yes" : "no") << endl;
#endif
            }

            attr = renderElement->Attribute("grid");
            if (attr) {
                GlutContext::showGrid = extractBoolFromString(attr);
#ifdef DEBUG_CONF_PARSING
                OUTPUT << "grid: " << (GlutContext::showGrid ? "yes" : "no") << endl;
#endif
            }
        } else {
#ifdef DEBUG_CONF_PARSING
            OUTPUT << "render node: NO" << endl;
#endif
        }
        return true;
    }

    TiXmlNode *Simulator::parseVS(TiXmlNode *parent, int argc, char *argv[]) {
        /* reading the xml file */
        parseVisuals(parent);
        return parseWorld(parent, argc, argv);
    }

    TiXmlNode *Simulator::parseWorld(TiXmlNode *parent, int argc, char *argv[]) {
        /* reading the xml file */
        auto xmlWorldNode = parent->FirstChild("world");
        if (!xmlWorldNode) return nullptr;
        if (xmlWorldNode) {
            TiXmlElement *worldElement = xmlWorldNode->ToElement();
            const char *attr = worldElement->Attribute("gridSize");
            Cell3DPosition gridSize;
            if (attr) {
                gridSize = extractCell3DPositionFromString(attr);
#ifdef DEBUG_CONF_PARSING
                OUTPUT << "grid size : " << gridSize << endl;
#endif
            } else {
#ifdef DEBUG_CONF_PARSING
                OUTPUT << "warning: No grid size in XML file" << endl;
#endif
            }

            attr = worldElement->Attribute("windowSize");
            if (attr) {
                string str = attr;
                int pos = str.find_first_of(',');
                GlutContext::initialScreenWidth = stoi(str.substr(0, pos));
                GlutContext::initialScreenHeight = stoi(str.substr(pos + 1, str.length() - pos - 1));
                GlutContext::screenWidth = GlutContext::initialScreenWidth;
                GlutContext::screenHeight = GlutContext::initialScreenHeight;
                cerr << "warning [DEPRECATED]: place windowSize in visuals tag!" << endl;
            }

            attr = worldElement->Attribute("maxSimulationTime");
            if (attr) {
                string str = attr;
                Time t = atol(attr);
                int l = strlen(attr);
                if (str.substr(l - 2, 2) == "mn") {
                    t *= 60000000;
                } else if (str.substr(l - 2, 2) == "ms") {
                    t *= 1000;
                } else if (str.substr(l - 1, 1) == "s") {
                    t *= 1000000;
                }

                schedulerMaxDate = t;
                cerr << "warning: maxSimulationTime in the configuration is not supported anymore,"
                     << " please use the command line option [-s <maxTime>]" << endl;
            }

//         // Get Blocksize
//         float blockSize[3] = {0.0,0.0,0.0};
            xmlBlockListNode = xmlWorldNode->FirstChild("blockList");
            if (not xmlBlockListNode) {
                stringstream error;
                error << "No blockList element in XML configuration file" << "\n";
                throw ParsingException(error.str());
            }

            // Create the simulation world and lattice
            loadWorld(gridSize, Vector3D(0, 0, 0), // Always use default blocksize
                      argc, argv);
        } else {
            stringstream error;
            error << "No world in XML configuration file" << "\n";
            throw ParsingException(error.str());
        }

        parseTarget(xmlWorldNode);
        parseCameraAndSpotlight(xmlWorldNode);
        return xmlWorldNode;
    }

    bool Simulator::parseCameraAndSpotlight(TiXmlNode *parent) {
        bool found = false;
        if (GlutContext::GUIisEnabled) {
            Lattice *lattice = getWorld()->lattice;
            auto camera = world->getCamera();

            /* Initialization of the camera and the light */
            // calculate the position of the camera from the lattice size
            Vector3D target(0.5f * lattice->gridSize[0] * lattice->gridScale[0],
                            0.5f * lattice->gridSize[1] * lattice->gridScale[1],
                            0.25f * lattice->gridSize[2] *
                            lattice->gridScale[2]); // usual target point (midx,miy,quarterz)
            camera->setTarget(target);
            double d = target.norme();
            camera->setDistance(3.0 * d);
            camera->setDirection(-30.0 - 90.0, 30.0);
            camera->setNearFar(0.25 * d, 5.0 * d);
            camera->setFOV(35.0);
            camera->setLightParameters(target, -30.0, 30.0, 3.0 * d, 30.0, 0.25 * d, 4.0 * d);

            // loading the camera parameters
            TiXmlNode *nodeConfig = parent->FirstChild("camera");
            found = (nodeConfig != nullptr);
            if (nodeConfig) {
                TiXmlElement *cameraElement = nodeConfig->ToElement();
                const char *attr = cameraElement->Attribute("target");
                double def_near = 1, def_far = 1500;
                if (attr) {
                    auto target = extractVector3DFromString(attr);
                    camera->setTarget(target);
                }

                attr = cameraElement->Attribute("angle");
                if (attr) {
                    float fov = atof(attr);
                    camera->setFOV(fov);
                    cerr << "warning [DEPRECATED]: use [fov] instead of [angle]!" << endl;
                }

                attr = cameraElement->Attribute("fov");
                if (attr) {
                    float fov = atof(attr);
                    camera->setFOV(fov);
                }

                attr = cameraElement->Attribute("directionSpherical");
                if (attr) {
                    float az, ele, dist;
                    auto vect = extractVector3DFromString(attr);
                    az = -90.0 + vect[0];
                    ele = vect[1];
                    dist = vect[2];
                    camera->setDirection(az, ele);
                    camera->setDistance(dist);
                    cerr << "warning [DEPRECATED]: use [thetaPhiDist] instead of [directionSpherical]!" << endl;
                }

                attr = cameraElement->Attribute("thetaPhiDist");
                if (attr) {
                    float az, ele, dist;
                    auto vect = extractVector3DFromString(attr);
                    az = -90.0 + vect[0];
                    ele = vect[1];
                    dist = vect[2];
                    camera->setDirection(az, ele);
                    camera->setDistance(dist);
                }

                attr = cameraElement->Attribute("near");
                if (attr) {
                    def_near = atof(attr);
                }

                attr = cameraElement->Attribute("far");
                if (attr) {
                    def_far = atof(attr);
                }
                camera->setNearFar(def_near, def_far);
            }

            // loading the spotlight parameters
            nodeConfig = parent->FirstChild("spotlight");
            found = found || (nodeConfig != nullptr);
            if (nodeConfig) {
                Vector3D target;
                float az = 0, ele = 60, dist = 1000, fov = 50;
                double nearPlane = 10, farPlane = 2000;
                TiXmlElement *lightElement = nodeConfig->ToElement();
                const char *attr = lightElement->Attribute("target");
                if (attr) {
                    target = extractVector3DFromString(attr);
                }

                attr = lightElement->Attribute("directionSpherical");
                if (attr) {
                    auto vect = extractVector3DFromString(attr);
                    az = -90.0 + vect[0];
                    ele = vect[1];
                    dist = vect[2];
                    cerr << "warning [DEPRECATED]: use [thetaPhiDist] instead of [directionSpherical]!" << endl;
                }

                attr = lightElement->Attribute("thetaPhiDist");
                if (attr) {
                    auto vect = extractVector3DFromString(attr);
                    az = -90.0 + vect[0];
                    ele = vect[1];
                    dist = vect[2];
                }

                attr = lightElement->Attribute("angle");
                if (attr) {
                    fov = atof(attr);
                    cerr << "warning [DEPRECATED]: use [fov] instead of [angle]!" << endl;
                }

                attr = lightElement->Attribute("fov");
                if (attr) {
                    fov = atof(attr);
                }

                camera->getNearFar(nearPlane, farPlane);
                attr = lightElement->Attribute("near");
                if (attr) {
                    nearPlane = atof(attr);
                }

                attr = lightElement->Attribute("far");
                if (attr) {
                    farPlane = atof(attr);
                }
                camera->setLightParameters(target, az, ele, dist, fov, nearPlane, farPlane);

                attr = lightElement->Attribute("show");
                if (attr) {
                    camera->showLightCone(extractBoolFromString(attr));
                }

            }
        }
        return found;
    }

    void Simulator::parseBlockList() {
        int indexBlock = 0;

        TiXmlElement *element = xmlBlockListNode->ToElement();
        if (xmlBlockListNode) {
            Color defaultColor = DARKGREY;
            const char *attr = element->Attribute("color");
            if (attr) {
                defaultColor = extractColorFromString(attr);
                /*string str(attr);
                int pos1 = str.find_first_of(','),
                    pos2 = str.find_last_of(',');
                defaultColor.set(stoi(str.substr(0,pos1)),
                                 stoi(str.substr(pos1+1,pos2-pos1-1)),
                                 stoi(str.substr(pos2+1,str.length()-pos1-1)));*/
#ifdef DEBUG_CONF_PARSING
                OUTPUT << "new default color :" << defaultColor << endl;
#endif
            }
            uint8_t defaultOrientation = 0;
            attr = element->Attribute("orientation");
            if (attr) {
                defaultOrientation = stoi(attr);
#ifdef DEBUG_CONF_PARSING
                OUTPUT << "new default orientation :" << int(defaultOrientation) << endl;
#endif
            }
            /* Reading a block */
            TiXmlNode *block = xmlBlockListNode->FirstChild("block");
            Cell3DPosition position;
            Color color;
            uint8_t orient = defaultOrientation;
            while (block) {
                element = block->ToElement();
                color = defaultColor;
                attr = element->Attribute("color");
                if (attr) {
                    color = extractColorFromString(attr);
#ifdef DEBUG_CONF_PARSING
                    OUTPUT << "new color :" << color << endl;
#endif
                }
                attr = element->Attribute("position");
                if (attr) {
                    position = extractVector3DFromString(attr);
                }

                orient = defaultOrientation;
                attr = element->Attribute("orientation");
                if (attr) {
                    orient = stoi(attr);
#ifdef DEBUG_CONF_PARSING
                    OUTPUT << "new orientation :" << int(orient) << endl;
#endif
                }

                if (not getWorld()->lattice->isInGrid(position)) {
                    cerr << "GridLowerBounds: "
                         << getWorld()->lattice->getGridLowerBounds(position[2]) << endl;
                    cerr << "GridUpperBounds: "
                         << getWorld()->lattice->getGridUpperBounds(position[2]) << endl;
                    stringstream error;
                    error << "module at " << position << " is out of grid" << "\n";
                    throw ParsingException(error.str());
                }

                loadBlock(element, ids == ORDERED ? ++indexBlock : IDPool[indexBlock++],
                          bcb, position, color, orient);

                block = block->NextSibling("block");
            } // end while (block)

            // Reading blocks lines
            block = xmlBlockListNode->FirstChild("blocksLine");
            int line = 0, plane = 0;
            while (block) {
                if (ids == MANUAL) {
                    stringstream error;
                    error << "blocksLine element cannot be used in MANUAL identifier assignment mode" << "\n";
                    throw ParsingException(error.str());
                }

                line = 0;
                element = block->ToElement();
                color = defaultColor;
                attr = element->Attribute("color");
                if (attr) {
                    color = extractColorFromString(attr);
#ifdef DEBUG_CONF_PARSING
                    OUTPUT << "line color :" << color << endl;
#endif
                }

                orient = defaultOrientation;
                attr = element->Attribute("orientation");
                if (attr) {
                    orient = stoi(attr);
#ifdef DEBUG_CONF_PARSING
                    OUTPUT << "line orientation :" << orient << endl;
#endif
                }

                attr = element->Attribute("line");
                if (attr) {
                    line = atoi(attr);
                }
                attr = element->Attribute("plane");
                if (attr) {
                    plane = atoi(attr);
                }
                attr = element->Attribute("values");
                if (attr) {
                    string str(attr);
                    position.pt[0] = 0;
                    position.pt[1] = line;
                    position.pt[2] = plane;
                    int n = str.length();
                    for (int i = 0; i < n; i++) {
                        if (str[i] == '1') {
                            position.pt[0] = i;
                            loadBlock(element, ids == ORDERED ? ++indexBlock : IDPool[indexBlock++],
                                      bcb, position, color, orient);
                        }
                    }
                }
                block = block->NextSibling("blocksLine");
            } // end while (blocksLine)

            // Reading block boxes
            block = xmlBlockListNode->FirstChild("blockBox");
            while (block) {
                if (ids == MANUAL) {
                    stringstream error;
                    error << "blocksLine element cannot be used in MANUAL identifier assignment mode" << "\n";
                    throw ParsingException(error.str());
                }

                element = block->ToElement();
                color = defaultColor;
                attr = element->Attribute("color");
                if (attr) {
                    color = extractColorFromString(attr);
#ifdef DEBUG_CONF_PARSING
                    OUTPUT << "box color :" << color << endl;
#endif
                }

                orient = defaultOrientation;
                attr = element->Attribute("orientation");
                if (attr) {
                    orient = stoi(attr);
#ifdef DEBUG_CONF_PARSING
                    OUTPUT << "box orientation :" << orient << endl;
#endif
                }

                Cell3DPosition boxOrigin(0, 0, 0);
                attr = element->Attribute("boxOrigin");
                if (attr) {
                    auto pos = extractCell3DPositionFromString(attr);
                    boxOrigin.pt[0] = max(short(0), pos[0]);
                    boxOrigin.pt[1] = max(short(0), pos[1]);
                    boxOrigin.pt[2] = max(short(0), pos[2]);
#ifdef DEBUG_CONF_PARSING
                    OUTPUT << "new boxOrigine:" << boxOrigin << endl;
#endif
                }

                Cell3DPosition boxDest(world->lattice->gridSize[0] - 1,
                                       world->lattice->gridSize[1] - 1,
                                       world->lattice->gridSize[2] - 1);
                attr = element->Attribute("boxSize");
                if (attr) {
                    auto pos = extractCell3DPositionFromString(attr);
                    boxDest.pt[0] = min(world->lattice->gridSize[0] - 1, boxOrigin.pt[0] + pos[0]);
                    boxDest.pt[1] = min(world->lattice->gridSize[1] - 1, boxOrigin.pt[1] + pos[1]);
                    boxDest.pt[2] = min(world->lattice->gridSize[2] - 1, boxOrigin.pt[2] + pos[2]);
#ifdef DEBUG_CONF_PARSING
                    OUTPUT << "new boxDest:" << boxDest << endl;
#endif
                }

                assert(world->lattice != nullptr);

                for (short iz = boxOrigin[2]; iz < boxDest[2]; iz++) {
                    for (short iy = boxOrigin[1]; iy < boxDest[1]; iy++) {
                        for (short ix = boxOrigin[0]; ix < boxDest[0]; ix++) {
                            position.set(ix, iy, iz);
                            loadBlock(element,
                                      ids == ORDERED ? ++indexBlock : IDPool[indexBlock++],
                                      bcb, position, color, orient);
                        }
                    }
                }

                block = block->NextSibling("blockBox");
            } // end while (blockBox)*/

            // Reading CSG Object
            block = xmlBlockListNode->FirstChild("csg");
            if (block) {
                TiXmlElement *element = block->ToElement();

                orient = defaultOrientation;
                attr = element->Attribute("orientation");
                if (attr) {
                    orient = stoi(attr);
#ifdef DEBUG_CONF_PARSING
                    OUTPUT << "csg block orientation :" << orient << endl;
#endif
                }

                BoundingBox bb;
                bool boundingBox = true;
                element->QueryBoolAttribute("boundingBox", &boundingBox);
                bool offsetBoundingBox = true;
                element->QueryBoolAttribute("offset", &offsetBoundingBox);

                CSGParser parser;
                string str = element->Attribute("content");
                CSGNode *csgRoot = parser.parseCSG(str);
                csgRoot->toString();
                cout << "---------Encoding scene------------\n" << csgRoot->toCode() << endl;

                if (boundingBox) csgRoot->boundingBox(bb);

                Vector3D csgPos;
                for (short iz = 0; iz <= world->lattice->getGridUpperBounds()[2]; iz++) {
                    const Cell3DPosition &glb = world->lattice->getGridLowerBounds(iz);
                    const Cell3DPosition &ulb = world->lattice->getGridUpperBounds(iz);
                    for (short iy = glb[1]; iy <= ulb[1]; iy++) {
                        for (short ix = glb[0]; ix <= ulb[0]; ix++) {
                            position.set(ix, iy, iz);
                            csgPos = world->lattice->gridToUnscaledWorldPosition(position);

                            if (offsetBoundingBox) {
                                csgPos += bb.P0 - Vector3D(1, 1, 1);
                            } else {
                                csgPos += bb.P0;
                            }

                            if (world->lattice->isInGrid(position)
                                and csgRoot->isInside(csgPos, color)
                                // @note Ignore position already filled through other means
                                // this can be used to initialize some parameters on select
                                // modules using `<block>` elements
                                and not world->lattice->cellHasBlock(position)) {
                                loadBlock(element,
                                          ids == ORDERED ? ++indexBlock : IDPool[indexBlock++],
                                          bcb, position, color, orient);
                            }
                        }
                    }
                }
            }

            // reading a Obj Mesh
            block = xmlBlockListNode->FirstChild("mesh");
            if (block) {
                cout << "load Mesh" << endl;
                TiXmlElement *element = block->ToElement();
                ObjLoader::ObjLoader *obj = nullptr;
                attr = element->Attribute("file");
                if (attr) {
                    auto configFile = cmdLine.getConfigFile();
                    auto p = configFile.find_last_of('/');
                    string configDir=".";
                    if (p!=string::npos) configDir= configFile.substr(0,p);

                    cout << "load OBJ" << configDir << "/" << attr << endl;
                    obj = new ObjLoader::ObjLoader(configDir.c_str(), attr);
                }

                orient = defaultOrientation;
                attr = element->Attribute("orientation");
                if (attr) {
                    orient = stoi(attr);
#ifdef DEBUG_CONF_PARSING
                    OUTPUT << "mesh block orientation :" << orient << endl;
#endif
                }

                color = GREY;
                attr = element->Attribute("color");
                if (attr) {
                    color = extractColorFromString(attr);
#ifdef DEBUG_CONF_PARSING
                    OUTPUT << "model color :" << color << endl;
#endif
                }

                float scale=1.0;
                attr = element->Attribute("scale");
                if (attr) {
                    scale = atof(attr);
#ifdef DEBUG_CONF_PARSING
                    OUTPUT << "model scale :" << scale << endl;
#endif
                }


                if (obj) {
                    Vector3D BBmin, BBmax;
                    obj->getBB(BBmin, BBmax);
                    cout << "Mesh BB" << BBmin << "/" << BBmax << endl;
                    Vector3D origin = -scale*BBmin;
                    Vector3D worldPos;
                    for (short iz = 0; iz <= world->lattice->getGridUpperBounds()[2]; iz++) {
                        const Cell3DPosition &glb = world->lattice->getGridLowerBounds(iz);
                        const Cell3DPosition &ulb = world->lattice->getGridUpperBounds(iz);
                        /*cout << "glb" << glb << endl;
                        cout << "ulb" << ulb << endl;*/
                        for (short iy = glb[1]; iy <= ulb[1]; iy++) {
                            for (short ix = glb[0]; ix <= ulb[0]; ix++) {
                                position.set(ix, iy, iz);
                                if (world->lattice->isInGrid(position)) {
                                    worldPos = origin+(1.0/scale)*world->lattice->gridToUnscaledWorldPosition(position)+Vector3D(0.5,0.5,0.5);
                                    if (obj->isInside(worldPos)) {
                                        loadBlock(element,
                                                  ids == ORDERED ? ++indexBlock : IDPool[indexBlock++],
                                                  bcb, position, color, orient);
                                    }
                                }
                            }
                        }
                    }
                } else {
                    cerr << "OBJ file is missing...\n";
                }
            }

        } else { // end if

            cerr << "warning: no Block List in configuration file" << endl;
        }
    }

    void Simulator::parseTarget(TiXmlNode *parent) {
        Target::targetListNode = parent->FirstChild("targetList");
        if (Target::targetListNode) {
            // Load initial target (BlockCode::target = nullptr if no target specified)
            BlockCode::target = Target::loadNextTarget();
        }
    }

    void Simulator::parseObstacles(TiXmlNode *parent) {
        // loading the obstacles
        TiXmlNode *nodeObstacle = parent->FirstChild("obstacleList");
        if (nodeObstacle) {
            Color defaultColor = DARKGREY;
            TiXmlElement *element = nodeObstacle->ToElement();
            const char *attr = element->Attribute("color");
            if (attr) {
                defaultColor = extractColorFromString(attr);
            }

            nodeObstacle = nodeObstacle->FirstChild("obstacle");
            Cell3DPosition position;
            Color color;
            while (nodeObstacle) {
                element = nodeObstacle->ToElement();
                color = defaultColor;
                attr = element->Attribute("color");
                if (attr) {
                    color = extractColorFromString(attr);
#ifdef DEBUG_CONF_PARSING
                    OUTPUT << "obstacle color :" << color << endl;
#endif
                }
                attr = element->Attribute("position");
                if (attr) {
                    position = extractVector3DFromString(attr);
#ifdef DEBUG_CONF_PARSING
                    OUTPUT << "position : " << position << endl;
#endif
                }

                world->addObstacle(position, color);
                nodeObstacle = nodeObstacle->NextSibling("obstacle");
            } // end while (nodeObstacle)
        }
    }

    void Simulator::parseCustomizations(TiXmlNode *parent) {
        TiXmlNode *customizationNode = parent->FirstChild("customization");
        if (customizationNode) {
            TiXmlNode *rotationDelayNode = customizationNode->FirstChild("motionDelay");

            if (rotationDelayNode) {
                TiXmlElement *element = rotationDelayNode->ToElement();
                const char *attr = element->Attribute("multiplier");

                if (attr != nullptr) {
                    motionDelayMultiplier = atof(attr);
                }
            }
        }
    }

    void Simulator::startSimulation() {
        // Connect all blocks – TODO: Check if needed to do it here (maybe all blocks are linked on addition)
        world->linkBlocks();

        // Finalize scheduler configuration
        Scheduler *scheduler = getScheduler();
        //scheduler->sem_schedulerStart->post();
        scheduler->setState(Scheduler::NOTSTARTED);


        // Start replay export if enabled
        if (cmdLine.isReplayEnabled()) {
            auto replay = ReplayExporter::getInstance();
            ReplayExporter::enable(true);
            replay->writeHeader();
            // ReplayExporter::getInstance()->exportKeyframe();
        }

        //start simulation if autoStart is enabled
        if (scheduler->willAutoStart())
            scheduler->start(scheduler->getSchedulerMode());
        // Enter graphical main loop
        GlutContext::mainLoop();
    }

    ruint Simulator::getRandomUint() {
        return generator();
    }

} // Simulator namespace
