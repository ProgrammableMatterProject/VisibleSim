/**
 * @file   replayExporter.h
 * @author Pierre Thalamy <pthalamy@pierre-ZenBook-UX433FA>
 * @date   Tue Jun  9 11:13:33 2020
 *
 * @brief  Simulation data exporter to be used for simulation replay
 *
 *
 */

#pragma once

#include <fstream>

#include "utils/exceptions.h"

using namespace std;

/**
 * Configuration exporter that outputs all relevant simulation data to an export file
 *  for simulation reconstruction using a player.
 * Export file will be name replay_<appName>_<confName>_timestamp.vs
 * @note To be used as a singleton instance
 */
class ReplayExporter {
    static inline ReplayExporter* singleton = nullptr; // the singleton instance

    static inline const string extension = "vs";
    ofstream* exportFile = nullptr;     // binary export file
    ofstream* debugFile = nullptr;      // corresponding clear text export file for debugging

    /**
     * @return a filename string with format replay_<appName>_<confName>_timestamp.vs
     */
    string buildExportFilename() const;

    /**
     * Essentially replaces the default extension in exportFn by ".txt"
     * @param exportFn
     * @return Transform export filename exportFn to debug filename
     */
    string debugFilenameFromExportFilename(const string& exportFn) const;
public:
    /**
     * Constructor for the abstract configuration exporter
     * Creates and writes the binary header for the simulation data file
     */
    ReplayExporter();
    virtual ~ReplayExporter() {
        endExport();
    }

    /**
     * @return the binary export file instance
     */
    std::ofstream& getExportFile() const {
        if (exportFile == nullptr)
            throw BaseSimulator::VisibleSimException("(error) ReplayExporter::getExportFile(): ReplayExporter has not been initialized");

        return *exportFile;
    }

    /**
     * @return the clear-text export file instance
     */
    std::ofstream& getDebugFile() const {
        if (exportFile == nullptr)
            throw BaseSimulator::VisibleSimException("(error) ReplayExporter::getDebugFile(): ReplayExporter has not been initialized");

        return *debugFile;
    }

    /**
     * [HEADER]
     * Writes the replay file's header, using the following format:
     *
     * [VS_MAGIC][MODULE_TYPE][GRID DIMENSIONS XYZ]
     *
     * @TODO
     */
    void writeHeader();


    /**
     * Writes an index with the position of all keyframes in the replay file,
     *  indexed by their time
     */
    void writeKeyframesIndex();


    /**
     * Terminates simulation replay export and properly closes associated files
     * @note Is called at scheduler end by default, or when simulator is deleted
     */
    void endExport();

    /**
     * Singleton getter
     * @return the singleton instance
     */
    static ReplayExporter* getInstance() {
        if (not singleton)
            singleton = new ReplayExporter();

        return singleton;
    }

    /**
     * @return true if simulation data is being exported for this instance of the simulation
     */
    static bool isReplayEnabled();
};
