/**
 * @file   replayExporter.cpp
 * @author Pierre Thalamy <pthalamy@pierre-ZenBook-UX433FA>
 * @date   Tue Jun  9 11:14:00 2020
 *
 * @brief Simulation data exporter to be used for simulation replay
 *
 *
 */

#include "replayExporter.h"

#include "replayTags.h"

#include "../utils/utils.h"
#include "../base/simulator.h"

using namespace BaseSimulator;
using namespace ReplayTags;

ReplayExporter::ReplayExporter() {
    const string& fnbin = buildExportFilename();

    cout << TermColor::BWhite
         << "(replay) exporting simulation data to file: " << TermColor::Reset
         << fnbin << endl;

    exportFile = new ofstream(fnbin, ios::out | ios::binary);

    if (debug) {
        const string& fndebug = debugFilenameFromExportFilename(fnbin);

        cout << TermColor::BWhite
             << "(replay) exporting replay debug to file: " << TermColor::Reset
             << fndebug << endl;

        debugFile = new ofstream(fndebug, ios::out);
    }
}

string ReplayExporter::buildExportFilename() const {
    const string& cliFilename = Simulator::getSimulator()->getCmdLine().getReplayFilename();
    if (not cliFilename.empty())
        return cliFilename;

    auto& cmdLine = Simulator::getSimulator()->getCmdLine();

    string appName = cmdLine.getApplicationName();
    // if appname starts with "./" strip it
    if (appName[0] == '.' and appName[1] == '/')
        appName = appName.substr(2, appName.size());

    string configName = cmdLine.getConfigFile();

    // assumes config name format *.xml
    configName = configName.substr(0, configName.size() - 4);

    stringstream prefix;
    prefix << "replay_" << appName;
    if (configName != "config")
        prefix << "_" << configName;

    return utils::generateTimestampedFilename(prefix.str(), extension, true);
}

string ReplayExporter::debugFilenameFromExportFilename(const string& exportFn) const {
    string str = string(exportFn);

    // @TODO PTHY: DO NOT ASSUME DEFAULT EXTENSION
    std::size_t ext = str.find_last_of(".");

    return str.replace(str.begin() + ext, str.end(), ".txt");
}


void ReplayExporter::endExport() {
    if (exportFile) {
        exportFile->close();
        delete exportFile;
        exportFile = nullptr;
    }

    if (debugFile) {
        debugFile->close();
        delete debugFile;
        debugFile = nullptr;
    }
}

bool ReplayExporter::isReplayEnabled() {
    return Simulator::getSimulator()->getCmdLine().isReplayEnabled();
}

void ReplayExporter::writeHeader() {
    exportFile->write((char*)&VS_MAGIC, sizeof(u4));
    exportFile->write((char*)&MODULE_TYPE_BB, sizeof(u1));

    const Cell3DPosition& gridSize = BaseSimulator::getWorld()->lattice->gridSize;
    exportFile->write((char*)&gridSize, 3 * sizeof(short)); // xyz

    keyFramesIndexPos = exportFile->tellp();

    // cout << "keyFramesIndexPos: " << keyFramesIndexPos << endl;

    if (debug) {
        debugFile->write((char*)&VS_MAGIC, sizeof(u4)); *debugFile << endl;
        *debugFile << (short)MODULE_TYPE_BB << endl;
        *debugFile << gridSize[0] << " " << gridSize[1] << " " << gridSize[2] << endl; // xyz

        keyFramesIndexPosDebug = debugFile->tellp();

        // cout << "keyFramesIndexPosDebug: " << keyFramesIndexPosDebug << endl;
    }
}

void ReplayExporter::writeKeyFramesIndex() {
    //@TODO PTHY/BP
}

void ReplayExporter::writeKeyFrame() {
    //@TODO PTHY/BP
}
