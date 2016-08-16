# TODO – Future improvements and issues

## Issues to address

### User Interactions 
- [ ] Catoms2D Rotations from the menu are sometimes mistakenly authorized, and blocks sometimes perform 2 rotations at a time.

## Improvements

### Meld Interpreter
- [ ] Enable Meld action for generic rotation events (Rotation 2D/3D)

### Scheduler
- [ ] Refactor Scheduler functions (e.g. startPaused), depends on Debugger implementation

### Configuration Files
- [ ] Camera and Spotlight elements could be automatically deduced from the rest of the data in the configuration file. See Configuration::exportToVisibleSim method in https://github.com/nazandre/VisibleSimConfigGenerator/blob/master/build/configuration.cpp.

### Code organization
- [ ] Organize simaltur core source code files in folders

### CMake buildsystem
- [ ] Build CMake with targets: Makefile and Code::Blocks IDE project

## New Features

### Debugger
- [ ] Meld Interpreter Debugger
- [ ] C++ Debugger

### Scenarios
- [ ] Scenario definition and parsing
- [ ] Scenario events scheduling and execution

