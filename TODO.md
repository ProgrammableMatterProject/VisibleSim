# TODO – Future improvements and issues

## Issues to address

### User Interactions 
- [x] Catoms2D Rotations from the menu are sometimes mistakenly authorized, and blocks sometimes perform 2 rotations at a time.

### Graphical Window
- [ ] Catoms2D graphical grid size seems too narrow, which cuts modules at coordinate x = (gridSize - 1) in two. See below:
![Catoms2D Broken Border Display](https://i.imgsafe.org/5256b40afe.png)

## Improvements

### Meld Interpreter
- [ ] Enable Meld action for generic rotation events (Rotation 2D/3D)

### Scheduler
- [ ] Refactor Scheduler functions (e.g. startPaused), depends on Debugger implementation
- [ ] Replace current event container `multimap<date, event>` to a more efficient one, e.g., a priority queue.  

### Configuration Files
- [ ] Camera and Spotlight elements could be automatically deduced from the rest of the data in the configuration file. See Configuration::exportToVisibleSim method in https://github.com/nazandre/VisibleSimConfigGenerator/blob/master/build/configuration.cpp.

### Code Organization
- [ ] Organize simulator core source code files in subfolders

### CMake BuildSystem
- [ ] Build CMake with targets: Makefile and Code::Blocks IDE project

## New Features

### Multi Robots
- [ ] Native support for non-modular multi-robot systems with wireless communication capability
- [ ] Support for obstacles as a standard feature in VisibleSim

### Debuggers
- [ ] Meld Interpreter Debugger
- [ ] C++ Debugger

### Scenarios
- [ ] Scenario definition and parsing
- [ ] Scenario events scheduling and execution

