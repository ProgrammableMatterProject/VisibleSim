cmake_minimum_required(VERSION 3.14)
project(TestGlut)

set(CMAKE_CXX_STANDARD 17)

if(UNIX AND NOT APPLE)
    set(LINUX TRUE)
endif()

IF (WIN32)
    add_compile_definitions(USE_FREEGLUT)
    set(GLUT_DIR F:/freeglut-3.0.0)
    set(CMAKE_PREFIX_PATH FREEGLUT)

    set(FREEGLUT_INCLUDE_DIRS F:/freeglut-3.0.0/include)
    set(FREEGLUT_LIBRARY_DIRS F:/freeglut-3.0.0/cmake-build-debug/lib)
    set(FREEGLUT_LIBRARIES freeglutd.lib)

    find_package(OPENGL REQUIRED)

    include_directories(${FREEGLUT_INCLUDE_DIRS} ${OPENGL_INCLUDE_DIRS})
    link_directories(${FREEGLUT_LIBRARY_DIRS} ${OPENGL_LIBRARY_DIRS})
ENDIF (WIN32)

IF(LINUX)
    add_compile_definitions(USE_FREEGLUT)

    SET(GLUT_INCLUDE_DIR /usr/include)
    SET(GLUT_glut_LIBRARY /usr/lib/x86_64-linux-gnu/libglut.so)

    set(LIBSMARTBLOCKS_DIRS  ./cmake-build-debug)
    SET(LIBSMARTBLOCKS libSmartBlocks.so)
    # GLUT
    #########################################################
    # FIND GLUT
    #########################################################
    find_package(GLUT REQUIRED)
    find_package(GLEW REQUIRED)
    link_directories(${GLUT_LIBRARY_DIRS})
    #include_directories(${GLUT_INCLUDE_DIRS})
    add_definitions(${GLUT_DEFINITIONS})
    if(NOT GLUT_FOUND)
        message(ERROR " GLUT not found!")
    endif(NOT GLUT_FOUND)

    # OpenGL
    find_package(OpenGL REQUIRED)

    include_directories( ${OPENGL_INCLUDE_DIRS} ${GLUT_INCLUDE_DIRS} ${LIBSMARTBLOCKS_DIRS} ${GLEW_INCLUDE_DIRS})
ENDIF()

add_definitions(${FREEGLUT_DEFINITIONS} ${OPENGL_DEFINITIONS})


add_executable(${PROJECT_NAME} main.cpp ../../simulatorCore/src/utils/color.cpp
        ../../simulatorCore/src/gui/objLoader.cpp
        ../../simulatorCore/src/math/vector3D.cpp
        ../../simulatorCore/src/gui/shaders.cpp
        ../../simulatorCore/src/math/matrix44.cpp
        ../../simulatorCore/src/gui/camera.cpp
        ../../simulatorCore/src/grid/cell3DPosition.cpp)
#add_executable(${PROJECT_NAME} main.cpp )
target_link_libraries(${PROJECT_NAME} ${FREEGLUT_LIBRARIES} ${GLUT_LIBRARIES} ${OPENGL_LIBRARIES} ${GLEW_LIBRARIES})

