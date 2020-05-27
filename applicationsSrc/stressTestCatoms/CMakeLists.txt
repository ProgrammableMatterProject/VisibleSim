cmake_minimum_required(VERSION 3.16)
include_directories(.)

set(PROJECT_NAME stressTestCatoms)

project(${PROJECT_NAME})
set(CMAKE_CXX_STANDARD 17)

set(SRC_DIRS  ../../simulatorCore/src)

IF(UNIX)
    message("Linux env. ...")
    add_compile_definitions(USE_FREEGLUT)

    set(BLOCKS_LIB_DIRS  ../../simulatorCore/lib)
    find_library(BLOCKS_LIBRARIES libsimCatoms3D.a ${BLOCKS_LIB_DIRS})
    add_compile_definitions(USE_FREEGLUT)
    set(CMAKE_PREFIX_PATH FREEGLUT)

    set(GLUT_INCLUDE_DIR /usr/include)
    set(GLUT_glut_LIBRARY /usr/lib/x86_64-linux-gnu/libglut.so)

    # GLUT
    #########################################################
    # FIND GLUT
    #########################################################
    find_package(GLUT REQUIRED)
    link_directories(${GLUT_LIBRARY_DIRS})
    add_definitions(${GLUT_DEFINITIONS})
    if(NOT GLUT_FOUND)
        message(ERROR " GLUT not found!")
    endif(NOT GLUT_FOUND)

    find_package(GLEW REQUIRED)
    if (GLEW_FOUND)
        include_directories(${GLEW_INCLUDE_DIRS})
        #link_libraries(${GLEW_LIBRARIES})
    endif()

    find_library( DL_LIBRARY libdl.so  )
    find_library( PTHREAD_LIBRARY libpthread.so  )
    # OpenGL
    find_package(OpenGL REQUIRED)
    include_directories( ${OPENGL_INCLUDE_DIRS} ${GLUT_INCLUDE_DIRS} ${SRC_DIRS})
ENDIF()

add_definitions(${FREEGLUT_DEFINITIONS} ${OPENGL_DEFINITIONS})

add_executable(${PROJECT_NAME} stressTestCatoms.cpp stressTestCatomsCode.cpp stressTestCatomsCode.hpp)

target_link_libraries(${PROJECT_NAME} ${SMARTBLOCKS_LIBRARIES} ${OPENGL_LIBRARIES} ${FREEGLUT_LIBRARIES} ${GLUT_LIBRARIES} ${GLEW_LIBRARIES} ${DL_LIBRARY} ${PTHREAD_LIBRARY})
