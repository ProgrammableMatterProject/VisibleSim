cmake_minimum_required(VERSION 2.8.4)
project(VisibleSim)

add_custom_target(VisibleSim COMMAND make -C ${VisibleSim_SOURCE_DIR})