cmake_minimum_required(VERSION 3.28.1)

set(PACKAGE_NAME 8086-Monitor)

if (NOT DEFINED PROJECT_NAME)
    set(NOT_SUBPROJECT ON)
else ()
    set(NOT_SUBPROJECT OFF)
endif ()

project(${PACKAGE_NAME} ASM_NASM)
add_library(${PACKAGE_NAME} STATIC
    monitor.asm
)

if (${NOT_SUBPROJECT})
    add_subdirectory(test)
endif()

target_include_directories(${PACKAGE_NAME} PRIVATE ${MONITOR_CONFIG_DIR})