cmake_minimum_required(VERSION 3.28.1)

set(PACKAGE_NAME 8086-Monitor)

project(${PACKAGE_NAME} ASM_NASM)
add_library(${PACKAGE_NAME} STATIC
    monitor.asm
)

if (${PACKAGE_NAME}_IS_TOP_LEVEL)
    add_subdirectory(test)
endif()

target_include_directories(${PACKAGE_NAME} PRIVATE ${MONITOR_CONFIG_DIR})