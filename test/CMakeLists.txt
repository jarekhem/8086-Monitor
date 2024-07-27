cmake_minimum_required(VERSION 3.30)
set(CMAKE_SYSTEM_PROCESSOR 286)
include(target_binary_format.cmake)


add_executable(${PACKAGE_NAME}-Test
    main.asm
    vectors.asm
    print.asm
    kb.asm
)

add_subdirectory(InterruptHandlers)

target_link_libraries(${PACKAGE_NAME}-Test PRIVATE
    ${PACKAGE_NAME}
)

set(MONITOR_CONFIG_DIR "${CMAKE_CURRENT_LIST_DIR}/" CACHE INTERNAL "")
set(LINKER_SCRIPT ${CMAKE_CURRENT_LIST_DIR}/ldScript.ld)

target_link_options(${PACKAGE_NAME}-Test PUBLIC
        "-T${LINKER_SCRIPT}"
        "-Map=${PROJECT_NAME}-Test.map"
        "-oformat=elf"
)

add_definitions(-p "${CMAKE_CURRENT_LIST_DIR}/architecture.inc")

target_binary_format(
        TARGET ${PACKAGE_NAME}-Test
        FORMAT BIN
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${PACKAGE_NAME}-Test.bin
)
