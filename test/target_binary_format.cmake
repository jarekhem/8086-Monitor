cmake_minimum_required(VERSION 3.30)

function(target_binary_format)
cmake_parse_arguments(
         BINARY_FORMAT
         ""
         "FORMAT;TARGET;OUTPUT"
         ""
         ${ARGN}
     )
if (${BINARY_FORMAT_FORMAT} STREQUAL BIN)
   set(FORMAT binary)
elseif (${BINARY_FORMAT_FORMAT} STREQUAL HEX)
   set(FORMAT ihex)      
elseif (${BINARY_FORMAT_FORMAT} STREQUAL ELF)
   set(FORMAT elf32)
else()
   message(FATAL_ERROR "Unknown format: " ${BINARY_FORMAT_FORMAT})
endif()

add_custom_command(
   TARGET ${BINARY_FORMAT_TARGET}
   COMMAND ${CMAKE_OBJCOPY} -O ${FORMAT} $<TARGET_FILE:${BINARY_FORMAT_TARGET}> ${BINARY_FORMAT_OUTPUT}
   BYPRODUCTS ${BINARY_FORMAT_OUTPUT}
)
endfunction()