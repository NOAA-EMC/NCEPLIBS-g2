# This is the CMake build file for utils directory in the
# NCEPLIBS-g2 project. Will only build if BUILD_UTILS=ON
# and BUILD_WITH_W3EMC=ON
#
# George Gayno, Mark Potts, Kyle Gerheiser, Alyson Stahl

if (BUILD_D)
  add_executable(copygb2 copygb2.F90)
  target_link_libraries(copygb2 PUBLIC ${PROJECT_NAME}_d)
  target_link_libraries(copygb2 PRIVATE PNG::PNG ${JASPER_LIBRARIES}
    bacio::bacio ip::ip_d w3emc::w3emc_d)

  if(ip_VERSION LESS 5.0)
    target_link_libraries(copygb2 PRIVATE sp::sp_d)
  endif()
  if(ip_VERSION VERSION_GREATER_EQUAL 4.0.0)
    target_compile_definitions(copygb2 PRIVATE USEIPMOD)
  endif()
  # Use openMP if found.
  if(OpenMP_Fortran_FOUND)
    target_link_libraries(copygb2 PRIVATE OpenMP::OpenMP_Fortran)
  endif()

  if(CMAKE_C_COMPILER_ID MATCHES "^(Intel|IntelLLVM)$")
    set_target_properties(copygb2 PROPERTIES COMPILE_FLAGS "-r8 -auto -convert big_endian -fpp")
  elseif(CMAKE_C_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
    set_target_properties(copygb2 PROPERTIES COMPILE_FLAGS "-fdefault-real-8 -fconvert=big-endian -cpp")
  endif()

  install(TARGETS copygb2 RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
endif()

if (BUILD_4)
  add_executable(cnvgrib cnv12.F90 cnv22.F90 cnv21.F90 cnvgrib.F90)
  target_link_libraries(cnvgrib PUBLIC ${PROJECT_NAME}_4)
  target_link_libraries(cnvgrib PRIVATE bacio::bacio w3emc::w3emc_4
    ${JASPER_LIBRARIES} PNG::PNG ${ZLIB_LIBRARY})

  add_executable(degrib2 prgrib2.F90 degrib2.F90)
  target_link_libraries(degrib2 PUBLIC ${PROJECT_NAME}_4)
  target_link_libraries(degrib2 PRIVATE bacio::bacio w3emc::w3emc_4
    ${JASPER_LIBRARIES} PNG::PNG ${ZLIB_LIBRARY})
    
  if(CMAKE_C_COMPILER_ID MATCHES "^(Intel|IntelLLVM)$")
    set_target_properties(degrib2 PROPERTIES COMPILE_FLAGS "-convert big_endian -fpp")
  elseif(CMAKE_C_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
    set_target_properties(degrib2 PROPERTIES COMPILE_FLAGS "-fconvert=big-endian -cpp")
  endif()

  add_executable(grb2index grb2index.F90)
  target_link_libraries(grb2index PUBLIC ${PROJECT_NAME}_4)
  target_link_libraries(grb2index PRIVATE bacio::bacio w3emc::w3emc_4)

  add_executable(tocgrib2 tocgrib2.F90)
  target_link_libraries(tocgrib2 PUBLIC ${PROJECT_NAME}_4)
  target_link_libraries(tocgrib2 PRIVATE bacio::bacio w3emc::w3emc_4
    ${JASPER_LIBRARIES} PNG::PNG ${ZLIB_LIBRARY})

  add_executable(tocgrib2super makwmo.F90 tocgrib2super.F90)
  target_link_libraries(tocgrib2super PUBLIC ${PROJECT_NAME}_4)
  target_link_libraries(tocgrib2super PRIVATE bacio::bacio w3emc::w3emc_4
    ${JASPER_LIBRARIES} PNG::PNG ${ZLIB_LIBRARY})

  if(CMAKE_C_COMPILER_ID MATCHES "^(Intel|IntelLLVM)$")
    set_target_properties(grb2index PROPERTIES COMPILE_FLAGS "-convert big_endian -fpp")
  elseif(CMAKE_C_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
    set_target_properties(grb2index PROPERTIES COMPILE_FLAGS "-fconvert=big-endian -cpp")
  endif()

  install(TARGETS cnvgrib RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
  install(TARGETS degrib2 RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
  install(TARGETS grb2index RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
  install(TARGETS tocgrib2 RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
  install(TARGETS tocgrib2super RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
endif()

add_executable(copygb copygb.F90)
target_link_libraries(copygb PRIVATE bacio::bacio w3emc::w3emc_d ip::ip_d)

add_executable(grbindex grbindex.f)
target_link_libraries(grbindex PRIVATE bacio::bacio w3emc::w3emc_4)

add_executable(tocgrib makwmo.F90 tocgrib.F90)
target_link_libraries(tocgrib PRIVATE bacio::bacio w3emc::w3emc_4)

if(ip_VERSION LESS 5.0)
  target_link_libraries(copygb PRIVATE sp::sp_d)
endif()
if(ip_VERSION VERSION_GREATER_EQUAL 4.0.0)
  target_compile_definitions(copygb PRIVATE USEIPMOD)
endif()
# Use openMP if found.
if(OpenMP_Fortran_FOUND)
  target_link_libraries(copygb PRIVATE OpenMP::OpenMP_Fortran)
endif()

if(CMAKE_C_COMPILER_ID MATCHES "^(Intel|IntelLLVM)$")
  set_target_properties(copygb PROPERTIES COMPILE_FLAGS "-r8 -auto -heap-arrays")
  set_target_properties(grbindex PROPERTIES COMPILE_FLAGS "-convert big_endian -fpp")
elseif(CMAKE_C_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
  set_target_properties(copygb PROPERTIES COMPILE_FLAGS "-fdefault-real-8 -frecursive")
  set_target_properties(grbindex PROPERTIES COMPILE_FLAGS "-fconvert=big-endian -cpp")
endif()

install(TARGETS copygb RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
install(TARGETS grbindex RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
install(TARGETS tocgrib RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
