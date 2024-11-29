find_package (PkgConfig)
pkg_check_modules (PC_Epoxy QUIET epoxy)
set (Epoxy_DEFINITIONS ${PC_Epoxy_CFLAGS_OTHER})

find_path (Epoxy_INCLUDE_DIR epoxy/gl.h
           HINTS ${PC_Epoxy_INCLUDEDIR} ${PC_Epoxy_INCLUDE_DIRS} $ENV{EPOXY_DIR}/include
           PATH_SUFFIXES libepoxy)

find_library (Epoxy_LIBRARY NAMES epoxy libepoxy
              HINTS ${PC_Epoxy_LIBDIR} ${PC_Epoxy_LIBRARY_DIRS} $ENV{EPOXY_DIR}/lib)

if (WIN32)
	find_file (Epoxy_SHARED_LIBRARY epoxy-0.dll
               HINTS $ENV{EPOXY_DIR}/bin)
endif ()

include (FindPackageHandleStandardArgs)

find_package_handle_standard_args (epoxy DEFAULT_MSG
                                   Epoxy_LIBRARY Epoxy_INCLUDE_DIR)

mark_as_advanced (Epoxy_INCLUDE_DIR Epoxy_LIBRARY)

set (Epoxy_LIBRARIES ${Epoxy_LIBRARY})
set (Epoxy_INCLUDE_DIRS ${Epoxy_INCLUDE_DIR})

add_library (Epoxy UNKNOWN IMPORTED)
set_target_properties (Epoxy PROPERTIES IMPORTED_LOCATION ${Epoxy_LIBRARY})
set_target_properties (Epoxy PROPERTIES INTERFACE_INCLUDE_DIRECTORIES ${Epoxy_INCLUDE_DIRS})
