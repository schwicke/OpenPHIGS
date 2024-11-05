SET(PHIGS_LIBRARIES ${PHIGS_LIBRARIES})

ADD_EXECUTABLE(ardisplay ardisplay.c)
TARGET_LINK_LIBRARIES(ardisplay ${PHIGS_LIBRARIES})

ADD_EXECUTABLE(objdisplay objdisplay.c glm.c)
TARGET_LINK_LIBRARIES(objdisplay ${PHIGS_LIBRARIES})

INSTALL(TARGETS
  ardisplay
  objdisplay
  DESTINATION
    bin
)
