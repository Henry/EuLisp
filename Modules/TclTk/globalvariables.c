#include "globalvariables.h"

// The main window for the application.
// If NULL then the application no longer exists.
Tk_Window mainWindow;

// Interpreter for this application.
Tcl_Interp *interp;

// Maximum length of script received by the tk-bind function
const uint maxScriptLen = 100;
