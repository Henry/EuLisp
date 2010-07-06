///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: eval
///  Description: Low-level readline prompt support
///-----------------------------------------------------------------------------

#ifdef READLINE
#include "../Runtime/Ff/eul-ext.h"

// Get the prompt-string from the i-rep module using `prompt-string'
#include "../Comptime2/i-rep.h"
EUL_DEFINTERN(prompt_string, "prompt-string", 0, i_rep)

char* eul_prompt_string()
{
    return eul_string_as_c_string(prompt_string());
}

#endif

///-----------------------------------------------------------------------------
