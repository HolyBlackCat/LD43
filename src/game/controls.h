#pragma once

#include "input/complete.h"

struct Controls
{
    Input::Button
        debug_reload,
        up           = Input::w,
        down         = Input::s,
        left         = Input::a,
        right        = Input::d;

    Input::Mouse mouse;
};

inline Controls controls;
