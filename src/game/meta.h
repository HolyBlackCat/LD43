#pragma once

#include "audio/context.h"
#include "game/adaptive_viewport.h"
#include "game/render.h"
#include "game/state.h"
#include "game/texture_atlas.h"
#include "graphics/font.h"
#include "graphics/texture.h"
#include "input/mouse.h"
#include "interface/window.h"
#include "utils/config.h"
#include "utils/dynamic_storage.h"
#include "utils/mat.h"
#include "utils/metronome.h"
#include "utils/random.h"

extern const ivec2 screen_size;

extern Interface::Window win;

extern Audio::Context &audio_context();

extern Random<> random; // <> can't be omitted, see `meta.cpp` for details.

extern Render render;

struct FontList
{
    Graphics::Font main;
};
extern FontList &Fonts();

TextureAtlas &Atlas();
void UpdateTextureAtlas();

extern Graphics::Texture main_texture;

extern AdaptiveViewport viewport;

extern Metronome &metronome();

extern DynStorage<States::State> game_state;
