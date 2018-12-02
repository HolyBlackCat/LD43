#include "program/parachute.h"

#include <iomanip>
#include <iostream>
#include <vector>

#include "audio/context.h"
#include "audio/errors.h"
#include "graphics/complete.h"
#include "input/mouse.h"
#include "interface/window.h"
#include "utils/adjust.h"
#include "utils/mat.h"
#include "utils/metronome.h"
#include "utils/range_set.h"
#include "utils/strings.h"

#include "game/adaptive_viewport.h"
#include "game/controls.h"
#include "game/meta.h"
#include "game/render.h"
#include "game/state.h"
#include "game/texture_atlas.h"
#include "game/states/world.h"

extern const ivec2 screen_size = ivec2(480,270);
static const Graphics::ShaderConfig shader_config;

Interface::Window win("Gamma", screen_size*2, Interface::windowed, ADJUST(Interface::WindowSettings{}, min_size = screen_size));

Audio::Context &audio_context()
{
    static Audio::Context ret;
    return ret;
}

static Graphics::DummyVertexArray dummy_vao;

// The `<>` should only be needed in `extern` declarations (since omitting it apparently requires class template argument deduction, which doesn't work at all with `extern` declarations).
// Due to a GCC bug, the presence of `extern` declaration makes `<>` necessary here as well.
Random<> random;

Render render(1000, shader_config);

//{ Fonts
extern FontList &Fonts()
{
    static FontList ret;
    return ret;
}
//}

TextureAtlas &Atlas()
{
    static TextureAtlas atlas;
    return atlas;
}

Graphics::Texture main_texture = Graphics::Texture().Interpolation(Graphics::nearest).Wrap(Graphics::clamp);

static Graphics::FontFile fontfile_main = nullptr;

void UpdateTextureAtlas()
{
    static uint64_t update_time = -1;
    if (update_time != uint64_t(-1) && update_time == metronome().ticks)
        return;
    update_time = metronome().ticks;

    TextureAtlas &atlas = Atlas();

    atlas = TextureAtlas(ivec2(2048), "assets/images", "assets/atlas.png", "assets/atlas.refl");

    namespace Ranges = Unicode::Ranges;
    Unicode::CharSet symbols =
    {
        Unicode::Ranges::Basic_Latin,
        Unicode::Ranges::Latin_1_Supplement,
        Unicode::Ranges::Latin_Extended_A,
        Unicode::Ranges::Latin_Extended_B,
        Unicode::Ranges::Latin_Extended_C,
        Unicode::Ranges::IPA_Extensions,
        Unicode::Ranges::Spacing_Modifier_Letters,
        Unicode::Ranges::Combining_Diacritical_Marks,
        Unicode::Ranges::Greek_and_Coptic,
        Unicode::Ranges::Currency_Symbols,
        Unicode::Ranges::Cyrillic,
        Unicode::Ranges::Cyrillic_Supplement,
    };

    auto storage = atlas.Get("font_storage.png");

    fontfile_main = Graphics::FontFile("assets/Monokat_6x12.ttf", 12);

    Graphics::MakeFontAtlas(const_cast<Graphics::Image &>(atlas.GetImage()), storage.pos, storage.size,
    {
        {Fonts().main, fontfile_main, symbols},
    });

    main_texture.SetData(atlas.GetImage());
}

AdaptiveViewport viewport(shader_config, screen_size);

extern Metronome &metronome()
{
    static Metronome ret;
    return ret;
}

DynStorage<States::State> game_state = nullptr;

#define main SDL_main

int main(int, char**)
{
    Atlas(); // Make sure the atlas is generated.
    audio_context(); // Make sure the context is created.

    Graphics::SetClearColor(fvec3(0));

    Graphics::Blending::Enable();
    Graphics::Blending::FuncNormalPre();

    UpdateTextureAtlas();
    render.SetTexture(main_texture);
    viewport.Update();

    render.SetMatrix(viewport.GetDetails().MatrixCentered());
    controls.mouse.SetMatrix(viewport.GetDetails().MouseMatrixCentered());

    Clock::DeltaTimer delta_timer;

    game_state = game_state.make<States::WorldState>();

    while (1)
    {
        uint64_t delta = delta_timer();
        while (metronome().Tick(delta))
        {
            win.ProcessEvents();

            if (win.Resized())
            {
                viewport.Update();
                controls.mouse.SetMatrix(viewport.GetDetails().MouseMatrixCentered());
            }
            if (win.ExitRequested())
                return 0;

            game_state->Tick();

            audio_context().Tick();
            Audio::CheckErrors();
        }

        game_state->Render();

        win.SwapBuffers();
    }

    return 0;
}
