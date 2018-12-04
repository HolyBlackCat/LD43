#include "world_pch.h"

using World = States::World;

//{ Images
ReflectStruct(AtlasImages,(
    (TextureAtlas::Image)(overlay, tiles, player, blood_tiles, cursor, blood_vial, runes, bunny, book_bg, book_icon, book_arrow, demon_swarm, fireball),
))

AtlasImages images;

void UpdateAtlasImages()
{
    static uint64_t update_time = -1;
    if (update_time != uint64_t(-1) && update_time == metronome().ticks)
        return;
    update_time = metronome().ticks;

    UpdateTextureAtlas();
    auto refl = Refl::Interface(images);
    refl.for_each_field([&](auto index)
    {
        constexpr int i = index.value;
        refl.field_value<i>() = Atlas().Get(refl.field_name(i) + ".png");
    });
}
//}

//{ Sounds
class Sound
{
    float base_vol = 0;
    float rand_pitch = 0;
    Audio::Buffer buffer;
  public:
    Sound(std::string file_name, float base_vol, float rand_pitch) : base_vol(base_vol), rand_pitch(rand_pitch), buffer((void(audio_context()), Audio::Sound(Audio::wav, "assets/sounds/" + file_name))) {}

    Audio::Source operator()(float vol = 1)
    {
        return Audio::Source(buffer).temporary().volume(base_vol * vol).pitch(pow(2, -rand_pitch <= random.real() <= rand_pitch)).relative();
    }
    Audio::Source operator()(fvec2 pos, float vol = 1)
    {
        return operator()(vol).relative(0).pos(pos);
    }

    inline static const int listener_dist = screen_size.x;
    inline static const ivec3 listener_dir = ivec3(0,0,1);
};

namespace Sounds
{
    Sound rune_created("rune_created.wav", 1, 0.4);
    Sound rune_clicked("rune_clicked.wav", 1, 0.4);
    Sound blood_placed("blood_placed.wav", 1, 0.4);
    Sound bunny_jumps("bunny_jumps.wav", 0.07, 0.5);
    Sound bunny_explodes("bunny_explodes.wav", 1, 0.5);
    Sound book_opens("book_opens.wav", 1, 0.4);
    Sound page_turns("page_turns.wav", 1, 0.4);
    Sound player_teleports("player_teleports.wav", 1, 0.2);
    Sound summoned_demon("summoned_demon.wav", 1, 0.2);
    Sound player_dies("player_dies.wav", 1, 0.2);
    Sound demon_dies("demon_dies.wav", 0.5, 0.5);
}
//}

namespace Visuals
{
    Graphics::Texture *fbuf_blood_tex = nullptr;
    Graphics::FrameBuffer *fbuf_blood = nullptr;
}

//{ Config
ReflectStruct(Page,(
    (std::string)(left, right),
))

ReflectStruct(ConfigType,(
    (float)(cam_mass)(=10000),
    (float)(cam_power)(=2),
    (float)(cam_drag)(=0.03),
    (fvec2)(cam_offset)(=ivec2(0,-16)),
    (ivec2)(plr_sprite_size)(=ivec2(36)),
    (int)(plr_walk_anim_frame_len)(=6),
    (int)(plr_walk_anim_frames)(=6),
    (int)(plr_bunny_eating_rad)(=10),
    (std::vector<ivec2>)(plr_hitbox),
    (float)(plr_walk_acc, plr_walk_dec, plr_walk_speed)(=0.2),
    (int)(plr_death_allow_respawn_after)(=40),
    (int)(plr_death_fade_start)(=60),
    (int)(plr_death_fade_end)(=120),
    (int)(plr_initial_blood_amount)(=10),
    (int)(plr_initial_blood_amount_2)(=30),
    (fvec3)(blood_color_mul, blood_color)(=fvec3(1,0.1,0)),
    (float)(blood_alpha)(=0.25),
    (float)(blood_alpha_erased)(=0.1),
    (int)(blood_capacity)(=50),
    (float)(rune_creation_particle_density)(=0.01),
    (float)(rune_creation_particle_min_speed)(=1),
    (float)(rune_creation_particle_max_speed)(=2),
    (float)(rune_creation_particle_min_size)(=3),
    (float)(rune_creation_particle_max_size)(=5),
    (int)(rune_creation_particle_min_life)(=5),
    (int)(rune_creation_particle_max_life)(=15),
    (float)(rune_hover_step)(=0.01),
    (float)(rune_hold_step)(=0.01),
    (float)(rune_sacrifice_hp_per_tick)(=0.01),
    (float)(blood_placing_particle_count)(=10),
    (float)(blood_placing_particle_min_speed)(=0.5),
    (float)(blood_placing_particle_max_speed)(=1),
    (float)(blood_placing_particle_min_size)(=5),
    (float)(blood_placing_particle_max_size)(=10),
    (int)(blood_placing_particle_min_life)(=60),
    (int)(blood_placing_particle_max_life)(=120),
    (int)(bunny_max_hp)(=20),
    (int)(bunny_eating_blood)(=4),
    (int)(bunny_sit_frames_min)(=30),
    (int)(bunny_sit_frames_max)(=60),
    (float)(bunny_jump_dist_min_allowed)(=5),
    (float)(bunny_jump_dist_min)(=32),
    (float)(bunny_jump_dist_max)(=48),
    (float)(bunny_jump_speed)(=0.5),
    (float)(bunny_jump_speed_fast)(=1.5),
    (float)(bunny_jump_height_fac)(=0.4),
    (float)(bunny_detection_rad)(=64),
    (float)(bunny_escape_jump_max_angle_div_pi)(=0.1),
    (int)(bunny_death_particle_count)(=15),
    (float)(bunny_death_particle_min_speed)(=0.2),
    (float)(bunny_death_particle_max_speed)(=0.8),
    (float)(bunny_death_particle_min_size)(=0.2),
    (float)(bunny_death_particle_max_size)(=0.8),
    (int)(bunny_death_particle_min_life)(=60),
    (int)(bunny_death_particle_max_life)(=120),
    (std::vector<ivec2>)(bunny_hitbox),
    (float)(book_icon_hover_step)(=0.1),
    (int)(book_icon_hover_offset)(=24),
    (ivec2)(book_text_offset)(=ivec2(150,16)),
    (fvec3)(book_text_color)(=fvec3(66,38,29)/255),
    (ivec2)(book_clickbox_center)(=ivec2(0,-20)),
    (ivec2)(book_clickbox_size)(=ivec2(300,150)),
    (int)(book_cooldown_after_closed)(=20),
    (std::vector<Page>)(book_pages)(={{"A","B"}}),
    (int)(rune_tp_particle_count)(=30),
    (float)(rune_tp_particle_min_speed)(=1),
    (float)(rune_tp_particle_max_speed)(=1.4),
    (float)(rune_tp_particle_min_size)(=0.4),
    (float)(rune_tp_particle_max_size)(=1),
    (int)(rune_tp_particle_min_life)(=60),
    (int)(rune_tp_particle_max_life)(=120),
    (int)(rune_tp_hold_ticks)(=20),
    (int)(rune_summon_hold_ticks)(=30),
    (float)(rune_attract_speed_fac)(=1),
    (float)(demon_max_tint_alpha)(=0.3),
    (float)(demon_tint_step)(=0.01),
    (int)(demon_swarm_units)(=13),
    (float)(demon_swarm_spawn_dist_min)(=250),
    (float)(demon_swarm_spawn_dist_max)(=300),
    (float)(demon_swarm_speed_min)(=1.5),
    (float)(demon_swarm_speed_max)(=2.5),
    (float)(demon_swarm_angle_div_pi)(=0.1),
    (float)(demon_swarm_hitbox_rad)(=12),
    (int)(demon_swarm_hp)(=40),
    (float)(fireball_hitbox)(=20),
    (int)(fireball_damage)(=16),
    (float)(fireball_speed)(=2),
    (float)(final_message_step)(=0.01),
    (std::string)(final_message),
))

Config<ConfigType> cfg("assets/config.refl");
//}



class Map
{
  public:
    static constexpr int tile_size = 12;

  private:
    ReflectStruct(TileInfo,(
        (using _refl_structure_tuple_tag = void;),
        (int)(source_index)(=0),
        (int)(image_index)(=0),
        (int)(first_variant)(=0),
        (int)(variants)(=0),
        (std::string)(name),
        (bool)(solid)(=0),
    ))

    struct MapInfo
    {
        std::vector<TileInfo> tiles;
        std::map<int, int> tile_index_map; // Maps tile indices in files to actual tile indices.
    };
    inline static MapInfo map_info;

    struct Tile
    {
        int index = 0;
    };

    struct Layer
    {
        ivec2 size = ivec2(0);
        std::vector<Tile> tiles;
        std::vector<int> random_indices;

        Tile &ClampGet(ivec2 pos)
        {
            clamp_var(pos, 0, size-1);
            return tiles[pos.x + pos.y * size.x];
        }
        const Tile &ClampGet(ivec2 pos) const
        {
            return const_cast<Layer *>(this)->ClampGet(pos);
        }

        int GetRandomIndex(ivec2 pos) const
        {
            pos = mod_ex(pos, size);
            return random_indices[pos.x + pos.y * size.x];
        }

        void Render(ivec2 camera_pos) const
        {
            ivec2 corner_a = PixelPosToTile(camera_pos - screen_size/2);
            ivec2 corner_b = PixelPosToTile(camera_pos + screen_size/2);

            for (int y = corner_a.y; y <= corner_b.y; y++)
            for (int x = corner_a.x; x <= corner_b.x; x++)
            {
                ivec2 tile_pos(x, y);
                int index = ClampGet(tile_pos).index;
                const TileInfo &info = map_info.tiles[index];

                if (info.image_index == -1)
                    continue;

                if (info.image_index == -2) // A shadow.
                {
                    for (int i = 0; i < 4; i++)
                    {
                        ivec2 offset = ivec2(1,0).rot90(i);

                        if (index != ClampGet(tile_pos + offset).index && (index != ClampGet(tile_pos + offset.rot90(1)).index || index != ClampGet(tile_pos + offset.rot90(-1)).index))
                            continue;

                        offset *= tile_size /2;
                        ivec2 center = tile_pos * tile_size + tile_size/2 - camera_pos;

                        render.itriangle(center, center + offset + offset.rot90(1), center + offset - offset.rot90(1)).color(fvec3(0)).alpha(0.5);
                    }
                    continue;
                }

                int random_index = info.first_variant + GetRandomIndex(tile_pos) % info.variants;


                render.iquad(tile_pos * tile_size - camera_pos, images.tiles.Region(tile_size * ivec2(info.image_index, random_index), ivec2(tile_size)));
            }
        }
    };

    std::string file_name;

    std::map<std::string, std::vector<ivec2>> objects;

    std::vector<int> blood_tiles;
    std::vector<bool> blocked_tiles;

  public:
    static void UpdateMapInfo()
    {
        static uint64_t update_time = -1;
        if (update_time != uint64_t(-1) && update_time == metronome().ticks)
            return;
        update_time = metronome().ticks;

        try
        {
            auto refl = Refl::Interface(map_info.tiles);
            refl.from_string(MemoryFile("assets/tiles.refl").construct_string());

            map_info.tile_index_map = {};
            for (size_t i = 0; i < map_info.tiles.size(); i++)
                map_info.tile_index_map.insert({map_info.tiles[i].source_index + 1, i});
        }
        catch (std::exception &e)
        {
            Program::Error("Unable to load tile list:\n", e.what());
        }
    }

    Map() {}

    Map(MemoryFile file)
    {
        try
        {
            file_name = file.name();

            Json json(file.construct_string().c_str(), 32);
            Json::View view = json.GetView();
            Json::View view_layer_list = view["layers"];
            int layer_count = view_layer_list.GetArraySize();

            auto LoadLayer = [&](Layer &layer, std::string layer_name)
            {
                Json::View view_layer;

                // Find the layer.
                bool found_layer = 0;
                for (int layer_index = 0; layer_index < layer_count; layer_index++)
                {
                    view_layer = view_layer_list[layer_index];

                    if (view_layer["type"].GetString() != "tilelayer")
                        continue;

                    if (view_layer["name"].GetString() == layer_name)
                    {
                        found_layer = 1;
                        break;
                    }
                }
                if (!found_layer)
                {
                    Program::Error("No layer named `", layer_name, "`.");
                }

                // Get size.
                layer.size = ivec2(view_layer["width"].GetInt(), view_layer["height"].GetInt());


                // Get tiles.
                Json::View view_data = view_layer["data"];
                for (int i = 0; i < layer.size.prod(); i++)
                {
                    Tile new_tile;

                    int source_index = view_data[i].GetInt();
                    if (auto it = map_info.tile_index_map.find(source_index); it == map_info.tile_index_map.end())
                        Program::Error("Invalid tile at position ", ivec2(i % layer.size.x, i / layer.size.x), ".");
                    else
                        new_tile.index = it->second;

                    layer.tiles.push_back(new_tile);
                }

                // Generate random indices.
                layer.random_indices.resize(layer.size.prod());
                for (int &index : layer.random_indices)
                    index = random.integer();
            };

            LoadLayer(mid, "mid");
            LoadLayer(shadows, "shadows");
            LoadLayer(pre_top, "pre_top");
            LoadLayer(top, "top");

            // Load objects
            bool found_layer = 0;
            for (int layer_index = 0; layer_index < layer_count; layer_index++)
            {
                Json::View view_layer = view_layer_list[layer_index];

                if (view_layer["type"].GetString() != "objectgroup")
                    continue;

                if (view_layer["name"].GetString() == "objects")
                {
                    found_layer = 1;

                    Json::View view_obj_list = view_layer["objects"];

                    int object_count = view_obj_list.GetArraySize();
                    for (int i = 0; i < object_count; i++)
                    {
                        Json::View view_obj = view_obj_list[i];
                        if (!view_obj.HasElement("point"))
                            Program::Error("Only point objects are supported.");

                        std::string obj_name = view_obj["name"].GetString();
                        ivec2 obj_pos = iround(fvec2(view_obj["x"].GetReal(), view_obj["y"].GetReal()));

                        objects[obj_name].push_back(obj_pos);
                    }

                    break;
                }
            }
            if (!found_layer)
                Program::Error("No layer named `objects`.");

            blood_tiles.resize(Size().prod());
            blocked_tiles.resize(Size().prod());
        }
        catch (std::exception &e)
        {
            Program::Error("Unable to load map `", file.name(), "`:\n", e.what());
        }
    }

    explicit operator bool() const
    {
        return DefaultLayer().tiles.size() > 0;
    }

    ivec2 Size() const
    {
        return DefaultLayer().size;
    }

    Layer mid;
    Layer shadows;
    Layer pre_top;
    Layer top;

          Layer &DefaultLayer()       {return mid;}
    const Layer &DefaultLayer() const {return mid;}

          Layer &CollisionLayer()       {return top;}
    const Layer &CollisionLayer() const {return top;}

    const std::vector<ivec2> &GetObjects(std::string name) const
    {
        if (auto it = objects.find(name); it == objects.end())
        {
            static std::vector<ivec2> ret;
            return ret;
        }
        else
        {
            return it->second;
        }
    }

    ivec2 GetExactlyOneObject(std::string name) const
    {
        auto &vec = GetObjects(name);
        if (vec.empty())
            Program::Error("No object named `", name, "` in map `", file_name, "`.");
        if (vec.size() > 1)
            Program::Error("More than one object named `", name, "` in map `", file_name, "`.");
        return vec[0];
    }

    static ivec2 PixelPosToTile(ivec2 pos)
    {
        return div_ex(pos, tile_size);
    }
    bool TilePosInBounds(ivec2 pos) const
    {
        return (pos >= 0).all() && (pos < Size()).all();
    }

    bool SolidAtTile(ivec2 pos) const
    {
        int index = CollisionLayer().ClampGet(pos).index;
        return map_info.tiles[index].solid;
    }
    bool SolidAtPixel(ivec2 pos) const
    {
        return SolidAtTile(PixelPosToTile(pos));
    }

    int BloodTileIndexAt(ivec2 pos) const
    {
        if (!TilePosInBounds(pos))
            return 0;
        return blood_tiles[pos.x + pos.y * Size().x];
    }
    bool HaveBloodAt(ivec2 pos) const
    {
        return BloodTileIndexAt(pos) > 0;
    }
    void SetBloodTileIndex(ivec2 pos, int index)
    {
        if (!TilePosInBounds(pos))
            return;
        blood_tiles[pos.x + pos.y * Size().x] = index;
    }
    void PutBlood(ivec2 pos)
    {
        if (HaveBloodAt(pos) || IsTileBlocked(pos))
            return;
        int blood_tile_count = images.blood_tiles.size.x / (tile_size * 2);
        SetBloodTileIndex(pos, (random.integer() < blood_tile_count) + 1);
    }
    void CleanBlood(ivec2 pos)
    {
        int old_index = BloodTileIndexAt(pos);
        if (old_index <= 0)
            return;
        SetBloodTileIndex(pos, -old_index);
    }
    void RenderBloodLayer(ivec2 camera_pos, fvec3 color, float alpha = 1, float beta = 1) const
    {
        ivec2 corner_a = PixelPosToTile(camera_pos - screen_size/2) - 1;
        ivec2 corner_b = PixelPosToTile(camera_pos + screen_size/2) + 1;

        for (int y = corner_a.y; y <= corner_b.y; y++)
        for (int x = corner_a.x; x <= corner_b.x; x++)
        {
            ivec2 tile_pos(x, y);

            int index = BloodTileIndexAt(tile_pos);
            if (index == 0)
                continue;

            render.iquad(tile_pos * tile_size - camera_pos - tile_size/2, images.blood_tiles.Region(ivec2(tile_size*2*(abs(index)-1), 0), ivec2(tile_size*2)))
                  .color(color).alpha(alpha * (index > 0 ? 1 : cfg->blood_alpha_erased)).beta(beta).mix(0);
        }
    }

    bool IsTileBlocked(ivec2 pos) const
    {
        if (!TilePosInBounds(pos))
            return 0;
        return blocked_tiles[pos.x + pos.y * Size().x];
    }
    void BlockTile(ivec2 pos)
    {
        if (!TilePosInBounds(pos))
            return;
        blocked_tiles[pos.x + pos.y * Size().x] = 1;
    }
    void UnblockTile(ivec2 pos)
    {
        if (!TilePosInBounds(pos))
            return;
        blocked_tiles[pos.x + pos.y * Size().x] = 0;
    }

    void Reload()
    {
        *this = Map(file_name);
    }
};

void UpdateEverything()
{
    UpdateAtlasImages();
    Map::UpdateMapInfo();
}

//{ Entities
struct Camera
{
    fvec2 pos_real = fvec2(0);
    ivec2 pos = ivec2(0);
    fvec2 vel = fvec2(0);


};

struct Player
{
    fvec2 pos = ivec2(0);
    fvec2 vel = fvec2(0);
    ivec2 control = ivec2(0);
    ivec2 dir = ivec2(0,1);

    fvec2 drawing_delta = fvec2(0);

    int blood_amount = cfg->plr_initial_blood_amount;

    bool dead = 0;
    int dead_for_ticks = 0;

    int walk_timer = 0;
    int anim_state = 0;
    bool anim_flip_x = 0;

    bool any_rune_hovered = 0;
};

struct Book
{
    bool open = 0;
    bool icon_hovered = 0;
    float icon_hover_time = 0;

    bool left_hovered = 0;
    bool right_hovered = 0;
    bool exit_hovered = 0;

    int cooldown_after_closed = 0;

    struct Page
    {
        std::string left, right;
    };

    int cur_page = 0;
};

struct Rune
{
    using func_t = std::function<void(World &world, Rune &rune)>;

    std::vector<ivec2> tiles;

    ivec2 pos = ivec2(0);
    ivec2 image_pos;
    int rad = 0;
    func_t func;

    bool hovered = 0;
    float hovered_time = 0;
    bool pressed = 0;
    bool holded = 0;
    float holded_time = 0;
    int holded_ticks = 0;

    float angle = 0;

    int remove_me_timer = 0;
    int remove_me_timer_max = 1;

    bool deals_damage_now = 0;
    bool traps_now = 0;

    bool dont_hover = 0;
};

struct BloodParticle
{
    fvec2 pos = fvec2(0,0);
    fvec2 rot_dir = fvec2(1,0);
    fvec2 vel;
    float size = 0;
    int life = 0;
    int max_life = 0;
    float color = 0;

    BloodParticle() {}
    BloodParticle(fvec2 pos, fvec2 vel, float size, int life, int max_life)
        : pos(pos), rot_dir(fvec2::dir(random.angle())), vel(vel), size(size), life(life), max_life(max_life), color(0.5 <= random.real() <= 1) {}
};

struct Lightning
{
    fvec3 color_a = fvec3(0);
    fvec3 color_b = fvec3(1);
    float alpha = 1;
    float beta = 1;
    fvec2 src_pos = fvec2(0);
    fvec2 src_ext_x = fvec2(0);
    fvec2 src_ext_y = fvec2(0);
    fvec2 dst_pos = fvec2(0);
    fvec2 dst_ext_x = fvec2(0);
    fvec2 dst_ext_y = fvec2(0);
    int lines = 0;
    float line_width = 0;
    float width = 0;
    int ticks = 0;

    struct Line
    {
        fvec3 color = fvec3(0.5);
        fvec2 a = fvec2(0);
        fvec2 b = fvec2(0);
        fvec2 c = fvec2(0);
    };

    std::vector<Line> line_list;

    Lightning(fvec3 color_a, fvec3 color_b, float alpha, float beta, fvec2 src_position, float src_angle, fvec2 src_size, fvec2 dst_position, float dst_angle, fvec2 dst_size, int lines, float line_width, float width, int ticks)
        : color_a(color_a), color_b(color_b), alpha(alpha), beta(beta), lines(lines), line_width(line_width), width(width), ticks(ticks)
    {
        fvec2 src_dir = fvec2::dir(src_angle);
        src_pos = src_position;
        src_ext_x = src_dir * src_size.x / 2;
        src_ext_y = src_dir.rot90() * src_size.y / 2;

        fvec2 dst_dir = fvec2::dir(dst_angle);
        dst_pos = dst_position;
        dst_ext_x = dst_dir * dst_size.x / 2;
        dst_ext_y = dst_dir.rot90() * dst_size.y / 2;

        line_list.resize(lines);
    }
};

struct Bunny
{
    fvec2 spawn_pos = fvec2(0);
    bool force_spawn_once = 1;

    bool alive = 0;
    int hp = 0;

    bool flip_x = 0;

    fvec2 pos = fvec2(0);
    float height = 0;
    fvec2 source_pos = fvec2(0);
    fvec2 target_pos = fvec2(0);
    float jump_pos = 0;
    float jump_len = 0;
    float jump_vel = 0;
    bool jumps_now = 0;
    int timer = 0;
};

struct Fireball
{
    fvec2 pos = fvec2(0);
    fvec2 vel = fvec2(0);
    bool dead = 0;
    float dist_sqr = 0;
};

struct Demon
{
    bool remove_me = 0;
    virtual void Init(World &) {}
    virtual void Tick(World &) {}
    virtual void Render(const World &) const {}
    virtual ~Demon() {}
};
//}

extern const std::deque<DynStorage<Demon>> demon_queue_orignial;

extern DynStorage<World> world_copy;

struct States::World
{
    Map map;
    Camera cam;
    Player p;
    Book book;

    std::deque<DynStorage<Demon>> demon_queue = demon_queue_orignial;
    DynStorage<Demon> demon = nullptr;
    float demon_tint = 0;

    std::deque<Rune> runes;
    std::deque<BloodParticle> blood_particles;
    std::deque<Lightning> lightning_particles;
    std::vector<Bunny> bunnies;
    std::vector<int> bunny_indices;
    std::deque<Fireball> fireballs;

    float final_message_alpha = 0;

    World()
    {
        // Initialize visual and audio stuff
        static bool first = 1;
        if (first)
        {
            first = 0;

            SDL_MaximizeWindow(win.Handle());

            Visuals::fbuf_blood_tex = new Graphics::Texture();
            Visuals::fbuf_blood_tex->Interpolation(Graphics::nearest).Wrap(Graphics::clamp).SetData(screen_size);
            Visuals::fbuf_blood = new Graphics::FrameBuffer();
            Visuals::fbuf_blood->Attach(*Visuals::fbuf_blood_tex);

            controls.mouse.HideCursor();

            Audio::Listener::Orientation(Sound::listener_dir, fvec3(0,-1,0));
            Audio::Source::DefaultMaxDistance(screen_size.x * 3);
            Audio::Source::DefaultRefDistance(Sound::listener_dist);
            Audio::Source::DefaultRolloffFactor(1);
        }
        UpdateEverything();

        // Load map
        map = Map("assets/map.json");

        // Set player and camera positions
        p.pos = map.GetExactlyOneObject("player_spawn");
        cam.pos = cam.pos_real = p.pos;

        // Place bunny spawners
        for (ivec2 pos : map.GetObjects("bunny"))
        {
            bunnies.emplace_back().spawn_pos = pos;
            bunny_indices.emplace_back(bunny_indices.size());
        }

        world_copy = world_copy.make(*this);
    }

    bool SolidForPlayerAtOffset(ivec2 offset)
    {
        ivec2 pos = iround(p.pos) + offset;
        for (ivec2 point : cfg->plr_hitbox)
            if (map.SolidAtPixel(pos + point))
                return 1;
        return 0;
    }

    void Tick();
    void Render() const;
};

DynStorage<World> world_copy = nullptr;

//{ Demon variants
struct DemonSwarm : Demon
{
    struct Unit
    {
        fvec2 pos = fvec2(0);
        float rel_angle = 0;
        fvec2 last_dir = fvec2(0);
        float speed = 0;

        int hp = 0;

        bool dead = 0;
        int dead_for_ticks = 0;
    };

    std::vector<Unit> units;

    void Init(World &world) override
    {
        for (int i = 0; i < cfg->demon_swarm_units; i++)
        {
            Unit &new_unit = units.emplace_back();
            new_unit.pos = world.p.pos + fvec2::dir(random.angle(), cfg->demon_swarm_spawn_dist_min <= random.real() <= cfg->demon_swarm_spawn_dist_max);
            new_unit.rel_angle = int(random.sign()) * (f_pi/2 - cfg->demon_swarm_angle_div_pi*f_pi);
            new_unit.speed = (cfg->demon_swarm_speed_min <= random.real() <= cfg->demon_swarm_speed_max);
            new_unit.hp = cfg->demon_swarm_hp;
        }
    }

    void Tick(World &world) override
    {
        if (Input::Button(Input::l_ctrl).down() && controls.debug_reload.pressed())
            units = {};

        remove_me = units.empty();

        for (Fireball &ball : world.fireballs)
            ball.dist_sqr = 1000000;

        auto it = units.begin();
        while (it != units.end())
        {
            Unit &unit = *it;

            if (!world.p.dead)
                unit.last_dir = (world.p.pos - unit.pos).norm();
            if (!unit.dead)
                unit.pos += unit.last_dir * unit.speed;

            if (!unit.dead)
            {
                // Eat bunnies
                for (Bunny &bunny : world.bunnies)
                    if ((bunny.pos - unit.pos).len_sqr() <= ipow(cfg->demon_swarm_hitbox_rad, 2))
                        bunny.hp = 0;

                // Eat player
                if ((world.p.pos - unit.pos).len_sqr() <= ipow(cfg->demon_swarm_hitbox_rad, 2))
                    clamp_var_min(world.p.blood_amount -= 1, 0);

                // Lightning
                world.lightning_particles.push_back(Lightning(fvec3(0), fvec3(0), 0.5, 1, world.p.pos, 0, fvec2(4), unit.pos, 0, fvec2(20), 1, 1, 16, 2));

                // Check rune collision
                for (const Rune &rune : world.runes)
                    if (rune.deals_damage_now && (unit.pos - rune.pos).len_sqr() < ipow(rune.rad, 2))
                        clamp_var_min(unit.hp -= 1, 0);
            }

            // Check rune attraction
            for (const Rune &rune : world.runes)
            {
                fvec2 delta = unit.pos - rune.pos;
                if (rune.traps_now && delta.len_sqr() < ipow(rune.rad*1.2, 2) && delta.len_sqr() > ipow(rune.rad/2, 2))
                {
                    unit.pos -= delta * cfg->rune_attract_speed_fac;
                }
            }

            // Handle fireballs
            for (Fireball &ball : world.fireballs)
            {
                if (ball.dead)
                    continue;

                fvec2 delta = ball.pos - unit.pos;
                float dist_sqr = delta.len_sqr();

                if (!unit.dead && dist_sqr < ipow(cfg->fireball_hitbox, 2))
                {
                    ball.dead = 1;
                    clamp_var_min(unit.hp -= cfg->fireball_damage, 0);
                }

                if (dist_sqr < ball.dist_sqr)
                {
                    ball.dist_sqr = dist_sqr;
                    ball.vel = -delta.norm() * cfg->fireball_speed;
                }


            }

            // Check HP
            if (!unit.dead && unit.hp <= 0)
            {
                unit.dead = 1;
                Sounds::demon_dies(unit.pos);
            }

            // Delete if necessary
            if (unit.dead)
                it = units.erase(it);
            else
                it++;
        }
    }
    void Render(const World &world) const override
    {
        for (const Unit &unit : units)
        {
            float alpha = smoothstep(clamp(linear_mapping<float>(cfg->demon_swarm_hp, 0, 1, 0.25)(unit.hp)));
            render.fquad(unit.pos - world.cam.pos, images.demon_swarm).center().alpha(alpha);
        }
    }
};
//}

const std::deque<DynStorage<Demon>> demon_queue_orignial = []{
    std::deque<DynStorage<Demon>> ret;
    ret.push_back(DynStorage<Demon>::make<DemonSwarm>());
    return ret;
}();

namespace Runes
{
    class RuneInfo
    {
        ivec2 image_pos;
        ivec2 size;
        std::vector<int> points; // 1 - blood, 0 - no blood, -1 - don't care.
        std::vector<ivec2> point_list;

        using func_t = Rune::func_t;
        func_t func;

      public:
        // 1 - blood, 0 - no blood, -1 - don't care.
              int &At(ivec2 pos)       {return points[pos.x + pos.y * size.x];}
        const int &At(ivec2 pos) const {return points[pos.x + pos.y * size.x];}

        RuneInfo() {}
        RuneInfo(ivec2 image_pos, ivec2 list_size, std::initializer_list<bool> list, func_t func) : image_pos(image_pos * Map::tile_size), func(func)
        {
            int list_len = list.end() - list.begin();
            if (list_len != list_size.prod())
                Program::Error("Invalid rune initializer: Size ", size, " doesn't match the list of length ", list_len, ".");

            size = list_size + 2;

            points.resize(size.prod());

            for (int y = 0; y < list_size.y; y++)
            for (int x = 0; x < list_size.x; x++)
            {
                bool blood = list.begin()[x + y * list_size.x];

                At(ivec2(x,y)+1) = (blood ? 1 : -1);

                if (blood)
                    point_list.push_back(ivec2(x,y)+1);
            }

            for (int y = 0; y < size.y; y++)
            for (int x = 0; x < size.x; x++)
            {
                ivec2 pos(x, y);

                if (At(pos) != -1)
                    continue;

                bool touches_blood = 0;
                for (int i = 0; i < 4; i++)
                {
                    if (At(pos + ivec2(1,0).rot90(i)) == 1)
                    {
                        touches_blood = 1;
                        break;
                    }
                }

                if (!touches_blood)
                    continue;

                At(pos) = 0;
            }
        }

        bool ExistsAtPosition(const World &world, ivec2 corner) const
        {
            for (int y = 0; y < size.y; y++)
            for (int x = 0; x < size.x; x++)
            {
                ivec2 pos(x,y);
                int value = At(pos);

                if (value == -1)
                    continue;

                if (world.map.HaveBloodAt(corner + pos) != value)
                    return 0;
            }
            return 1;
        }

        bool FindAroundPoint(const World &world, ivec2 new_point, ivec2 &out_corner) const
        {
            if (!world.map.HaveBloodAt(new_point))
                return 0;

            for (ivec2 point : point_list)
            {
                ivec2 corner = new_point - point;
                if (ExistsAtPosition(world, corner))
                {
                    out_corner = corner;
                    return 1;
                }
            }
            return 0;
        }

        bool TryPlacing(World &world, ivec2 new_point) const
        {
            ivec2 corner;
            bool found = FindAroundPoint(world, new_point, corner);
            if (!found)
                return 0;

            // Create rune object
            Rune &new_rune = world.runes.emplace_back();
            new_rune.rad = (size.max()-2) * Map::tile_size/2;
            new_rune.pos = (corner+1) * Map::tile_size + new_rune.rad;
            new_rune.func = func;
            new_rune.image_pos = image_pos;

            // Remove blood tiles
            for (ivec2 point : point_list)
            {
                world.map.CleanBlood(point + corner);
                world.map.BlockTile(point + corner);
                new_rune.tiles.push_back(point + corner);
            }

            // Add particles
            float area = ipow(new_rune.rad, 2) * f_pi;
            int particle_count = iround(area * cfg->rune_creation_particle_density);
            while (particle_count-- > 0)
            {
                float r = new_rune.rad * sqrt(0 <= random.real() <= 1);
                fvec2 dir = fvec2::dir(random.angle());
                fvec2 offset = dir * r;
                fvec2 pos = new_rune.pos + offset;
                fvec2 vel = dir * float(cfg->rune_creation_particle_min_speed <= random.real() <= cfg->rune_creation_particle_max_speed);
                float size = (cfg->rune_creation_particle_min_size <= random.real() <= cfg->rune_creation_particle_max_size);
                int max_life = cfg->rune_creation_particle_max_life;
                int life = (cfg->rune_creation_particle_min_life <= random.integer() < max_life);

                world.blood_particles.push_back(BloodParticle(pos, vel, size, life, max_life));
            }

            // Play sound
            Sounds::rune_created(new_rune.pos);

            return 1;
        }
    };

    // Rune list
    std::vector<RuneInfo> list
    {
        // Sacrifice
        RuneInfo(ivec2(0,0), ivec2(7,7),{
            0,0,1,1,1,0,0,
            0,1,0,0,0,1,0,
            1,0,1,1,1,0,1,
            1,0,1,0,1,0,1,
            1,0,1,1,1,0,1,
            0,1,0,0,0,1,0,
            0,0,1,1,1,0,0,
        },[](World &world, Rune &rune)
        {
            if (!rune.holded)
                return;

            for (Bunny &bunny : world.bunnies)
            {
                if ((bunny.pos - rune.pos).len_sqr() >= ipow(rune.rad, 2))
                    continue;

                if (bunny.hp > 0)
                {
                    world.lightning_particles.push_back(Lightning(cfg->blood_color, cfg->blood_color/4, 1, 1, bunny.pos, 0, fvec2(6), world.p.pos, 0, fvec2(6), 2, 1, rune.rad/2, 2));
                    int old_bunny_hp = bunny.hp;
                    clamp_var_min(bunny.hp -= cfg->rune_sacrifice_hp_per_tick, 0);

                    int hp_delta = old_bunny_hp - bunny.hp;
                    clamp_var_max(world.p.blood_amount += hp_delta, cfg->blood_capacity);
                }
            }
        }),
        // Teleport
        RuneInfo(ivec2(7,0), ivec2(7,7),{
            0,0,0,1,0,0,0,
            0,0,1,0,1,0,0,
            0,1,1,1,1,1,0,
            1,0,1,0,1,0,1,
            0,1,1,1,1,1,0,
            0,0,1,0,1,0,0,
            0,0,0,1,0,0,0,
        },[](World &world, Rune &rune)
        {
            if (rune.holded_ticks != cfg->rune_tp_hold_ticks)
                return;

            // Particles
            for (int i = 0; i < cfg->rune_tp_particle_count; i++)
            {
                fvec2 pos = fvec2(-1 <= random.real() <= 1, -1 <= random.real() <= 1) * 4;
                fvec2 vel = fvec2::dir(random.angle(), cfg->rune_tp_particle_min_speed <= random.real() <= cfg->rune_tp_particle_max_speed);
                float size = (cfg->rune_tp_particle_min_size <= random.real() <= cfg->rune_tp_particle_max_size);
                int max_life = (cfg->rune_tp_particle_min_life <= random.integer() <= cfg->rune_tp_particle_max_life);

                world.blood_particles.push_back(BloodParticle(pos + world.p.pos, vel, size, 0, max_life));
                world.blood_particles.push_back(BloodParticle(pos + rune.pos, vel, size, 0, max_life));
            }

            // Lightning
            world.lightning_particles.push_back(Lightning(cfg->blood_color, cfg->blood_color/4, 1, 1, world.p.pos, 0, fvec2(4), rune.pos, f_pi/4, fvec2(rune.rad * sqrt(2)), 10, 2, rune.rad, 20));

            // Actually teleport
            Sounds::player_teleports(world.p.pos);
            world.p.pos = rune.pos;
        }),
        // Summon
        RuneInfo(ivec2(7*2,0), ivec2(7,7),{
            0,0,0,1,0,0,0,
            0,1,0,1,0,1,0,
            0,0,0,1,0,0,0,
            1,1,1,1,1,1,1,
            0,0,0,1,0,0,0,
            0,1,0,1,0,1,0,
            0,0,0,1,0,0,0,
        },[](World &world, Rune &rune)
        {
            if (rune.holded_ticks != cfg->rune_summon_hold_ticks)
                return;

            if (rune.remove_me_timer > 0)
                return;

            if (world.demon_queue.empty())
                return;

            rune.remove_me_timer = 1;
            rune.remove_me_timer_max = 60;
            world.demon = std::move(world.demon_queue.front());
            world.demon_queue.pop_front();
            world.demon->Init(world);
            Sounds::summoned_demon(rune.pos);
        }),
        // Trapping
        RuneInfo(ivec2(7*3,0), ivec2(7,7),{
            1,0,0,1,0,0,1,
            0,1,1,1,1,1,0,
            0,1,0,0,0,1,0,
            1,1,0,1,0,1,1,
            0,1,0,0,0,1,0,
            0,1,1,1,1,1,0,
            1,0,0,1,0,0,1,
        },[](World &world, Rune &rune)
        {
            (void)world;
            rune.dont_hover = 1;
            rune.traps_now = 1;
            world.lightning_particles.push_back(Lightning(fvec3(1, 0.5, 1), fvec3(0.5,0,0.5), 1, 1, rune.pos, 0, ivec2(rune.rad/3), rune.pos, f_pi/4, ivec2(rune.rad/3), 1, 2, 16, 2));
        }),
        // Electricity
        RuneInfo(ivec2(7*4,0), ivec2(7,7),{
            0,0,0,1,0,0,0,
            0,1,0,0,0,1,0,
            0,0,0,0,0,0,0,
            1,0,0,0,0,0,1,
            0,0,0,0,0,0,0,
            0,1,0,0,0,1,0,
            0,0,0,1,0,0,0,
        },[](World &world, Rune &rune)
        {
            (void)world;
            rune.dont_hover = 1;
            rune.deals_damage_now = 1;

            world.lightning_particles.push_back(Lightning(fvec3(0.5, 1, 1), fvec3(1), 1, 1, rune.pos, 0, ivec2(rune.rad), rune.pos, f_pi/4, ivec2(rune.rad), 6, 2, 16, 2));
        }),
        // Fire
        RuneInfo(ivec2(7*5,0), ivec2(5,5),{
            0,0,1,0,0,
            0,1,1,1,0,
            1,1,0,1,1,
            0,1,1,1,0,
            0,0,1,0,0,
        },[](World &world, Rune &rune)
        {
            if (rune.holded_ticks != cfg->rune_summon_hold_ticks)
                return;

            if (rune.remove_me_timer > 0)
                return;

            rune.remove_me_timer = 1;
            rune.remove_me_timer_max = 40;

            Fireball &ball = world.fireballs.emplace_back();
            ball.pos = rune.pos;
            ball.vel = fvec2::dir(random.angle(), cfg->fireball_speed);
            Sounds::bunny_explodes(rune.pos, 0.5).pitch(0.5);
        }),
    };

    bool TryPlacingRune(World &world, ivec2 new_point)
    {
        for (size_t i = 0; i < list.size(); i++)
            if (list[i].TryPlacing(world, new_point))
                return 1;
        return 0;
    }
}

void States::World::Tick()
{
    // Reload stuff if F5 is pressed.
    if (controls.debug_reload.pressed())
    {
        if (Input::Button(Input::l_shift).up())
        {
            UpdateEverything();
            map.Reload();
            cfg.Reload();
        }
        else
        {
            if (demon_queue.size() > 0)
            {
                demon = std::move(demon_queue.front());
                demon_queue.pop_front();
                demon->Init(*this);
            }
        }
    }

    { // Player
        { // Controls
            // Movement
            p.control = ivec2(controls.right.down() - controls.left.down(), controls.down.down() - controls.up.down());
            if (p.dead)
                p.control = ivec2(0);

            if (p.control)
            {
                p.dir = p.control;

                fvec2 dir_norm = fvec2(p.dir).norm();
                p.vel += dir_norm * cfg->plr_walk_acc;
                if (p.vel.len_sqr() > ipow(cfg->plr_walk_speed, 2))
                    p.vel = p.vel.norm() * cfg->plr_walk_speed;

                for (int i = 0; i < 2; i++)
                {
                    if (p.control[i])
                        continue;
                    if (abs(p.vel[i]) <= cfg->plr_walk_dec)
                        p.vel[i] = 0;
                    else
                        p.vel[i] -= sign(p.vel[i]) * (abs(p.vel[i]) - cfg->plr_walk_dec);
                }
            }
            else
            {
                float speed = p.vel.len();
                if (speed <= cfg->plr_walk_dec)
                    p.vel = ivec2(0);
                else
                    p.vel = p.vel / speed * (speed - cfg->plr_walk_dec);
            }

            constexpr float vel_step = 0.1;
            ivec2 int_vel = iround(p.vel / vel_step);
            while (int_vel)
            {
                for (int i = 0; i < 2; i++)
                {
                    if (!int_vel[i])
                        continue;

                    ivec2 offset = sign(int_vel * ivec2(1,0).rot90(i));
                    if (SolidForPlayerAtOffset(offset))
                    {
                        if (sign(p.vel[i] * int_vel[i]) > 0)
                            p.vel[i] = 0;
                        int_vel[i] = 0;
                        continue;
                    }

                    p.pos[i] += sign(int_vel[i]) * vel_step;

                    int_vel[i] -= sign(int_vel[i]);
                }
            }
        }

        { // Animation
            if (p.control)
                p.walk_timer++;
            else
                p.walk_timer = 0;

            int anim_frame = p.walk_timer / cfg->plr_walk_anim_frame_len % cfg->plr_walk_anim_frames;

            int dir_index = p.dir == ivec2( 0, 1) ? 0 :
                            p.dir == ivec2( 1, 1) ? 1 :
                            p.dir == ivec2( 1, 0) ? 2 :
                            p.dir == ivec2( 1,-1) ? 3 :
                            p.dir == ivec2( 0,-1) ? 4 :
                            p.dir == ivec2(-1,-1) ? 5 :
                            p.dir == ivec2(-1, 0) ? 6 :
                                        /*(-1, 1)*/ 7;

            p.anim_flip_x = dir_index >= 4;

            p.anim_state = min(dir_index, 8 - dir_index) * cfg->plr_walk_anim_frames + anim_frame;
        }

        { // Death
            if (!p.dead && p.blood_amount <= 0)
            {
                p.dead = 1;
                Sounds::player_dies(p.pos);
                Sounds::bunny_explodes(p.pos); // Sic.

                for (int i = 0; i < cfg->bunny_death_particle_count; i++)
                {
                    fvec2 pos = p.pos + fvec2(-1 <= random.real() <= 1, -1 <= random.real() <= 1) * 4;
                    fvec2 vel = fvec2::dir(random.angle(), cfg->bunny_death_particle_min_speed <= random.real() <= cfg->bunny_death_particle_max_speed);
                    float size = (cfg->bunny_death_particle_min_size <= random.real() <= cfg->bunny_death_particle_max_size);
                    int max_life = (cfg->bunny_death_particle_min_life <= random.integer() <= cfg->bunny_death_particle_max_life);

                    blood_particles.push_back(BloodParticle(pos, vel, size, 0, max_life));
                }
            }

            if (p.dead)
                p.dead_for_ticks++;
            else if (p.dead_for_ticks > 0)
                p.dead_for_ticks--;
        }

        { // Respawning
            if (p.dead && p.dead_for_ticks > cfg->plr_death_allow_respawn_after && Input::Button{}.AssignKey())
            {
                *this = *world_copy;

                p.dead_for_ticks = cfg->plr_death_fade_end;
                p.blood_amount = cfg->plr_initial_blood_amount_2;
            }
        }
    }

    { // Demon
        // Demon tick
        if (demon)
        {
            demon->Tick(*this);

            if (demon->remove_me)
                demon = nullptr;
        }

        // Tint
        clamp_var(demon_tint += (bool(demon) ? 1 : -1) * cfg->demon_tint_step);
    }

    { // Bunnies
        for (Bunny &bunny : bunnies)
        {
            auto SolidAtOffset = [&](ivec2 offset)
            {
                ivec2 pos = iround(bunny.pos) + offset;
                for (ivec2 point : cfg->bunny_hitbox)
                    if (map.SolidAtPixel(pos + point))
                        return 1;
                return 0;
            };

            bool spawn_on_screen = ((abs(bunny.spawn_pos - cam.pos) <= screen_size/2 + Map::tile_size * 1).all());
            bool body_on_screen = ((abs(bunny.pos - cam.pos) <= screen_size/2 + Map::tile_size * 1).all());

            // Spawn a new bunny
            if ((!bunny.alive && !spawn_on_screen) || bunny.force_spawn_once)
            {
                bunny.force_spawn_once = 0;

                bunny.alive = 1;
                bunny.hp = cfg->bunny_max_hp;
                bunny.pos = bunny.spawn_pos;
                bunny.jumps_now = 0;
                bunny.timer = (cfg->bunny_sit_frames_min <= random.integer() <= cfg->bunny_sit_frames_max);
            }

            // Player collision
            if (bunny.alive && bunny.hp > 0 && (p.pos - bunny.pos.sub_y(bunny.height)).len_sqr() < ipow(cfg->plr_bunny_eating_rad, 2))
            {
                clamp_var_max(p.blood_amount += cfg->bunny_eating_blood, cfg->blood_capacity);
                bunny.hp = 0;
            }

            // Check hp
            if (bunny.alive && bunny.hp == 0 && body_on_screen)
            {
                bunny.alive = 0;
                Sounds::bunny_explodes(bunny.pos);

                for (int i = 0; i < cfg->bunny_death_particle_count; i++)
                {
                    fvec2 pos = bunny.pos + fvec2(-1 <= random.real() <= 1, -1 <= random.real() <= 1) * 4;
                    fvec2 vel = fvec2::dir(random.angle(), cfg->bunny_death_particle_min_speed <= random.real() <= cfg->bunny_death_particle_max_speed);
                    float size = (cfg->bunny_death_particle_min_size <= random.real() <= cfg->bunny_death_particle_max_size);
                    int max_life = (cfg->bunny_death_particle_min_life <= random.integer() <= cfg->bunny_death_particle_max_life);

                    blood_particles.push_back(BloodParticle(pos, vel, size, 0, max_life));
                }
            }

            // Movement
            if (bunny.alive)
            {
                if (bunny.jumps_now)
                {
                    bunny.jump_pos += bunny.jump_vel;
                    float t = bunny.jump_pos / bunny.jump_len;
                    fvec2 old_pos = bunny.pos;
                    bunny.pos = (1 - t) * bunny.source_pos + t * bunny.target_pos;
                    bunny.height = (t - t*t) * 4 * bunny.jump_len * cfg->bunny_jump_height_fac;

                    bool stop_jump = 0;
                    if (SolidAtOffset(ivec2(0)))
                        bunny.pos = old_pos;

                    if (bunny.jump_pos >= bunny.jump_len || stop_jump)
                    {
                        bunny.jumps_now = 0;
                        bunny.timer = (cfg->bunny_sit_frames_min <= random.integer() <= cfg->bunny_sit_frames_max);
                    }
                }
                else
                {
                    bunny.height = 0;
                    bunny.timer--;

                    fvec2 dir_away_from_player = bunny.pos - p.pos;
                    bool sees_player = dir_away_from_player.len_sqr() <= ipow(cfg->bunny_detection_rad, 2);

                    if (bunny.timer <= 0 || sees_player)
                    {
                        float desired_jump_len = cfg->bunny_jump_dist_min <= random.real() <= cfg->bunny_jump_dist_max;

                        fvec2 jump_dir;
                        if (!sees_player)
                            jump_dir = fvec2::dir(random.angle());
                        else
                            jump_dir = fvec2::dir(dir_away_from_player.angle() + float(-1 <= random.real() <= 1) * cfg->bunny_escape_jump_max_angle_div_pi * f_pi);

                        if (sees_player && SolidAtOffset(jump_dir * desired_jump_len))
                            jump_dir = fvec2::dir(random.angle());


                        for (bunny.jump_len = 0; bunny.jump_len <= desired_jump_len; bunny.jump_len += 0.5)
                            if (SolidAtOffset(iround(jump_dir * bunny.jump_len)))
                                break;

                        bunny.jump_len -= 1;
                        if (bunny.jump_len > cfg->bunny_jump_dist_min_allowed)
                        {
                            bunny.jumps_now = 1;
                            bunny.jump_vel = (sees_player ? cfg->bunny_jump_speed_fast : cfg->bunny_jump_speed);
                            bunny.source_pos = bunny.pos;
                            bunny.target_pos = bunny.pos + jump_dir * bunny.jump_len;
                            bunny.flip_x = jump_dir.x > 0;
                            bunny.jump_pos = 0;

                            if (body_on_screen)
                                Sounds::bunny_jumps(bunny.pos);
                        }
                    }
                }
            }

            // Rune trapping
            for (const Rune &rune : runes)
            {
                fvec2 delta = bunny.pos - rune.pos;
                if (rune.traps_now && delta.len_sqr() < ipow(rune.rad, 2) && delta.len_sqr() > ipow(rune.rad/2, 2))
                {
                    bunny.pos -= delta * cfg->rune_attract_speed_fac;
                    if ((bunny.target_pos - rune.pos).len_sqr() > ipow(rune.rad, 2))
                        bunny.target_pos = rune.pos + (bunny.target_pos - rune.pos).norm() * rune.rad/2;
                }
            }
        }

        std::sort(bunny_indices.begin(), bunny_indices.end(), [&](int a, int b)
        {
            int y_a = bunnies[a].pos.y;
            int y_b = bunnies[b].pos.y;
            return y_a < y_b;
        });
    }

    { // Fireballs
        auto it = fireballs.begin();
        while (it != fireballs.end())
        {
            it->pos += it->vel;

            if (map.SolidAtPixel(it->pos))
            {
                it->dead = 1;
                Sounds::bunny_explodes(it->pos, 0.5).pitch(0.5);
            }

            if (it->dead)
                it = fireballs.erase(it);
            else
                it++;
        }
    }

    { // Player drawing runes
        auto TryPosition = [&](ivec2 mouse_pos)
        {
            p.drawing_delta = mouse_pos + cam.pos - p.pos;

            ivec2 mouse_world_pos = mouse_pos + cam.pos;
            ivec2 mouse_tile = map.PixelPosToTile(mouse_world_pos);

            if (!map.TilePosInBounds(mouse_tile))
                return;

            ivec2 pixel_pos = mouse_tile * Map::tile_size + Map::tile_size/2;

            if (controls.mouse.left.down() && !map.HaveBloodAt(mouse_tile) && !map.IsTileBlocked(mouse_tile) && !map.SolidAtTile(mouse_tile))
            {
                if (p.blood_amount > 1)
                {
                    p.blood_amount--;
                    map.PutBlood(mouse_tile);


                    Sounds::blood_placed(pixel_pos);

                    Runes::TryPlacingRune(*this, mouse_tile);

                    int particle_count = cfg->blood_placing_particle_count;
                    while (particle_count-- > 0)
                    {
                        ivec2 pos = mouse_tile * Map::tile_size + ivec2(random.integer() < Map::tile_size, random.integer() < Map::tile_size);
                        fvec2 vel = fvec2::dir(random.angle(), cfg->blood_placing_particle_min_speed <= random.real() <= cfg->blood_placing_particle_max_speed);
                        float size = (cfg->blood_placing_particle_min_size <= random.real() <= cfg->blood_placing_particle_max_size);
                        int max_life = cfg->blood_placing_particle_max_life;
                        int life = (cfg->blood_placing_particle_min_life <= random.integer() < max_life);

                        blood_particles.push_back(BloodParticle(pos, vel, size, life, max_life));
                    }

                    lightning_particles.push_back(Lightning(cfg->blood_color, cfg->blood_color/4, 1, 1, p.pos, 0, fvec2(2), pixel_pos, 0, fvec2(Map::tile_size), 2, 1, 24, 5));
                }
            }
            else if (controls.mouse.right.down() && map.HaveBloodAt(mouse_tile))
            {
                map.CleanBlood(mouse_tile);
                Sounds::blood_placed(pixel_pos);
                lightning_particles.push_back(Lightning(cfg->blood_color, 1 - (1 - cfg->blood_color) * 0.5, 1, 1, p.pos, 0, fvec2(2), pixel_pos, 0, fvec2(Map::tile_size), 2, 1, 24, 5));

                for (int i = 0; i < 4; i++)
                    Runes::TryPlacingRune(*this, mouse_tile + ivec2(1,0).rot90(i));
            }
        };

        if (!p.any_rune_hovered && !book.icon_hovered && book.cooldown_after_closed == 0 && !p.dead)
        {
            if (controls.mouse.left.down() || controls.mouse.right.down())
            {
                ivec2 a = controls.mouse.pos() - controls.mouse.pos_delta();
                ivec2 b = controls.mouse.pos();
                int len = iround((b - a).len());
                for (int i = 0; i < len; i++)
                {
                    ivec2 point = a + iround((b - a) / float(len) * i);
                    TryPosition(point);
                }
            }

            TryPosition(controls.mouse.pos());
        }
    }

    { // Runes
        p.any_rune_hovered = 0;

        auto it = runes.begin();
        while (it != runes.end())
        {
            Rune &rune = *it;

            ivec2 delta_to_mouse = cam.pos + controls.mouse.pos() - rune.pos;
            rune.hovered = (delta_to_mouse.len_sqr() < ipow(rune.rad, 2)) && !rune.dont_hover;
            clamp_var(rune.hovered_time += (rune.hovered ? 1 : -1) * cfg->rune_hover_step);

            if (rune.hovered)
                p.any_rune_hovered = 1;

            rune.pressed = rune.hovered && controls.mouse.right.pressed() && !book.icon_hovered && book.cooldown_after_closed == 0 && !p.dead;
            if (rune.pressed)
                Sounds::rune_clicked(rune.pos);

            rune.holded = rune.hovered && controls.mouse.right.down() && !book.icon_hovered && book.cooldown_after_closed == 0 && !p.dead;

            clamp_var(rune.holded_time += (rune.holded ? 1 : -1) * cfg->rune_hold_step);

            if (rune.holded)
                lightning_particles.push_back(Lightning(cfg->blood_color, cfg->blood_color/2, 1, 1, rune.pos, 0, ivec2(rune.rad*1.2), rune.pos, f_pi/4, ivec2(rune.rad*1.2), 5, 2, 16, 2));

            if (rune.holded)
                rune.holded_ticks++;
            else
                rune.holded_ticks = 0;

            rune.func(*this, rune);

            if (rune.remove_me_timer)
                rune.remove_me_timer++;

            if (rune.remove_me_timer >= rune.remove_me_timer_max)
            {
                for (ivec2 tile : rune.tiles)
                    map.UnblockTile(tile);
                it = runes.erase(it);
            }
            else
            {
                it++;
            }
        }
    }

    { // Camera
        fvec2 cam_target = p.pos + cfg->cam_offset;

        fvec2 delta = cam.pos_real.delta(cam_target);
        float dist = delta.len();

        cam.vel *= 1 - cfg->cam_drag;
        cam.vel += delta.norm() * pow(dist, cfg->cam_power) / cfg->cam_mass;
        cam.pos_real += cam.vel;
        cam.pos = iround(cam.pos_real);
    }

    { // Book
        book.icon_hovered = !book.open && !p.dead && (abs(controls.mouse.pos() - ivec2(-screen_size.x/2, screen_size.y/2)) < images.book_icon.size).all();
        clamp_var(book.icon_hover_time += cfg->book_icon_hover_step * (book.icon_hovered ? 1 : -1));

        if (book.icon_hovered && controls.mouse.left.pressed())
        {
            book.open = 1;
            Sounds::book_opens();
        }

        if (book.open && !book.icon_hovered/*sic*/)
        {
            book.cooldown_after_closed = cfg->book_cooldown_after_closed;

            book.left_hovered = 0;
            book.right_hovered = 0;
            book.exit_hovered = 0;

            if (((controls.mouse.pos() - cfg->book_clickbox_center).abs() <= cfg->book_clickbox_size / 2).all())
            {
                if (controls.mouse.pos().x < cfg->book_clickbox_center.x)
                    book.left_hovered = (book.cur_page > 0);
                else
                    book.right_hovered = (book.cur_page < int(cfg->book_pages.size())-1);
            }
            else
            {
                book.exit_hovered = 1;
            }

            if (controls.mouse.left.pressed())
            {
                book.cur_page += book.right_hovered - book.left_hovered;
                if (book.right_hovered || book.left_hovered)
                    Sounds::page_turns();
                if (book.exit_hovered)
                {
                    book.open = 0;
                    Sounds::book_opens();
                }
            }
        }
        else
        {
            clamp_var_min(book.cooldown_after_closed -= 1, 0);
        }
    }

    { // Final message
        if (demon_queue.empty() && !demon)
            final_message_alpha += cfg->final_message_step;
    }

    { // Update listener position
        Audio::Listener::Position(p.pos.to_vec3(0) - Sound::listener_dir * Sound::listener_dist);
    }

    { // Particles
        { // Blood particles
            auto it = blood_particles.begin();
            while (it != blood_particles.end())
            {
                it->pos += it->vel;
                it->life++;

                if (it->life > it->max_life)
                    it = blood_particles.erase(it);
                else
                    it++;
            }
        }

        { // Lightning
            auto it = lightning_particles.begin();
            while (it != lightning_particles.end())
            {
                it->ticks--;

                if (it->ticks > 0)
                {
                    for (int i = 0; i < it->lines; i++)
                    {
                        Lightning::Line &line = it->line_list[i];
                        float c = (0 <= random.real() <= 1);
                        line.color = (1 - c) * it->color_a + c * it->color_b;
                        line.a = it->src_pos + it->src_ext_x * float(-1 <= random.real() <= 1) + it->src_ext_y * float(-1 <= random.real() <= 1);
                        line.c = it->dst_pos + it->dst_ext_x * float(-1 <= random.real() <= 1) + it->dst_ext_y * float(-1 <= random.real() <= 1);
                        fvec2 delta = line.c - line.a;
                        fvec2 delta_norm = delta.norm();
                        fvec2 offset_vec_x = delta_norm.rot90() * it->width / 2;
                        fvec2 offset_vec_y = delta / 2;
                        line.b = (line.a + line.c) / 2 + offset_vec_x * float(-1 <= random.real() <= 1) + offset_vec_y * float(-1 <= random.real() <= 1);
                    }
                }

                if (it->ticks <= 0)
                    it = lightning_particles.erase(it);
                else
                    it++;
            }
        }
    }
}

void States::World::Render() const
{
    { // Render blood layer (tiles and runes)
        Visuals::fbuf_blood->Bind();
        Graphics::Viewport(screen_size);
        Graphics::Clear();
        render.BindShader();

        // Background
        render.iquad(ivec2(0), screen_size).center().color(fvec3(1));

        // Blood tiles
        map.RenderBloodLayer(cam.pos, cfg->blood_color_mul);

        // Runes
        for (const Rune &rune : runes)
        {
            if ((abs(rune.pos - cam.pos) > screen_size/2 + rune.rad + Map::tile_size * 3).any())
                continue;

            float alpha = 1 - rune.remove_me_timer / float(rune.remove_me_timer_max);

            render.iquad(rune.pos - cam.pos, images.runes.Region(rune.image_pos, ivec2(84))).center().color(cfg->blood_color_mul).mix(0).alpha(alpha);

            if (rune.hovered_time > 0)
            {
                int rad = rune.rad;

                float speeds[] {-0.1, 0.07, -0.02, 0.015};

                for (float speed : speeds)
                {
                    fvec2 dir = fvec2::dir(metronome().ticks * speed);
                    float r = rad + abs(speed * 100);

                    for (int i = 0; i < 4; i++)
                    {
                        fvec2 d = dir.rot90(i);
                        render.fquad(rune.pos + d * r - cam.pos, fvec2(1 + rune.holded_time*2, r * 2)).center().matrix(fmat2(d.x, -d.y, d.y, d.x)).color(cfg->blood_color_mul).alpha(0.5 * rune.hovered_time * alpha);
                    }
                }
            }
        }

        render.Finish();
    }

    // Pre
    viewport.BeginFrame();
    Graphics::Clear();
    render.BindShader();

    { // Map (bottom)
        map.mid.Render(cam.pos);
        map.shadows.Render(cam.pos);
    }

    { // Blood (first pass)
        render.SetTexture(*Visuals::fbuf_blood_tex);
        Graphics::Blending::Func(Graphics::Blending::dst, Graphics::Blending::zero);
        render.iquad(ivec2(0), screen_size).center().tex(ivec2(0,0)).flip_y();
        render.SetTexture(main_texture);
        Graphics::Blending::FuncNormalPre();
    }

    // Blood (second pass)
    map.RenderBloodLayer(cam.pos, cfg->blood_color, cfg->blood_alpha);

    // Map (top)
    map.pre_top.Render(cam.pos);
    map.top.Render(cam.pos);

    { // Lightning
        for (const Lightning &li : lightning_particles)
        {
            for (const Lightning::Line &line : li.line_list)
            {
                render.fquad((line.a + line.b)/2 - cam.pos, fvec2((line.b - line.a).len(), li.line_width)).color(line.color).alpha(li.alpha).beta(li.beta).center().rotate((line.b - line.a).angle());
                render.fquad((line.b + line.c)/2 - cam.pos, fvec2((line.c - line.b).len(), li.line_width)).color(line.color).alpha(li.alpha).beta(li.beta).center().rotate((line.c - line.b).angle());
            }
        }
    }

    { // Bunny shadows
        constexpr ivec2 size(24);

        // Shadows
        for (const Bunny &bunny : bunnies)
        {
            if (!bunny.alive)
                continue;

            fvec2 shadow_dir = fvec2::dir(f_pi / 4);

            render.iquad(iround(bunny.pos + shadow_dir * bunny.height) - cam.pos, images.bunny.Region(ivec2(size.x * 2, 0), size)).center();
        }
    }

    // Bunnies (back)
    auto DrawBunnies = [&](int s)
    {
        constexpr ivec2 size(24);
        for (int index : bunny_indices)
        {
            const Bunny &bunny = bunnies[index];

            if (!bunny.alive)
                continue;

            if (sign((bunny.pos.y + cfg->bunny_hitbox[0].y) - (p.pos.y + cfg->plr_hitbox[0].x)) * s == -1)
                continue;

            render.iquad(iround(bunny.pos + ivec2(0, -bunny.height)) - cam.pos, images.bunny.Region(ivec2(size.x * bunny.jumps_now, 0), size)).center().flip_x(bunny.flip_x);
        }
    };
    DrawBunnies(-1);

    { // Player
        auto m = linear_mapping<float>(0, cfg->plr_death_fade_start/4, 0, 1);
        float alpha = 1;
        if (p.dead)
            alpha = smoothstep(1 - clamp(m(p.dead_for_ticks)));
        render.iquad(iround(p.pos) - cam.pos + ivec2(0,12), images.player.Region(ivec2(0, cfg->plr_sprite_size.y), cfg->plr_sprite_size)).center().alpha(alpha);
        render.iquad(iround(p.pos) - cam.pos, images.player.Region(ivec2(cfg->plr_sprite_size.x * p.anim_state,0), cfg->plr_sprite_size)).center().alpha(alpha).flip_x(p.anim_flip_x);
    }

    // Bunnies (front)
    DrawBunnies(1);

    { // Fireballs
        for (const Fireball &ball : fireballs)
            render.iquad(iround(ball.pos) - cam.pos, images.fireball).center();
    }

    { // Demon
        if (demon)
            demon->Render(*this);
    }

    { // Blood particles
        for (const BloodParticle &it : blood_particles)
        {
            float t = sqrt(1 - it.life / float(it.max_life));
            render.fquad(it.pos - cam.pos, fvec2(it.size * t)).center().matrix(fmat2(it.rot_dir, it.rot_dir.rot90())).color(cfg->blood_color * it.color).alpha(t);
        }
    }

    { // Overlay
        render.iquad(ivec2(0), images.overlay).center();
    }

    { // Demon tint
        render.iquad(ivec2(0), screen_size).color(fvec3(0)).alpha(cfg->demon_max_tint_alpha * smoothstep(demon_tint)).center();
    }

    { // Blood vial
        constexpr ivec2 vial_size(64);
        ivec2 pos = screen_size/2 - vial_size;
        float blood_frac = p.blood_amount / float(cfg->blood_capacity);
        int blood_pixels = 6 + iround(blood_frac * 42);

        // Back layer
        render.iquad(pos, images.blood_vial.Region(ivec2(vial_size.x * 2, 0), vial_size));

        // Blood
        if (p.blood_amount > 0)
        {
            int offset = iround(sin(metronome().ticks % 600 / 600.0 * f_pi * 2) * min(10, blood_pixels));
            int a = vial_size.y - blood_pixels + offset;
            int b = vial_size.y - blood_pixels - offset;
            ivec2 tex = images.blood_vial.pos + ivec2(vial_size.x, 0);

            render.itriangle(pos + ivec2(0, a), pos + ivec2(vial_size.x, b), pos + ivec2(0, vial_size.y))
                        .tex(tex + ivec2(0, a), tex + ivec2(vial_size.x, b), tex + ivec2(0, vial_size.y));
            render.itriangle(pos + vial_size  , pos + ivec2(vial_size.x, b), pos + ivec2(0, vial_size.y))
                        .tex(tex + vial_size  , tex + ivec2(vial_size.x, b), tex + ivec2(0, vial_size.y));
        }

        // Front layer
        render.iquad(pos, images.blood_vial.Region(ivec2(0), vial_size));
    }

    { // Book
        if (!book.open)
        {
            // Icon
            render.iquad(ivec2(-screen_size.x/2, screen_size.y/2) + ivec2(0, smoothstep(1-book.icon_hover_time) * cfg->book_icon_hover_offset), images.book_icon).center(ivec2(0, images.book_icon.size.y));
        }
        else
        {
            // Background
            render.iquad(ivec2(0), images.book_bg).center();

            // Text
            render.itext(cfg->book_text_offset.mul_x(-1), Graphics::Text(Fonts().main, cfg->book_pages[book.cur_page].left )).color(cfg->book_text_color);
            render.itext(cfg->book_text_offset          , Graphics::Text(Fonts().main, cfg->book_pages[book.cur_page].right)).color(cfg->book_text_color);

            // Buttons
            int arrow_offset = screen_size.x/2 - images.book_arrow.size.x/2;
            if (book.left_hovered)
                render.iquad(ivec2(-arrow_offset, 0), images.book_arrow).center().flip_x();
            if (book.right_hovered)
                render.iquad(ivec2(arrow_offset, 0), images.book_arrow).center();
        }
    }

    { // Cursor
        render.iquad(controls.mouse.pos(), images.cursor).center();
    }

    { // Death tint
        auto m = linear_mapping<float>(cfg->plr_death_fade_start, cfg->plr_death_fade_end, 0, 1);
        float alpha = smoothstep(clamp(m(p.dead_for_ticks)));
        render.iquad(ivec2(0), screen_size).color(fvec3(0)).alpha(alpha).center();
    }

    { // Death message
        if (p.dead && p.dead_for_ticks > cfg->plr_death_fade_end)
        {
            auto m = linear_mapping<float>(cfg->plr_death_fade_end, cfg->plr_death_fade_end + 60, 0, 1);
            float alpha = smoothstep(clamp(m(p.dead_for_ticks)));
            render.itext(ivec2(0, -24), Graphics::Text(Fonts().main, "YOU DIED")).color(cfg->blood_color).alpha(alpha);
            render.itext(ivec2(0, 24), Graphics::Text(Fonts().main, "Press any key to try again")).color(cfg->blood_color/2).alpha(alpha);
        }
    }

    { // Final message
        Graphics::Text text(Fonts().main, cfg->final_message);

        for (int i = 0; i < 4; i++)
        for (int j = 0; j < 2; j++)
            render.itext(ivec2(1,j).rot90(i), text).color(fvec3(0.1,0.7,0.1)).alpha(smoothstep(clamp(final_message_alpha)));

        render.itext(ivec2(0), text).color(fvec3(0)).alpha(smoothstep(clamp(final_message_alpha)));
    }

    // Post
    render.Finish();
    viewport.FinishFrame();
    Graphics::CheckErrors();
}


States::WorldState::WorldState() : world(std::make_unique<World>()) {}
States::WorldState::WorldState(WorldState &&) = default;
States::WorldState &States::WorldState::operator=(WorldState &&) = default;
States::WorldState::~WorldState() = default;
void States::WorldState::Tick()
{
    world->Tick();
}
void States::WorldState::Render() const
{
    world->Render();
}
