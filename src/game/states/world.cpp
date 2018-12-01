#include "world.h"

#include <iostream>
#include <map>
#include <string>

#include "game/controls.h"
#include "game/meta.h"
#include "graphics/clear.h"
#include "graphics/errors.h"
#include "program/errors.h"
#include "utils/json.h"

//{ Images
ReflectStruct(AtlasImages,(
    (TextureAtlas::Image)(tiles, player),
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

    static ivec2 PixelPosToTile(ivec2 pos)
    {
        return div_ex(pos, tile_size);
    }

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

    bool SolidAtTile(ivec2 pos) const
    {
        int index = CollisionLayer().ClampGet(pos).index;
        return map_info.tiles[index].solid;
    }
    bool SolidAtPixel(ivec2 pos) const
    {
        return SolidAtTile(PixelPosToTile(pos));
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

ReflectStruct(ConfigType,(
    (float)(cam_mass)(=10000),
    (float)(cam_power)(=2),
    (float)(cam_drag)(=0.03),
    (fvec2)(cam_offset)(=ivec2(0,-16)),
    (ivec2)(plr_sprite_size)(=ivec2(36)),
    (int)(plr_walk_anim_frame_len)(=6),
    (int)(plr_walk_anim_frames)(=6),
    (std::vector<ivec2>)(plr_hitbox),
    (float)(plr_walk_acc, plr_walk_dec, plr_walk_speed)(=0.2),
))

Config<ConfigType> cfg("assets/config.refl");

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

    int walk_timer = 0;
    int anim_state = 0;
    bool anim_flip_x = 0;
};

using World = States::World;
struct States::World
{
    Map map;
    Player p;
    Camera cam;


    World()
    {
        UpdateEverything();
        map = Map("assets/map.json");

        p.pos = map.GetExactlyOneObject("player_spawn");
        cam.pos = cam.pos_real = p.pos;
    }

    bool SolidForPlayerAtOffset(ivec2 offset)
    {
        ivec2 pos = iround(p.pos) + offset;
        for (ivec2 point : cfg->plr_hitbox)
            if (map.SolidAtPixel(pos + point))
                return 1;
        return 0;
    }

    void Tick()
    {
        // Reload stuff if F5 is pressed.
        if (controls.debug_reload.pressed())
        {
            UpdateEverything();
            map.Reload();
            cfg.Reload();
        }

        std::cout << SolidForPlayerAtOffset(ivec2(0)) << '\n';

        { // Player
            { // Controls
                // Movement
                p.control = ivec2(controls.right.down() - controls.left.down(), controls.down.down() - controls.up.down());

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
    }

    void Render() const
    {
        { // Pre
            viewport.BeginFrame();
            Graphics::Clear();
            render.BindShader();
        }

        map.mid.Render(cam.pos);
        map.shadows.Render(cam.pos);
        map.top.Render(cam.pos);

        render.iquad(iround(p.pos) - cam.pos + ivec2(0,12), images.player.Region(ivec2(0, cfg->plr_sprite_size.y), cfg->plr_sprite_size)).center();
        render.iquad(iround(p.pos) - cam.pos, images.player.Region(ivec2(cfg->plr_sprite_size.x * p.anim_state,0), cfg->plr_sprite_size)).center().flip_x(p.anim_flip_x);

        { // Post
            render.Finish();
            viewport.FinishFrame();
            Graphics::CheckErrors();
        }
    }
};


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
