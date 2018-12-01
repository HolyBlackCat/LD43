#include "game/state.h"

#include <memory>

namespace States
{
    struct World;

    class WorldState : public State
    {
        std::unique_ptr<World> world;
      public:
        WorldState();
        WorldState(WorldState &&);
        WorldState &operator=(WorldState &&);
        ~WorldState();
        void Tick() override;
        void Render() const override;
    };
}
