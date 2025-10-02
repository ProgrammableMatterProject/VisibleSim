#include "limited_vilabity.hpp"

#include "utils/color.h"
using namespace BaseSimulator;
LimitedVisibility::LimitedVisibility()
    : Lattice(),
      visibility_range(100),
      has_internal_light(false),
      has_external_light(false),
      center(Cell3DPosition()) {}

LimitedVisibility::LimitedVisibility(BaseSimulator::Lattice* base,
                                     Cell3DPosition& center)
    : Lattice(*base),
      visibility_range(100),
      has_internal_light(false),
      has_external_light(false),
      center(center) {}

LimitedVisibility::LimitedVisibility(BaseSimulator::Lattice* base,
                                     Cell3DPosition& center, const int range,
                                     const bool internal_light,
                                     const bool external_light)
    : Lattice(*base),
      visibility_range(range),
      has_internal_light(internal_light),
      has_external_light(external_light),
      center(center) {}

LimitedVisibility::~LimitedVisibility() { delete[] grid; }

bool LimitedVisibility::isInGrid(const Cell3DPosition& p) const {
	int dist = std::max(
	    {abs(p[0] - center[0]), abs(p[1] - center[1]), abs(p[2] - center[2])});
	return (dist <= visibility_range);
}
bool LimitedVisibility::cellHasBlock(const Cell3DPosition& p) const {
	if (!isInGrid(p)) return false;
	return (grid[getIndex(p)] != nullptr);
}
bool LimitedVisibility::isFree(const Cell3DPosition& p) const {
	if (!isInGrid(p)) return false;
	return (grid[getIndex(p)] == nullptr);
}
BuildingBlock* LimitedVisibility::getBlock(const Cell3DPosition& p) const {
	if (!isInGrid(p)) return nullptr;
	return grid[getIndex(p)];
}
Color LimitedVisibility::getColor(const Cell3DPosition& p) const {
	if (!isInGrid(p)) return BLACK;
	BuildingBlock* b = grid[getIndex(p)];
	if (b == nullptr) return BLACK;
	if (has_internal_light && p == Cell3DPosition(0, 0, 0)) return b->color;
	if (has_external_light && p != Cell3DPosition(0, 0, 0)) return b->color;
	return BLACK;
}

Vector3D LimitedVisibility::gridToUnscaledWorldPosition(
    const Cell3DPosition& pos) const {
	return Vector3D(pos[0] + center[0], pos[1] + center[1], pos[2] + center[2],
	                0);
}
Cell3DPosition LimitedVisibility::unscaledWorldToGridPosition(
    const Vector3D& pos) const {
	return Cell3DPosition(pos[0] + center[0], pos[1] + center[1],
	                      pos[2] + center[2]);
}
Cell3DPosition LimitedVisibility::worldToGridPosition(
    const Vector3D& pos) const {
	return Cell3DPosition((pos[0] + center[0]) / gridScale[0],
	                      (pos[1] + center[1]) / gridScale[1],
	                      (pos[2] + center[2]) / gridScale[2]);
}
std::vector<Cell3DPosition> LimitedVisibility::getRelativeConnectivity(
    const Cell3DPosition& p) const {
	return std::vector<Cell3DPosition>();
}
Cell3DPosition LimitedVisibility::getCellInDirection(const Cell3DPosition& pRef,
                                                     int direction) const {
	return Cell3DPosition(0, 0, 0);
}