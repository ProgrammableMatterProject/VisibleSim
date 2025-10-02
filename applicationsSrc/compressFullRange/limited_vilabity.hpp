#ifndef LIMITED_VISIBILITY_HPP
#define LIMITED_VISIBILITY_HPP
#include "grid/lattice.h"
using namespace BaseSimulator;

class LimitedVisibility : public BaseSimulator::Lattice {
protected:
	const int visibility_range;
	const bool has_internal_light;
	const bool has_external_light;
	Cell3DPosition center;

public:
	LimitedVisibility();
	LimitedVisibility(BaseSimulator::Lattice* base, Cell3DPosition& center);
	LimitedVisibility(BaseSimulator::Lattice* base, Cell3DPosition& center,
	                  const int range, const bool internal_light,
	                  const bool external_light);

	~LimitedVisibility() override;
	/**
	 * @brief 基準位置からの相対位置pが視野内にあるかどうかを返す
	 *
	 * @param p
	 * @return true pが視野内にある
	 * @return false pが視野外にある
	 */
	bool isInGrid(const Cell3DPosition& p) const override;
	/**
	 * @brief 指定した位置pにブロックが存在するかどうかを返す
	 *
	 * @param p 基準位置からのブロックの相対位置
	 * @return true 指定した位置にブロックが存在する
	 * @return false 指定した位置にブロックが存在しない、もしくは視野外である
	 */
	bool cellHasBlock(const Cell3DPosition& p) const;

	/**
	 * @brief 指定した位置pにブロックが存在しないかどうかを返す
	 *
	 * @param p 基準位置からのブロックの相対位置
	 * @return true 指定した位置にブロックが存在しない
	 * @return false 指定した位置にブロックが存在する、もしくは視野外である
	 */
	bool isFree(const Cell3DPosition& p) const;
	/**
	 * @brief 指定した位置pに存在するブロックを返す
	 *
	 * @param p 基準位置からのブロックの相対位置
	 * @return
	 * その位置に存在するブロックへのポインタ、もしくは視野外である場合はnullptr
	 */
	BuildingBlock* getBlock(const Cell3DPosition& p) const;

	/**
	 * @brief 指定した位置pに存在するモジュールの色を返す
	 *
	 * @param p 基準位置からのブロックの相対位置
	 * @return Color
	 * 指定した位置pに存在するモジュールの色、ただし、色が取得できない場合はBLACKを返す
	 */
	Color getColor(const Cell3DPosition& p) const;

	Vector3D gridToUnscaledWorldPosition(
	    const Cell3DPosition& pos) const override;
	Cell3DPosition unscaledWorldToGridPosition(
	    const Vector3D& pos) const override;
	Cell3DPosition worldToGridPosition(const Vector3D& pos) const override;
	std::vector<Cell3DPosition> getRelativeConnectivity(
	    const Cell3DPosition& p) const override;
	inline const int getMaxNumNeighbors() const override {
		return MAX_NB_NEIGHBORS;
	}
	Cell3DPosition getCellInDirection(const Cell3DPosition& pRef,
	                                  int direction) const override;
};
#endif  // LIMITED_VISIBILITY_HPP
