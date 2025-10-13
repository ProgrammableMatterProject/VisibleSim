

#ifndef CompressFullRangeBlockCode_H_
#define CompressFullRangeBlockCode_H_

#include <climits>
#include <list>
#include <map>
#include <mutex>
#include <set>
#include <unordered_set>
#include <utility>

#include "grid/lattice.h"
#include "positionParser.hpp"
#include "robots/slidingCubes/slidingCubesBlockCode.h"
#include "robots/slidingCubes/slidingCubesSimulator.h"
#include "robots/slidingCubes/slidingCubesWorld.h"

using namespace SlidingCubes;
enum class BlockState {
	INITALIZED = 1000,
	LOOK       = 1001,
	COMPUTE    = 1002,
	MOVE       = 1003,
	MOVING     = 1005,
	TERMINATE  = 1004
};

class CompressFullRangeBlockCode : public SlidingCubes::SlidingCubesBlockCode {
private:
	SlidingCubesBlock* module = nullptr;
	static int range;  // 視野範囲(自身を中心とする)
	static bool internal_light;
	static bool external_light;

	static const int LOOK_EST       = 300;
	static const int COMPUTE_EST    = 10000;
	static const int MOVE_EST       = 1000000;
	static const int ROUND_INTERVAL = 2000000;  // 移動間隔[us]
	static const bool debug         = true;
	Lattice* views;  // 全体の観測した視野

	Cell3DPosition nextPos;                     // 移動先座標の相対座標
	Cell3DPosition module_pos;                  // モジュールの現在位置
	BlockState state = BlockState::INITALIZED;  // 現在の状態
	/**
	 * @brief 次のLCMサイクルをスケジュールする
	 *
	 */
	void scheduleNextMove();

	/**
	 * @brief 現在位置からの距離を返す
	 * @param p 距離を計算したい位置
	 * @return int
	 * 現在位置を(x1,y1,z1)、指定した場所を(x2,y2,z2)としたとき、max(|x1-x2|,|y1-y2|,|z1-z2|)を返す
	 */
	int distance(const Cell3DPosition& p) const;

	bool hasModule(const Cell3DPosition& p) const;
	bool isEmpty(const Cell3DPosition& p) const;
	Color getLight(const Cell3DPosition& p) const;
	ParserResult parseView(const PositionParser& grid, const char target_marker,
	                       const Cell3DPosition& offset);

public:
	Scheduler* scheduler;

	CompressFullRangeBlockCode(SlidingCubesBlock* host);
	~CompressFullRangeBlockCode() {};

	/**
	 * This function is called on startup of the blockCode, it can be used to
	 * perform initial configuration of the host or this instance of the
	 * program.
	 * @note this can be thought of as the main function of the module
	 */
	void startup() override;

	void onGlDraw() override;

	void processLocalEvent(std::shared_ptr<Event> pev) override;
	/**
	 * @brief 現在のビューと現在地を更新する
	 * @note 現在地はmodule_posに、ビューはviewsに保存される
	 */
	void look();

	/**
	 * @brief 次の移動先と色を計算する
	 *
	 * @return std::pair<Cell3DPosition, Color>
	 * それぞれ、次に移動する場所の相対座標と点灯する色
	 * @note 色がColor()の場合、色は変更しない
	 */
	std::pair<Cell3DPosition, Color> compute();

	/**
	 * @brief 移動を行う
	 *
	 * @return true 移動を行い、成功した
	 * @return false 移動しない、もしくは移動に失敗した
	 */
	bool move();
	/**
	 * @brief 移動終了時の処理を行う
	 *
	 */
	void onMotionEnd() override;
	/**
	 * @brief Set the Light object
	 *
	 * @param c 点灯したい色
	 */
	void setLight(const Color& c);
	string onInterfaceDraw() override;

	/*****************************************************************************/
	/** needed to associate code to module **/
	static BlockCode* buildNewBlockCode(BuildingBlock* host) {
		return (new CompressFullRangeBlockCode((SlidingCubesBlock*)host));
	}
	/*****************************************************************************/
};

#endif /* CompressFullRangeBlockCode */
