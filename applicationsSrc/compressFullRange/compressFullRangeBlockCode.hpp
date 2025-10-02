

#ifndef CompressFullRangeBlockCode_H_
#define CompressFullRangeBlockCode_H_

#include <climits>
#include <list>
#include <map>
#include <mutex>
#include <set>
#include <unordered_set>
#include <utility>
#include "limited_vilabity.hpp"
#include "grid/lattice.h"
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
	SlidingCubesBlock* module        = nullptr;
	static const int range           = 99;  // 視野範囲(自身を中心とする)
	static const bool internal_light = false;
	static const bool external_light = false;

	static const int LOOK_EST       = 300;
	static const int COMPUTE_EST    = 10000;
	static const int MOVE_EST       = 1000000;
	static const int ROUND_INTERVAL = 2000000;  // 移動間隔[us]
	static const bool debug         = true;
	LimitedVisibility *views;//全体の観測した視野

	Cell3DPosition nextPos;  // 移動先座標の相対座標
	Cell3DPosition module_pos;//モジュールの現在位置
	BlockState state = BlockState::INITALIZED;
	void scheduleNextMove();

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
	void onMotionEnd() override;
	void processLocalEvent(std::shared_ptr<Event> pev) override;
	bool move(); //移動を行う、移動が成功したならtrueを返す 
	void
	look();  // 現在のviewを返す-->返した型で視野の範囲外のアクセスを定義外とする
	std::pair<Cell3DPosition, Color> compute();// 次に移動する場所の相対座標と色を返すようにする
	void setColor(const Color& c);
	string onInterfaceDraw() override;

	/*****************************************************************************/
	/** needed to associate code to module **/
	static BlockCode* buildNewBlockCode(BuildingBlock* host) {
		return (new CompressFullRangeBlockCode((SlidingCubesBlock*)host));
	}
	/*****************************************************************************/
};

#endif /* CompressFullRangeBlockCode */
