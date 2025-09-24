

#ifndef CompressFullRangeBlockCode_H_
#define CompressFullRangeBlockCode_H_

#include <climits>
#include <list>
#include <map>
#include <mutex>
#include <set>
#include <unordered_set>

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

class CompressFullRangeBlockCode : public SlidingCubesBlockCode {
private:
	SlidingCubesBlock *module = nullptr;
	static const int range    = 99;  // 視野範囲(自身を中心とする)

	static const int LOOK_EST       = 300;
	static const int COMPUTE_EST    = 10000;
	static const int MOVE_EST       = 1000000;
	static const int ROUND_INTERVAL = 2000000;  // 移動間隔[us]
	static const bool debug         = true;
	vector<Cell3DPosition> views;
	vector<Cell3DPosition> views_at_north;     // 自身より北にある視野
	vector<Cell3DPosition> views_at_south;     // 自身より南にある視野
	vector<Cell3DPosition> views_at_east;      // 自身より東にある視野
	vector<Cell3DPosition> views_at_west;      // 自身より西にある視野
	vector<Cell3DPosition> views_same_row;     // 自身と同じ行にある視野
	vector<Cell3DPosition> views_same_column;  // 自身と同じ列にある視野
	Cell3DPosition nextPos;
	string moving_strategy = "none";

	BlockState state                 = BlockState::INITALIZED;
	BaseSimulator::Lattice *lattice_ = nullptr;
	static string on_note;

	/**
	 * @brief
	 * 自身にオフセットを加えた座標が，最も北かつ，最も北のモジュールの中で最も西かどうかを判定する
	 *
	 * @param lattice 現在の座標系で見てているビュー
	 * @param offset オフセットの座標
	 * @return true 対象の座標が最も北かつ，最も西
	 * @return false
	 * 対象の座標が最も北でない，もしくは対象の座標が最も北であるが，自身より西にモジュールがある
	 */
	bool is_indexed_first(BaseSimulator::Lattice *lattice,
	                      Cell3DPosition offset = Cell3DPosition(0, 0, 0));

	void scheduleNextMove();

	/**
	 * @brief 停止するかの判定を行う
	 *
	 * @return true
	 * @return false
	 */
	bool determinate_terminate();

	// 自身がx軸方向の多角形の壁を構成しているかの判定を行う
	bool is_in_column_wall(Cell3DPosition offset = Cell3DPosition(0, 0, 0));

	// 自身がy軸方向の多角形の壁を構成しているかの判定を行う
	bool is_in_row_wall(Cell3DPosition offset = Cell3DPosition(0, 0, 0));

	// 自身が多角形の角を構成しているかの判定を行う(階段であるかの判定は別で行う)
	// bool is_in_angle(Cell3DPosition offset = Cell3DPosition(0, 0, 0));

	// 自身が北東にある角かの判定を行う
	bool is_in_angle_nw(Cell3DPosition offset = Cell3DPosition(0, 0, 0));

	bool is_in_angle_ne(Cell3DPosition offset = Cell3DPosition(0, 0, 0));

	bool is_in_angle_sw(Cell3DPosition offset = Cell3DPosition(0, 0, 0));

	bool is_in_angle_se(Cell3DPosition offset = Cell3DPosition(0, 0, 0));

	// 自身の左が西北の角のときに移動できるかの判定を行う
	bool can_process_phase1_nw(Cell3DPosition offset = Cell3DPosition(0, 0, 0));
	bool can_process_phase1_ne(Cell3DPosition offset = Cell3DPosition(0, 0, 0));
	bool can_process_phase1_sw(Cell3DPosition offset = Cell3DPosition(0, 0, 0));
	bool can_process_phase1_se(Cell3DPosition offset = Cell3DPosition(0, 0, 0));

	bool can_process_phase2_nw(Cell3DPosition offset = Cell3DPosition(0, 0, 0));
	bool can_process_phase2_ne(Cell3DPosition offset = Cell3DPosition(0, 0, 0));
	bool can_process_phase2_sw(Cell3DPosition offset = Cell3DPosition(0, 0, 0));
	bool can_process_phase2_se(Cell3DPosition offset = Cell3DPosition(0, 0, 0));

	// 枝を伸ばすことができるかの判定(Sw,Ne, Nwの順)
	bool can_extend_phase1_sw(Cell3DPosition offset = Cell3DPosition(0, 0, 0));
	bool can_extend_phase1_ne(Cell3DPosition offset = Cell3DPosition(0, 0, 0));
	bool can_extend_phase1_nw(Cell3DPosition offset = Cell3DPosition(0, 0, 0));

	bool can_extend_phase2_sw(Cell3DPosition offset = Cell3DPosition(0, 0, 0));
	bool can_extend_phase2_ne(Cell3DPosition offset = Cell3DPosition(0, 0, 0));
	bool can_extend_phase2_nw(Cell3DPosition offset = Cell3DPosition(0, 0, 0));

public:
	Scheduler *scheduler;

	CompressFullRangeBlockCode(SlidingCubesBlock *host);
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
	void move();
	void look();
	bool compute();
	void setColor(const Color &c);
	string onInterfaceDraw() override;

	/*****************************************************************************/
	/** needed to associate code to module **/
	static BlockCode *buildNewBlockCode(BuildingBlock *host) {
		return (new CompressFullRangeBlockCode((SlidingCubesBlock *)host));
	}
	/*****************************************************************************/
};

#endif /* CompressFullRangeBlockCode */
