#include "compressFullRangeBlockCode.hpp"

#include <bitset>
#include <mutex>
#include <vector>

#include "lcmEvent.hpp"
string CompressFullRangeBlockCode::on_note = "not started";
Color directionModulate(const Color& base, const string direction) {
	switch (direction[0]) {
		case 'N':
			return Color(base[0], base[1], base[2]);
		case 'E':
			return Color(base[0] >> 1, base[1], base[2]);
		case 'S':
			return Color(base[0], base[1] >> 1, base[2]);
		case 'W':
			return Color(base[0] >> 1, base[1] >> 1, base[2]);
		default:
			return base;
	}
}

CompressFullRangeBlockCode::CompressFullRangeBlockCode(SlidingCubesBlock* host)
    : SlidingCubesBlockCode(host), module(host) {
	scheduler = getScheduler();
	if (not host) return;
	int range_dist = range >> 1;
	for (int dx = -range_dist; dx <= range_dist; ++dx)
		for (int dy = -range_dist; dy <= range_dist; ++dy) {
			if (dx == 0 && dy == 0) continue;
			views.push_back(Cell3DPosition(dx, dy, 0));
			if (dy > 0) views_at_north.push_back(Cell3DPosition(dx, dy, 0));
			if (dy < 0) views_at_south.push_back(Cell3DPosition(dx, dy, 0));
			if (dy == 0) views_same_row.push_back(Cell3DPosition(dx, dy, 0));
			if (dx < 0) views_at_west.push_back(Cell3DPosition(dx, dy, 0));
			if (dx > 0) views_at_east.push_back(Cell3DPosition(dx, dy, 0));
			if (dx == 0) views_same_column.push_back(Cell3DPosition(dx, dy, 0));
		}
	state = BlockState::LOOK;
}

bool CompressFullRangeBlockCode::is_indexed_first(
    BaseSimulator::Lattice* lattice, Cell3DPosition offset) {
	Cell3DPosition myPos = module->position + offset;
	// 対象の座標にモジュールが存在しない，もしくはモジュールが存在してもその下にモジュールが存在しない場合はfalse
	if (!lattice->cellHasBlock(myPos) or
	    !lattice->cellHasBlock(myPos.offsetY(-1))) {
		return false;
	}
	// 自身とその上と右上にモジュールが存在する場合はfalse
	if (lattice->cellHasBlock(myPos) and
	    lattice->cellHasBlock(myPos.offsetY(1)) and
	    lattice->cellHasBlock(myPos + Cell3DPosition(1, 1, 0)))
		return false;
	// 自身の右隣と下と右下にモジュールが存在する場合はfalse
	if (lattice->cellHasBlock(myPos.offsetX(1)) and
	    lattice->cellHasBlock(myPos.offsetY(-1)) and
	    lattice->cellHasBlock(myPos.offsetX(1).offsetY(-1))) {
		return false;
	}

	// 自身より北にあるモジュール(a)について，両隣にモジュールがあるか，両隣にモジュールがない場合はfalse
	for (const auto& view : views_at_north) {
		if (lattice->cellHasBlock(myPos + view) and
		    not(lattice->cellHasBlock(myPos + view +
		                              Cell3DPosition(1, 0, 0)) xor
		        lattice->cellHasBlock(myPos + view +
		                              Cell3DPosition(-1, 0, 0)))) {
			return false;
		}
	}

	vector<Cell3DPosition> same_row_west;
	set_intersection(views_same_row.begin(), views_same_row.end(),
	                 views_at_west.begin(), views_at_west.end(),
	                 back_inserter(same_row_west));
	// 自身が最も西かを判定する

	// 自身と同じy座標で，自身より左にあるモジュールがいればfalse
	for (auto& view : same_row_west) {
		if (lattice->cellHasBlock(myPos + view)) return false;
	}
	return true;
}

bool CompressFullRangeBlockCode::determinate_terminate() {
	// 視野に見えたモジュールが自身の周辺に連続して3つあるかを判定する
	Cell3DPosition myPos = module->position;
	for (const auto& view : views) {
		if (!lattice_->cellHasBlock(myPos + view)) continue;
		if (not((lattice_->cellHasBlock(myPos + view +
		                                Cell3DPosition(1, 0, 0)) and
		         lattice_->cellHasBlock(myPos + view +
		                                Cell3DPosition(1, 1, 0)) and
		         lattice_->cellHasBlock(myPos + view +
		                                Cell3DPosition(0, 1, 0))) or
		        (lattice_->cellHasBlock(myPos + view +
		                                Cell3DPosition(-1, 0, 0)) and
		         lattice_->cellHasBlock(myPos + view +
		                                Cell3DPosition(-1, 1, 0)) and
		         lattice_->cellHasBlock(myPos + view +
		                                Cell3DPosition(0, 1, 0))) or
		        (lattice_->cellHasBlock(myPos + view +
		                                Cell3DPosition(1, 0, 0)) and
		         lattice_->cellHasBlock(myPos + view +
		                                Cell3DPosition(1, -1, 0)) and
		         lattice_->cellHasBlock(myPos + view +
		                                Cell3DPosition(0, -1, 0))) or
		        (lattice_->cellHasBlock(myPos + view +
		                                Cell3DPosition(-1, 0, 0)) and
		         lattice_->cellHasBlock(myPos + view +
		                                Cell3DPosition(-1, -1, 0)) and
		         lattice_->cellHasBlock(myPos + view +
		                                Cell3DPosition(0, -1, 0))))) {
			return false;
		}
	}
	// 自身に対しても確認
	if (not((lattice_->cellHasBlock(myPos + Cell3DPosition(1, 0, 0)) and
	         lattice_->cellHasBlock(myPos + Cell3DPosition(1, 1, 0)) and
	         lattice_->cellHasBlock(myPos + Cell3DPosition(0, 1, 0))) or
	        (lattice_->cellHasBlock(myPos + Cell3DPosition(-1, 0, 0)) and
	         lattice_->cellHasBlock(myPos + Cell3DPosition(-1, 1, 0)) and
	         lattice_->cellHasBlock(myPos + Cell3DPosition(0, 1, 0))) or
	        (lattice_->cellHasBlock(myPos + Cell3DPosition(1, 0, 0)) and
	         lattice_->cellHasBlock(myPos + Cell3DPosition(1, -1, 0)) and
	         lattice_->cellHasBlock(myPos + Cell3DPosition(0, -1, 0))) or
	        (lattice_->cellHasBlock(myPos + Cell3DPosition(-1, 0, 0)) and
	         lattice_->cellHasBlock(myPos + Cell3DPosition(-1, -1, 0)) and
	         lattice_->cellHasBlock(myPos + Cell3DPosition(0, -1, 0))))) {
		return false;
	}
	cerr << "terminate id:" << module->blockId << "\n";
	setColor(GREY);
	state = BlockState::TERMINATE;
	return true;
}
bool CompressFullRangeBlockCode::is_in_column_wall(Cell3DPosition offset) {
	// 自身が横方向の壁になっているなら、その左右と自身にはモジュールがあるはず
	Cell3DPosition myPos = module->position + offset;
	if (not(lattice_->cellHasBlock(myPos) and
	        lattice_->cellHasBlock(myPos.offsetX(-1)) and
	        lattice_->cellHasBlock(myPos.offsetX(1))))
		return false;
	// 自身の南にモジュールが無いならば自身の北の3方向にあるモジュールは2未満のはず
	if (not(lattice_->cellHasBlock(myPos.offsetY(-1))) and
	    not(lattice_->cellHasBlock(myPos.offsetY(1)) and
	        lattice_->cellHasBlock(myPos.offsetY(1).offsetX(1)) and
	        lattice_->cellHasBlock(myPos.offsetY(1).offsetX(-1)))) {
		return true;
	}
	if (not(lattice_->cellHasBlock(myPos.offsetY(1))) and
	    not(lattice_->cellHasBlock(myPos.offsetY(-1)) and
	        lattice_->cellHasBlock(myPos.offsetY(-1).offsetX(1)) and
	        lattice_->cellHasBlock(myPos.offsetY(-1).offsetX(-1)))) {
		return true;
	}
	return false;
}

bool CompressFullRangeBlockCode::is_in_row_wall(Cell3DPosition offset) {
	// 自身が横方向の壁になっているなら、その上下と自身にはモジュールがあるはず
	Cell3DPosition myPos = module->position + offset;
	if (not(lattice_->cellHasBlock(myPos) and
	        lattice_->cellHasBlock(myPos.offsetY(-1)) and
	        lattice_->cellHasBlock(myPos.offsetY(1))))
		return false;
	// 自身の西にモジュールが無いならば自身の東の3方向にあるモジュールは2未満のはず
	if (not(lattice_->cellHasBlock(myPos.offsetX(-1))) and
	    not(lattice_->cellHasBlock(myPos.offsetX(1)) and
	        lattice_->cellHasBlock(myPos.offsetX(1).offsetY(1)) and
	        lattice_->cellHasBlock(myPos.offsetX(1).offsetY(-1)))) {
		return true;
	}
	if (not(lattice_->cellHasBlock(myPos.offsetX(1))) and
	    not(lattice_->cellHasBlock(myPos.offsetX(-1)) and
	        lattice_->cellHasBlock(myPos.offsetX(-1).offsetY(1)) and
	        lattice_->cellHasBlock(myPos.offsetX(-1).offsetY(-1)))) {
		return true;
	}
	return false;
}

bool CompressFullRangeBlockCode::is_in_angle_nw(Cell3DPosition offset) {
	Cell3DPosition myPos = module->position + offset;
	if (not lattice_->cellHasBlock(myPos)) {
		return false;
	}
	// 北西
	if (/*is_in_column_wall(offset.offsetX(1))*/ lattice_->cellHasBlock(
	        myPos.offsetX(1)) and
	    /* is_in_row_wall(offset.offsetY(-1))*/
	    lattice_->cellHasBlock(myPos.offsetY(-1)) and
	    lattice_->isFree(myPos.offsetX(1).offsetY(-1))) {
		return true;
	}
	return false;
}
bool CompressFullRangeBlockCode::is_in_angle_ne(Cell3DPosition offset) {
	Cell3DPosition myPos = module->position + offset;
	if (not lattice_->cellHasBlock(myPos)) {
		return false;
	}
	// 北東
	if (/*is_in_column_wall(offset.offsetX(-1))*/ lattice_->cellHasBlock(
	        myPos.offsetX(-1)) and
	    /* is_in_row_wall(offset.offsetY(-1))*/
	    lattice_->cellHasBlock(myPos.offsetY(-1)) and
	    lattice_->isFree(myPos.offsetX(-1).offsetY(-1))) {
		return true;
	}
	return false;
}
bool CompressFullRangeBlockCode::is_in_angle_sw(Cell3DPosition offset) {
	Cell3DPosition myPos = module->position + offset;
	if (not lattice_->cellHasBlock(myPos)) {
		return false;
	}
	// 南西
	if (/*is_in_column_wall(offset.offsetX(1))*/ lattice_->cellHasBlock(
	        myPos.offsetX(1)) and
	    /* is_in_row_wall(offset.offsetY(1))*/
	    lattice_->cellHasBlock(myPos.offsetY(1)) and
	    lattice_->isFree(myPos.offsetX(1).offsetY(1))) {
		return true;
	}
	return false;
}
bool CompressFullRangeBlockCode::is_in_angle_se(Cell3DPosition offset) {
	Cell3DPosition myPos = module->position + offset;
	if (not lattice_->cellHasBlock(myPos)) {
		return false;
	}
	// 南東
	if (/*is_in_column_wall(offset.offsetX(-1))*/ lattice_->cellHasBlock(
	        myPos.offsetX(-1)) and
	    /* is_in_row_wall(offset.offsetY(1))*/
	    lattice_->cellHasBlock(myPos.offsetY(1)) and
	    lattice_->isFree(myPos.offsetX(-1).offsetY(1))) {
		return true;
	}
	return false;
}

bool CompressFullRangeBlockCode::can_process_phase1_nw(Cell3DPosition offset) {
	Cell3DPosition myPos = module->position + offset;

	if (!is_in_angle_nw(offset.offsetX(-1))) {  // 自身に左がNwの角
		return false;
	}
	if ((lattice_->cellHasBlock(myPos.offsetX(1).offsetY(1)) or
	     not lattice_->cellHasBlock(myPos.offsetY(
	         1)))  // 自身の上にモジュールがある時、自身の右上にもモジュールがある
	    and
	    (not(
	        lattice_->cellHasBlock(myPos.offsetX(-2)) and
	        lattice_->cellHasBlock(myPos.offsetX(-2).offsetY(
	            -1))))  // 自身の2つ左と2つ左、1つ下の両方にモジュールがあっては行けない
	) {
		return true;
	}
	return false;
	/*

	if (lattice_->isFree(myPos.offsetX(1)) and  // 自身より右は空き
	    lattice_->cellHasBlock(
	        myPos.offsetY(-1)) and  // 自身の下はブロックがある
	    lattice_->cellHasBlock(
	        myPos.offsetX(1).offsetY(-1)) and  // 自身の右下にブロックがある
	    lattice_->cellHasBlock(
	        myPos.offsetX(2))  // 自身の右から2つめにブロックがある
	    and
	    (not lattice_->cellHasBlock(myPos.offsetX(1).offsetY(1)) or
	     lattice_->cellHasBlock(myPos.offsetX(2).offsetY(
	         1)))  //
	自身右上にブロックがあるなら自身の右から2つめ、1つ上にブロックがある　 and
	    (not lattice_->cellHasBlock(myPos.offsetY(1)) or
	     lattice_->cellHasBlock(myPos.offsetX(1).offsetY(
	         1)))  // 自身の上にブロックがあるなら、自身の右上にブロックがある
	    and (not lattice_->cellHasBlock(
	            myPos.offsetX(-1))))  // 自身の左側にブロックはない
	    return true;
	else
	    return false;
	*/
}

bool CompressFullRangeBlockCode::can_process_phase1_ne(Cell3DPosition offset) {
	Cell3DPosition myPos = module->position + offset;

	if (!is_in_angle_ne(offset.offsetY(1))) {
		return false;
	}
	if ((not lattice_->cellHasBlock(myPos.offsetX(1).offsetY(-1)) and
	     lattice_->cellHasBlock(myPos.offsetX(1)))) {
		return true;
	}
	return false;
}

bool CompressFullRangeBlockCode::can_process_phase1_sw(Cell3DPosition offset) {
	Cell3DPosition myPos = module->position + offset;

	if (!is_in_angle_sw(offset.offsetY(-1))) {
		return false;
	}
	if ((not lattice_->cellHasBlock(myPos.offsetX(-1).offsetY(1)) and
	     lattice_->cellHasBlock(myPos.offsetX(-1)))) {
		return false;
	}
	return true;
}

bool CompressFullRangeBlockCode::can_process_phase1_se(Cell3DPosition offset) {
	Cell3DPosition myPos = module->position + offset;

	if (!is_in_angle_se(offset.offsetX(1))) {
		return false;
	}
	if ((not lattice_->cellHasBlock(myPos.offsetX(-1).offsetY(-1)) and
	     lattice_->cellHasBlock(myPos.offsetY(-1)))) {
		return false;
	}
	return true;
}

void CompressFullRangeBlockCode::scheduleNextMove() {
	if (state == BlockState::TERMINATE) return;
	onMotionEnd();

	Time currentTime = scheduler->now();
	getScheduler()->schedule(
	    new MoveEvent(currentTime + ROUND_INTERVAL - MOVE_EST, module));
	getScheduler()->schedule(new ComputeEvent(
	    currentTime + ROUND_INTERVAL - COMPUTE_EST - MOVE_EST, module));
	getScheduler()->schedule(new LookEvent(
	    currentTime + ROUND_INTERVAL - LOOK_EST - COMPUTE_EST - MOVE_EST,
	    module));
	getScheduler()->schedule(
	    new InterruptionEvent(currentTime + ROUND_INTERVAL, module, 0));
}

void CompressFullRangeBlockCode::startup() {
	console << "start " << module->blockId << "\n";
	SlidingCubesWorld* wrl = SlidingCubes::getWorld();

	nextPos = module->position;
	scheduleNextMove();
}

bool CompressFullRangeBlockCode::compute() {
	// nextPos = nullptr;

	//  自身が最も北かを判定する

	if (state != BlockState::COMPUTE) return false;

	//	cerr << "move:" << std::setw(2) << module->blockId << " @"
	//	     << module->position << " | " << getColor() << endl;

	setColor(BLUE);
	if (debug and false) {
		if (is_in_angle_nw()) {
			setColor(directionModulate(ORANGE, "N"));
		} else if (is_in_angle_ne()) {
			setColor(directionModulate(ORANGE, "E"));
		} else if (is_in_angle_se()) {
			setColor(directionModulate(ORANGE, "S"));
		} else if (is_in_angle_sw()) {
			setColor(directionModulate(ORANGE, "W"));
		} else if (is_in_column_wall()) {
			setColor(GREEN);
		} else if (is_in_row_wall()) {
			setColor(DARKGREEN);
		}
	}
	// move_lock_ = unique_lock<mutex>(self_mtx_, defer_lock);
	Cell3DPosition myPos = module->position;

	// 前提:他のモジュールがphase2の操作は行っていない
	bool can_process_phase1   = true;
	string compress_candidate = "null";
	// int compress_candidate_flag = 0;
	//  圧縮可能な角の候補を探す

	int can_process_flag         = 0;  // 可能ならば1
	int flagged_before_mine_flag = 0;  // 他にないなら1
	int flagged_all_flag         = 0;  // 他にないなら1
	enum strategy_wait {
		p2_nw,
		extend_p2_sw,
		extend_p1_sw,
		extend_p2_ne,
		extend_p1_ne,

		p1_nw,
		extend_p2_nw,
		extend_p1_nw,
		p2_sw,
		p1_sw,
		p2_ne,
		p1_ne,
		p2_se,
		p1_se,
		STRATEGY_WAIT_SIZE
	};
	for (const auto& view : views) {
		////視野に対する各フェーズの処理可能かの判定
		flagged_all_flag |= int(can_process_phase1_nw(view)) << p1_nw;
		flagged_all_flag |= int(can_process_phase1_ne(view)) << p1_ne;
		flagged_all_flag |= int(can_process_phase1_se(view)) << p1_se;
		flagged_all_flag |= int(can_process_phase1_sw(view)) << p1_sw;

		flagged_all_flag |= int(can_process_phase2_nw(view)) << p2_nw;
		flagged_all_flag |= int(can_process_phase2_ne(view)) << p2_ne;
		flagged_all_flag |= int(can_process_phase2_se(view)) << p2_se;
		flagged_all_flag |= int(can_process_phase2_sw(view)) << p2_sw;

		flagged_all_flag |= int(can_extend_phase1_nw(view)) << extend_p1_nw;
		flagged_all_flag |= int(can_extend_phase1_ne(view)) << extend_p1_ne;
		// flagged_all_flag |= int(can_extend_phase1_se(view)) << 7;
		flagged_all_flag |= int(can_extend_phase1_sw(view)) << extend_p1_sw;

		flagged_all_flag |= int(can_extend_phase2_nw(view)) << extend_p2_nw;
		flagged_all_flag |= int(can_extend_phase2_ne(view)) << extend_p2_ne;
		// flagged_all_flag |= int(can_extend_phase2_se(view)) << 6
		flagged_all_flag |= int(can_extend_phase2_sw(view)) << extend_p2_sw;

		if (view[1] > 0 or ((view[1] == 0) and view[0] < 0)) {
			flagged_before_mine_flag |= int(can_process_phase1_nw(view))
			                            << p1_nw;
			flagged_before_mine_flag |= int(can_process_phase2_nw(view))
			                            << p2_nw;

			flagged_before_mine_flag |= int(can_extend_phase1_nw(view))
			                            << extend_p1_nw;
			flagged_before_mine_flag |= int(can_extend_phase2_nw(view))
			                            << extend_p2_nw;
		}
		if (view[0] > 0 or ((view[0] == 0) and view[1] > 0)) {
			flagged_before_mine_flag |= int(can_process_phase1_ne(view))
			                            << p1_ne;
			flagged_before_mine_flag |= int(can_process_phase2_ne(view))
			                            << p2_ne;

			flagged_before_mine_flag |= int(can_extend_phase1_ne(view))
			                            << extend_p1_ne;
			flagged_before_mine_flag |= int(can_extend_phase2_ne(view))
			                            << extend_p2_ne;
		}
		if (view[1] < 0 or ((view[1] == 0) and view[0] > 0)) {
			flagged_before_mine_flag |= int(can_process_phase1_se(view))
			                            << p1_se;
			flagged_before_mine_flag |= int(can_process_phase2_se(view))
			                            << p2_se;
		}
		if (view[0] < 0 or ((view[0] == 0) and view[1] < 0)) {
			flagged_before_mine_flag |= int(can_process_phase1_sw(view))
			                            << p1_sw;
			flagged_before_mine_flag |= int(can_process_phase2_sw(view))
			                            << p2_sw;

			flagged_before_mine_flag |= int(can_extend_phase1_sw(view))
			                            << extend_p1_sw;
			flagged_before_mine_flag |= int(can_extend_phase2_sw(view))
			                            << extend_p2_sw;
		}
	}
	can_process_flag |= int(can_process_phase1_nw()) << p1_nw;  // 0
	can_process_flag |= int(can_process_phase1_ne()) << p1_ne;  // 1
	can_process_flag |= int(can_process_phase1_se()) << p1_se;  // 2
	can_process_flag |= int(can_process_phase1_sw()) << p1_sw;  // 3

	can_process_flag |= int(can_process_phase2_nw()) << p2_nw;  // 4
	can_process_flag |= int(can_process_phase2_ne()) << p2_ne;  // 5
	can_process_flag |= int(can_process_phase2_se()) << p2_se;  // 6
	can_process_flag |= int(can_process_phase2_sw()) << p2_sw;  // 7

	can_process_flag |= int(can_extend_phase1_nw()) << extend_p1_nw;  // 8
	can_process_flag |= int(can_extend_phase1_ne()) << extend_p1_ne;  // 9
	// can_process_flag |= int(can_extend_phase1_se()) << extend_p1_se;  //
	can_process_flag |= int(can_extend_phase1_sw()) << extend_p1_sw;  // 10

	can_process_flag |= int(can_extend_phase2_nw()) << extend_p2_nw;  // 11
	can_process_flag |= int(can_extend_phase2_ne()) << extend_p2_ne;  // 12
	// can_process_flag |= int(can_extend_phase2_se()) << extend_p2_se;  //
	can_process_flag |= int(can_extend_phase2_sw()) << extend_p2_sw;  // 13

	flagged_all_flag <<= 1;
	if (debug and false) {
		clog << "id : " << module->blockId << "\n";
		clog << "flagged_all_flag:        "
		     << bitset<STRATEGY_WAIT_SIZE>(flagged_all_flag) << "\n";
		clog << "flagged_before_mine_flag:"
		     << bitset<STRATEGY_WAIT_SIZE>(flagged_before_mine_flag) << "\n";
		clog << "can_process_flag:        "
		     << bitset<STRATEGY_WAIT_SIZE>(can_process_flag) << "\n";
		// clog << "compress_candidate_flag: "
		//      << bitset<8>(compress_candidate_flag) << "\n";
	}

	for (int i = 0; i < STRATEGY_WAIT_SIZE; ++i) {
		flagged_all_flag |= flagged_all_flag << 1;
	}
	flagged_all_flag         = ~flagged_all_flag;
	flagged_before_mine_flag = ~flagged_before_mine_flag;

	// 自身での圧縮可能化の判定

	can_process_flag &= flagged_all_flag;
	can_process_flag &= flagged_before_mine_flag;
	// can_process_flag &= compress_candidate_flag;
	if (0) {
	} else if (can_process_flag & (1 << p2_nw)) {
		nextPos = myPos.offsetX(1);
		on_note = moving_strategy = "compress R-angle phase2";  // "NW-phase2";
		setColor(directionModulate(RED, "E"));
	} else if (can_process_flag & (1 << extend_p2_sw)) {
		nextPos = myPos.offsetY(1);
		on_note = moving_strategy =
		    "expand L-pocket phase2";  // "SWextend-phase2";
		setColor(directionModulate(PURPLE, "N"));
	} else if (can_process_flag & (1 << extend_p1_sw)) {
		nextPos = myPos.offsetX(1);
		on_note = moving_strategy =
		    "expand L-pocket phase1";  //"SWextend-phase1";
		setColor(directionModulate(PURPLE, "E"));
	} else if (can_process_flag & (1 << extend_p2_ne)) {
		nextPos = myPos.offsetX(-1);
		on_note = moving_strategy =
		    "expand mirrored-R-pocket phase2";  // "NEextend-phase2";
		setColor(directionModulate(PURPLE, "W"));
	} else if (can_process_flag & (1 << extend_p1_ne)) {
		nextPos = myPos.offsetY(-1);
		on_note = moving_strategy =
		    "expand mirrored-R-pocket phase1";  //"NEextend-phase1";
		setColor(directionModulate(PURPLE, "S"));
	} else if (can_process_flag & (1 << p1_nw)) {
		nextPos = myPos.offsetY(-1);
		on_note = moving_strategy = "compress R-angle phase1";  //"NW-phase1";
		setColor(directionModulate(RED, "S"));
	} else if (can_process_flag & (1 << extend_p2_nw)) {
		nextPos = myPos.offsetY(-1);
		on_note = moving_strategy =
		    "expand R-pocket phase2";  //"NWextend-phase2";
		setColor(directionModulate(PURPLE, "S"));
	} else if (can_process_flag & (1 << extend_p1_nw)) {
		nextPos = myPos.offsetX(1);
		on_note = moving_strategy =
		    "expand R-pocket phase1";  // "NWextend-phase1";
		setColor(directionModulate(PURPLE, "E"));
	} else if (can_process_flag & (1 << p2_sw)) {
		nextPos = myPos.offsetY(1);
		on_note = moving_strategy = "SW-phase2";
		setColor(directionModulate(RED, "N"));
		assert(false);
	} else if (can_process_flag & (1 << p1_sw)) {
		nextPos = myPos.offsetX(1);
		on_note = moving_strategy = "SW-phase1";
		setColor(directionModulate(RED, "E"));
		assert(false);
	} else if (can_process_flag & (1 << p2_ne)) {
		nextPos = myPos.offsetY(-1);
		on_note = moving_strategy = "NE-phase2";
		setColor(directionModulate(RED, "S"));
		assert(false);
	} else if (can_process_flag & (1 << p1_ne)) {
		nextPos = myPos.offsetX(1);
		on_note = moving_strategy = "NE-phase1";
		setColor(directionModulate(RED, "E"));
		assert(false);
	}
	if (moving_strategy != "" and moving_strategy != "none") {
		console << "strategy: " << moving_strategy << "\n";
	}

	if (moving_strategy != "" and moving_strategy != "none" and debug) {
		// cerr << "strategy: " << moving_strategy << "\n";
	}

	// 停止するかの判定
	if (determinate_terminate()) {
		on_note = moving_strategy = "done";
		return true;
	}
	state = BlockState::MOVE;

	// cerr << "exit : " << module->blockId << "\n";
	return true;
}

void CompressFullRangeBlockCode::onGlDraw() {
	static const float thick    = 0.8;
	static const float color[4] = {2.2f, 0.2f, 0.2f, 1.0f};
	const Cell3DPosition& gs    = lattice->gridSize;
	const Vector3D gl           = lattice->gridScale;
	glDisable(GL_TEXTURE);
	glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, color);
	glPushMatrix();
	glNormal3f(0, 0, 1);
	glScalef(gl[0], gl[1], gl[2]);
	glBegin(GL_QUAD_STRIP);
	for (int i = 0; i <= 36; i++) {
		double cs = 0.5 * cos(i * M_PI / 18);
		double ss = 0.5 * sin(i * M_PI / 18);
		glVertex3f(thick * cs, thick * ss, 0);
		glVertex3f(cs, ss, 0);
	}
	glEnd();

	glPopMatrix();
}

void CompressFullRangeBlockCode::onMotionEnd() {
	// scheduleNextMove();
	// cerr << "Motion ended for block " << module->blockId << endl;
	// move_lock_.unlock();
	// if(state != BlockState::MOVING) return;
	nextPos         = module->position;
	moving_strategy = "none";
	state           = BlockState::LOOK;
}

void CompressFullRangeBlockCode::move() {
	//	cerr << "move() called id:" << module->blockId << " time=" <<
	// scheduler->now() << "\n";
	if (state != BlockState::MOVE) return;
	// state = BlockState::MOVING;
	if (nextPos == module->position) {
		return;
	}

	if (module->canMoveTo(nextPos)) {
		cerr<< "strategy:" << moving_strategy << " \n";
		cerr << "id:" << module->blockId << " ";
		if (module->moveTo(nextPos)) {
			cerr << "moving " << nextPos << "\n";

		} else {
			cerr << "failed to move to " << nextPos << "\n";

			return;
		}

	} else {
		cerr << "move id: " << module->blockId << " : can't move \n";

		return;
	}
}

void CompressFullRangeBlockCode::processLocalEvent(std::shared_ptr<Event> pev) {
	// cerr << "event by id " << module->blockId << " type=" <<
	// pev->eventType
	//      << " date=" << pev->date << " / now=" << scheduler->now() << "@
	//      "
	//      << module->position << "\n";
	switch (pev->eventType) {
		case EVENT_MOVE:
			move();
			break;
		case EVENT_LOOK:
			look();
			break;
		case EVENT_COMPUTE:
			compute();
			break;
		case EVENT_TELEPORTATION_END:
		case EVENT_TELEPORTATION_STOP:
			onMotionEnd();
			break;
		case EVENT_INTERRUPTION:
			scheduleNextMove();
			break;
		case EVENT_ADD_NEIGHBOR:
			break;
		case EVENT_REMOVE_NEIGHBOR:
			break;
		default:
			cerr << "unknown event type " << pev->eventType << "\n";
			break;
	}
}
void CompressFullRangeBlockCode::look() {
	if (state != BlockState::LOOK) return;
	lattice_ = SlidingCubesWorld::getWorld()->lattice;
	state    = BlockState::COMPUTE;
}
string CompressFullRangeBlockCode::onInterfaceDraw() {
	string res = "strategy: " + on_note;
	return res;
}
void CompressFullRangeBlockCode::setColor(const Color& c) {
	if (!debug) return;
	return SlidingCubesBlockCode::setColor(c);
}
/*

大枠の処理の流れ
方針:モジュールを東、南に集める
枝が途中で決まったっら?-->枝ごと動かす?圧縮方向を変える?
- leaderをもとにした払い出し(南移動と東移動の繰り返し)
- 枝の処理
-
重要なのは根幹の枝は横方向（一番下？）、それらの橋から垂直に伸ばせることが重要?
*/

bool CompressFullRangeBlockCode::can_process_phase2_nw(Cell3DPosition offset) {
	Cell3DPosition myPos = module->position + offset;
	if (!lattice_->cellHasBlock(myPos)) return false;
	if (lattice_->isFree(myPos.offsetX(1)) and  // 自身より右は空き
	    lattice_->cellHasBlock(
	        myPos.offsetY(-1)) and  // 自身の下はブロックがある
	    lattice_->cellHasBlock(
	        myPos.offsetX(1).offsetY(-1)) and  // 自身の右下にブロックがある
	    lattice_->cellHasBlock(
	        myPos.offsetX(2))  // 自身の右から2つめにブロックがある
	    and
	    (not lattice_->cellHasBlock(myPos.offsetX(1).offsetY(1)) or
	     lattice_->cellHasBlock(myPos.offsetX(2).offsetY(
	         1)))  // 自身右上にブロックがあるなら自身の右から2つめ、1つ上にブロックがある　
	    and
	    (not lattice_->cellHasBlock(myPos.offsetY(1)) or
	     lattice_->cellHasBlock(myPos.offsetX(1).offsetY(
	         1)))  // 自身の上にブロックがあるなら、自身の右上にブロックがある
	    and (not lattice_->cellHasBlock(
	            myPos.offsetX(-1)))  // 自身の左側にブロックはない

	)
		return true;
	else
		return false;
}

bool CompressFullRangeBlockCode::can_process_phase2_ne(Cell3DPosition offset) {
	Cell3DPosition myPos = module->position + offset;
	if (!lattice_->cellHasBlock(myPos)) return false;
	if (lattice_->isFree(myPos.offsetY(-1)) and
	    lattice_->cellHasBlock(myPos.offsetX(-1)) and
	    lattice_->cellHasBlock(myPos.offsetX(-1).offsetY(-1)) and
	    lattice_->cellHasBlock(myPos.offsetY(-2)))
		return true;
	else
		return false;
}

bool CompressFullRangeBlockCode::can_process_phase2_se(Cell3DPosition offset) {
	Cell3DPosition myPos = module->position + offset;
	if (!lattice_->cellHasBlock(myPos)) return false;
	if (lattice_->isFree(myPos.offsetX(-1)) and
	    lattice_->cellHasBlock(myPos.offsetY(1)) and
	    lattice_->cellHasBlock(myPos.offsetX(-1).offsetY(1)) and
	    lattice_->cellHasBlock(myPos.offsetX(-2)))
		return true;
	else
		return false;
}
bool CompressFullRangeBlockCode::can_extend_phase1_sw(Cell3DPosition offset) {
	Cell3DPosition myPos = module->position + offset;

	if ((lattice_->cellHasBlock(myPos)) and   // 自身にブロックがある
	    (lattice_->isFree(myPos.offsetX(1)))  // 自身の右は空き
	    and
	    (not lattice_->cellHasBlock(myPos.offsetY(-2)))  // 自身の2つ下は空き
	    and
	    (lattice_->cellHasBlock(myPos.offsetX(-1)))  // 自身の左にブロックがある
	    and
	    (lattice_->cellHasBlock(myPos.offsetY(-1)))  // 自身の下にブロックがある
	    and (lattice_->cellHasBlock(
	            myPos.offsetX(1).offsetY(-1)))  // 自身の右下にブロックがある
	    and
	    ((lattice_->cellHasBlock(myPos.offsetX(1).offsetY(-2))) or
	     (lattice_->cellHasBlock(myPos.offsetX(2).offsetY(
	         -1))))  // 自身から下に3,右に1のセルか、自身から下に1,右に2のセルにブロックがある
	    and (lattice_->cellHasBlock(myPos.offsetX(-1).offsetY(1)) ==
	         lattice_->cellHasBlock(
	             myPos.offsetY(1)))  // 自身の上と左上のブロックの状態は同じ
	    and (not(lattice_->cellHasBlock(myPos.offsetX(-2).offsetY(0)) and
	             lattice_->cellHasBlock(myPos.offsetX(-2).offsetY(
	                 1))))  // 自身から左に2,上に(0,1)のセルのどちらかは空き
	    and (lattice_->cellHasBlock(
	            myPos.offsetX(-1).offsetY(-1)))  // 自身の左下にブロックがある
	) {
		return true;
	}
	return false;
}
bool CompressFullRangeBlockCode::can_extend_phase2_ne(Cell3DPosition offset) {
	Cell3DPosition myPos = module->position + offset;

	if ((lattice_->cellHasBlock(myPos)) and    // 自身にブロックがある
	    (lattice_->isFree(myPos.offsetX(-1)))  // 自身の左は空き
	    and (not lattice_->cellHasBlock(myPos.offsetX(1)))  // 自身の右は空き
	    and (lattice_->cellHasBlock(
	            myPos.offsetX(-1).offsetY(1)))  // 自身の左上にブロックがある
	    and (lattice_->cellHasBlock(
	            myPos.offsetX(-1).offsetY(-1)))  // 自身の左下にブロックがある
	    and
	    (lattice_->cellHasBlock(myPos.offsetY(-1)))  // 自身の下にブロックがある
	    and (lattice_->cellHasBlock(myPos.offsetX(1).offsetY(1)) or
	         lattice_->cellHasBlock(myPos.offsetY(2)))
	    // 自身から右上、2つ上のセルのどちらかにブロックがある

	    and (lattice_->cellHasBlock(myPos.offsetX(-2).offsetY(1)) ==
	         lattice_->cellHasBlock(myPos.offsetX(-2)))
	    // 自身から左に2,上に(0,1)にあるブロックの状態は同じ
	    and (lattice_->cellHasBlock(myPos.offsetY(-2)) or
	         lattice_->cellHasBlock(myPos.offsetX(1).offsetY(-1)))
	    // 自身から下に2、もしくは右下のセルのどちらかにブロックがある
	    and (lattice_->cellHasBlock(myPos.offsetY(1)))
	    // 自身の上にブロックがある
	)
		return true;

	return false;
}
// to east
bool CompressFullRangeBlockCode::can_extend_phase1_nw(Cell3DPosition offset) {
	Cell3DPosition myPos = module->position + offset;
	if (

	    (lattice_->cellHasBlock(myPos))           // 自身にブロックがある
	    and (lattice_->isFree(myPos.offsetX(1)))  // 自身の右は空き
	    and (not lattice_->cellHasBlock(myPos.offsetY(2)))  // 自身の2つ上は空き
	    and
	    (lattice_->cellHasBlock(myPos.offsetX(-1)))  // 自身の左にブロックがある
	    and (lattice_->cellHasBlock(
	            myPos.offsetX(-1).offsetY(1)))  // 自身の左上にブロックがある
	    and
	    (lattice_->cellHasBlock(myPos.offsetY(1)))  // 自身の上にブロックがある
	    and (lattice_->cellHasBlock(myPos.offsetX(1).offsetY(1)))
	    // 自身の右上のブロックがある
	    and ((lattice_->cellHasBlock(myPos.offsetX(1).offsetY(2))) or
	         (lattice_->cellHasBlock(myPos.offsetX(2).offsetY(1))))
	    // 自身から上に2右に１，もしくは上に1、右に2セルのどちらかにブロックがある
	    and (lattice_->cellHasBlock(myPos.offsetY(-1).offsetX(-1)) ==
	         lattice_->cellHasBlock(
	             myPos.offsetY(-1)))  // 自身の左下、下のブロックの状態は同じ
	    and (lattice_->cellHasBlock(myPos.offsetX(-2).offsetY(1)) or
	         lattice_->cellHasBlock(myPos.offsetX(-1).offsetY(2)))
	    // 自身から左に2,上に1のせるもしくは左に1,上に2のセルのどちらかにブロックがある
	) {
		return true;
	}
	return false;
}
// to south
bool CompressFullRangeBlockCode::can_extend_phase2_nw(Cell3DPosition offset) {
	Cell3DPosition myPos = module->position + offset;

	if ((lattice_->cellHasBlock(myPos)) and    // 自身にブロックがある
	    (lattice_->isFree(myPos.offsetY(-1)))  // 自身の下は空き
	    and (not lattice_->cellHasBlock(myPos.offsetX(1)))  // 自身の上は空き
	    and
	    (lattice_->cellHasBlock(myPos.offsetX(1)))  // 自身の右にブロックがある
	    and
	    (lattice_->cellHasBlock(myPos.offsetX(-1)))  // 自身の左にブロックがある
	    and (lattice_->cellHasBlock(
	            myPos.offsetX(-1).offsetY(-1)))  // 自身の左下にブロックがある
	    and (lattice_->cellHasBlock(
	            myPos.offsetX(1).offsetY(-1)))  // 自身の右下のブロックがある
	    and
	    ((lattice_->cellHasBlock(myPos.offsetX(-2))) or
	     (lattice_->cellHasBlock(myPos.offsetX(-1).offsetY(
	         1))))  // 自身から左に2のセルか、自身の左上どちらkブロックがある
	    and (lattice_->cellHasBlock(myPos.offsetX(0).offsetY(2)) ==
	         lattice_->cellHasBlock(myPos.offsetX(1).offsetY(-2)))
	    // 自身の２つ下、左から（0.1）のブロックの状態は同じ
	    and (lattice_->cellHasBlock(myPos.offsetX(2)) or
	         lattice_->cellHasBlock(myPos.offsetX(1).offsetY(1)))

	    // 自身から右の2のセルか自身の右上のセルのどちらかにブロックがある
	) {
		return true;
	}
	return false;
}

bool CompressFullRangeBlockCode::can_extend_phase2_sw(Cell3DPosition offset) {
	Cell3DPosition myPos = module->position + offset;
	if (

	    (lattice_->cellHasBlock(myPos))           // 自身にブロックがある
	    and (lattice_->isFree(myPos.offsetY(1)))  // 自身の上は空き
	    and (not lattice_->cellHasBlock(myPos.offsetY(-1)))  // 自身の下は空き
	    and
	    (lattice_->cellHasBlock(myPos.offsetX(1)))  // 自身の右にブロックがある
	    and (lattice_->cellHasBlock(
	            myPos.offsetX(-1).offsetY(1)))  // 自身の左上にブロックがある
	    and (lattice_->cellHasBlock(
	            myPos.offsetX(1).offsetY(1)))  // 自身の右上にブロックがある
	    and
	    ((lattice_->cellHasBlock(myPos.offsetX(1).offsetY(-1))) or
	     (lattice_->cellHasBlock(myPos.offsetX(
	         2))))  // 自身から下に1,右に1のセルか、自身から右に2のセルにブロックがある
	    and (lattice_->cellHasBlock(myPos.offsetX(-1).offsetY(2)) ==
	         lattice_->cellHasBlock(myPos.offsetY(
	             2)))  // 自身の2つ上と1つ左、2つ上のブロックの状態は同じ
	    and (not(lattice_->cellHasBlock(myPos.offsetX(-2).offsetY(2)) and
	             lattice_->cellHasBlock(myPos.offsetX(-2).offsetY(
	                 1))))  // 自身から左に2,上に(2,1)のセルのどちらかは空き
	    and
	    (lattice_->cellHasBlock(myPos.offsetX(-1)))  // 自身の左にブロックがある

	    // 左に2,上に1にブロックがある
	) {
		return true;
	}
	return false;
}
bool CompressFullRangeBlockCode::can_extend_phase1_ne(Cell3DPosition offset) {
	Cell3DPosition myPos = module->position + offset;
	if (

	    (lattice_->cellHasBlock(myPos))            // 自身にブロックがある
	    and (lattice_->isFree(myPos.offsetY(-1)))  // 自身の下は空き
	    and (not lattice_->cellHasBlock(myPos.offsetX(2)))  // 自身の2つ右は空き
	    and
	    (lattice_->cellHasBlock(myPos.offsetX(1)))  // 自身の右にブロックがある
	    and (lattice_->cellHasBlock(
	            myPos.offsetX(1).offsetY(-1)))  // 自身の右下にブロックがある
	    and
	    (lattice_->cellHasBlock(myPos.offsetY(1)))  // 自身の上にブロックがある
	    and (lattice_->cellHasBlock(myPos.offsetX(1).offsetY(2)) or
	         lattice_->cellHasBlock(myPos.offsetX(2).offsetY(1)))
	    // 自身から上に1,右に2のセルにブロックがある
	    // もしくは　上に２．右の1にブロックがある
	    and (lattice_->cellHasBlock(myPos.offsetX(-1).offsetY(1)) ==
	         lattice_->cellHasBlock(
	             myPos.offsetX(-1)))  // 自身の左と、左上ブロックの状態は同じ
	    and (lattice_->cellHasBlock(myPos.offsetX(2).offsetY(-1)) or
	         lattice_->cellHasBlock(myPos.offsetX(1).offsetY(-2)))
	    // 自身から右に2,下に1、もしくは右に１，下に2のセルのどちらかにブロックがある
	    and (lattice_->cellHasBlock(myPos.offsetX(1).offsetY(1)))
	    // 自身の右上にブロックがある
	) {
		return true;
	}
	return false;
}

bool CompressFullRangeBlockCode::can_process_phase2_sw(Cell3DPosition offset) {
	Cell3DPosition myPos = module->position + offset;
	if (!lattice_->cellHasBlock(myPos)) return false;
	if (lattice_->isFree(myPos.offsetY(1)) and
	    lattice_->cellHasBlock(myPos.offsetX(1)) and
	    lattice_->cellHasBlock(myPos.offsetX(1).offsetY(1)) and
	    lattice_->cellHasBlock(myPos.offsetY(2)))
		return true;
	else
		return false;
}

//note:凸型で操作がおかしかった
//Lpocketの動作が速いか、r angleの動作条件がおかしい
