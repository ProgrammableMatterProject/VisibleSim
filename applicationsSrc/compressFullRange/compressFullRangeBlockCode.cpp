#include "compressFullRangeBlockCode.hpp"

#include <bitset>
#include <mutex>
#include <vector>

#include "lcmEvent.hpp"
int CompressFullRangeBlockCode::range = 99;  // 視野範囲(自身を中心とする)
bool CompressFullRangeBlockCode::internal_light = false;
bool CompressFullRangeBlockCode::external_light = false;
// string CompressFullRangeBlockCode::on_note = "not started";
const Color INITIAL_COLOR      = Color();  // 初期色
const Color HALT_COLOR         = GREY;
const Color WAITING_COLOR      = WHITE;
const Color COMPRESS_PH1_COLOR = RED;  // LIGHTGREEN
const Color COMPRESS_PH2_COLOR = Color(200, 0, 0);

const Color EXTEND_NW_PH1_COLOR = ORANGE;
const Color EXTEND_NW_PH2_COLOR = Color(200, 165, 0);

const Color EXTEND_SE_PH1_COLOR = Color(255, 0, 0);
const Color EXTEND_SE_PH2_COLOR = Color(128, 0, 0);

const Color EXTEND_NE_PH1_COLOR = CYAN;
const Color EXTEND_NE_PH2_COLOR = Color(0, 200, 200);

const Color EXTEND_SW_PH1_COLOR = BLUE;
const Color EXTEND_SW_PH2_COLOR = Color(0,0, 200);
// 方向リスト
const Cell3DPosition STAY  = Cell3DPosition(0, 0, 0);
const Cell3DPosition NORTH = Cell3DPosition(0, 1, 0);
const Cell3DPosition SOUTH = Cell3DPosition(0, -1, 0);
const Cell3DPosition EAST  = Cell3DPosition(1, 0, 0);
const Cell3DPosition WEST  = Cell3DPosition(-1, 0, 0);

const PositionParser halt_parser_sw({"****", "*mm*", "*xm*", "****"});

const PositionParser halt_parser_se({"****", "*mm*", "*mx*", "****"});

const PositionParser halt_parser_nw({"****", "*xm*", "*mm*", "****"});

const PositionParser halt_parser_ne({"****", "*mx*", "*mm*", "****"});

const PositionParser halt_about_parser({
    "*****",
    "*mmm*",
    "*mxm*",
    "*mme*",
    "*****",
});
// 圧縮動作
const PositionParser phase2_nw_parser({
    "**ebc*",
    "*exem*",
    "*lmm**",
    "**l***",
});

const PositionParser phase1_nw_parser({
    "******",
    "***ab*",
    "*nmxm*",
    "*nme**",
    "******",
});

// L型ポケットの開拓

const PositionParser extend_phase2_nw_parser({"*******", "**bb***", "**mem**",
                                              "*lmxmr*", "**l*r**", "*******"});
const PositionParser extend_phase1_nw_parser({"*******", "**bb***", "**mxe**",
                                              "*lmmmr*", "**l*r**", "*******"});

// 鏡像R型ポケットの開拓
const PositionParser extend_phase2_ne_parser({
    "******",
    "***r**",
    "*bmmr*",
    "*bmx**",
    "**mel*",
    "***l**",
    "******",
});

const PositionParser extend_phase1_ne_parser({
    "******",
    "***r**",
    "*bmmr*",
    "*bmm**",
    "**exl*",
    "***l**",
    "******",
});

// R型ポケットの開拓

const PositionParser extend_phase2_sw_parser({
    "*******",
    "***rr**",
    "**mmmR*",
    "**mxeR*",
    "**mmm**",
    "*******",
    "*******",
});

const PositionParser extend_phase1_sw_parser({
    "*******",
    "***rr**",
    "**mmmR*",
    "**mmxR*",
    "**mme**",
    "*******",
    "*******",
});
// 最初に呼び出す
void CompressFullRangeBlockCode::startup() {
	console << "start " << module->blockId << "\n";

	nextPos = module->position;
	scheduleNextMove();
	setColor(INITIAL_COLOR);
}

// 計算フェーズ
std::pair<Cell3DPosition, Color> CompressFullRangeBlockCode::compute() {
	// 停止判定
	{
		bool can_halt = true;
		for (int range_x = -this->range / 2; range_x <= this->range / 2;
		     range_x++)
			for (int range_y = -this->range / 2; range_y <= this->range / 2;
			     range_y++) {
				Cell3DPosition offset(range_x, range_y, 0);
				Cell3DPosition abspos = module->position + offset;
				if (this->isEmpty(abspos)) continue;

				ParserResult pr_sw = parseView(halt_parser_sw, 'm', offset);
				ParserResult pr_se = parseView(halt_parser_se, 'm', offset);
				ParserResult pr_nw = parseView(halt_parser_nw, 'm', offset);
				ParserResult pr_ne = parseView(halt_parser_ne, 'm', offset);
				ParserResult pr_about_module =
				    parseView(halt_about_parser, 'm', offset);
				ParserResult pr_about_empty =
				    parseView(halt_about_parser, 'e', offset);
				if ((pr_sw.all_module() || pr_se.all_module() ||
				     pr_nw.all_module() || pr_ne.all_module()) and
				    not(pr_about_empty.all_empty() and
				        pr_about_module.all_module())) {
				} else {
					can_halt = false;
				}
			}
		if (can_halt) {
			this->state = BlockState::TERMINATE;
			return {STAY, HALT_COLOR};
		}
	}
	// 戦略の決定方針
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
	int can_process_this_flag =
	    strategy_wait::STRATEGY_WAIT_SIZE;  // 可能うちの最小戦略
	int flagged_before_mine_flag = strategy_wait::
	    STRATEGY_WAIT_SIZE;  // 自分より前で実行可能な戦略のうちの最小番号
	int flagged_all_flag = strategy_wait::
	    STRATEGY_WAIT_SIZE;  // 全体で実行可能な戦略のうちの最小番号

	// 圧縮のフェーズ2
	{
		for (int range_y = this->range / 2; range_y >= -this->range / 2;
		     range_y--)
			for (int range_x = -this->range / 2; range_x <= this->range / 2;
			     range_x++) {
				// if (range_y == 0 and range_x > 0) break;
				Cell3DPosition offset(range_x, range_y, 0);
				Cell3DPosition abspos = module->position + offset;
				if (this->isEmpty(abspos)) continue;
				ParserResult pr_module =
				    parseView(phase2_nw_parser, 'm', offset);
				ParserResult pr_empty =
				    parseView(phase2_nw_parser, 'e', offset);
				ParserResult pr_b = parseView(phase2_nw_parser, 'b', offset);
				ParserResult pr_c = parseView(phase2_nw_parser, 'c', offset);
				ParserResult pr_l = parseView(phase2_nw_parser, 'l', offset);
				if (pr_module.all_module() and pr_empty.all_empty() and
				    (pr_b.all_empty() or pr_c.all_module()) and
				    (not pr_l.all_empty())) {
					flagged_all_flag =
					    std::min(flagged_all_flag, (int)strategy_wait::p2_nw);
					if (range_y == 0 and range_x == 0) {
						// 自分自身の場合
						can_process_this_flag = std::min(
						    can_process_this_flag, (int)strategy_wait::p2_nw);
					}
					if (range_y > 0 or (range_y == 0 and range_x < 0)) {
						flagged_before_mine_flag =
						    std::min(flagged_before_mine_flag,
						             (int)strategy_wait::p2_nw);
					}
				}
			}
	}

	// 圧縮のフェーズ1
	{
		for (int range_y = this->range / 2; range_y >= -this->range / 2;
		     range_y--)
			for (int range_x = -this->range / 2; range_x <= this->range / 2;
			     range_x++) {
				if (range_y == 0 and range_x > 0) break;
				Cell3DPosition offset(range_x, range_y, 0);
				Cell3DPosition abspos = module->position + offset;
				if (this->isEmpty(abspos)) continue;
				ParserResult pr_module =
				    parseView(phase1_nw_parser, 'm', offset);
				ParserResult pr_empty =
				    parseView(phase1_nw_parser, 'e', offset);
				ParserResult pr_a = parseView(phase1_nw_parser, 'a', offset);
				ParserResult pr_b = parseView(phase1_nw_parser, 'b', offset);
				ParserResult pr_n = parseView(phase1_nw_parser, 'n', offset);
				if (pr_module.all_module() and pr_empty.all_empty() and
				    (pr_a.all_empty() or pr_b.all_module()) and
				    (not pr_n.all_module())) {
					flagged_all_flag =
					    std::min(flagged_all_flag, (int)strategy_wait::p1_nw);
					if (range_y == 0 and range_x == 0) {
						// 自分自身の場合
						can_process_this_flag = std::min(
						    can_process_this_flag, (int)strategy_wait::p1_nw);
					}
					if (range_y > 0 or (range_y == 0 and range_x < 0)) {
						//	cerr<< "ph1 "<<abspos << endl;
						flagged_before_mine_flag =
						    std::min(flagged_before_mine_flag,
						             (int)strategy_wait::p1_nw);
					}
				}
			}
	}
	// L型ポケットの開拓フェーズ1
	{
		for (int range_y = this->range / 2; range_y >= -this->range / 2;
		     range_y--)
			for (int range_x = -this->range / 2; range_x <= this->range / 2;
			     range_x++) {
				Cell3DPosition offset(range_x, range_y, 0);
				Cell3DPosition abspos = module->position + offset;
				if (this->isEmpty(abspos)) continue;
				ParserResult pr_module =
				    parseView(extend_phase1_nw_parser, 'm', offset);
				ParserResult pr_empty =
				    parseView(extend_phase1_nw_parser, 'e', offset);

				ParserResult pr_branch =
				    parseView(extend_phase1_nw_parser, 'b', offset);
				ParserResult pa_left =
				    parseView(extend_phase1_nw_parser, 'l', offset);
				ParserResult pa_right =
				    parseView(extend_phase1_nw_parser, 'r', offset);
				if (pr_module.all_module() and pr_empty.all_empty() and
				    (pr_branch.all_empty() or pr_branch.all_module()) and
				    not pa_left.all_empty() and not pa_right.all_empty()) {
					flagged_all_flag = std::min(
					    flagged_all_flag, (int)strategy_wait::extend_p1_sw);
					if (range_y == 0 and range_x == 0) {
						// 自分自身の場合
						can_process_this_flag =
						    std::min(can_process_this_flag,
						             (int)strategy_wait::extend_p1_sw);
					}
					if (range_x < 0 or ((range_x == 0) and range_y < 0)) {
						flagged_before_mine_flag =
						    std::min(flagged_before_mine_flag,
						             (int)strategy_wait::extend_p1_sw);
					}
				}
			}
	}
	// L型ポケットの開拓フェーズ2
	{
		for (int range_y = this->range / 2; range_y >= -this->range / 2;
		     range_y--)
			for (int range_x = -this->range / 2; range_x <= this->range / 2;
			     range_x++) {
				Cell3DPosition offset(range_x, range_y, 0);
				Cell3DPosition abspos = module->position + offset;
				if (this->isEmpty(abspos)) continue;
				ParserResult pr_module =
				    parseView(extend_phase2_nw_parser, 'm', offset);
				ParserResult pr_empty =
				    parseView(extend_phase2_nw_parser, 'e', offset);

				ParserResult pr_branch =
				    parseView(extend_phase2_nw_parser, 'b', offset);
				ParserResult pa_left =
				    parseView(extend_phase2_nw_parser, 'l', offset);
				ParserResult pa_right =
				    parseView(extend_phase2_nw_parser, 'r', offset);
				if (pr_module.all_module() and pr_empty.all_empty() and
				    (pr_branch.all_empty() or pr_branch.all_module()) and
				    not pa_right.all_empty() and not pa_left.all_empty()) {
					flagged_all_flag = std::min(
					    flagged_all_flag, (int)strategy_wait::extend_p2_sw);
					if (range_y == 0 and range_x == 0) {
						// 自分自身の場合
						can_process_this_flag =
						    std::min(can_process_this_flag,
						             (int)strategy_wait::extend_p2_sw);
					}
					if (range_x < 0 or ((range_x == 0) and range_y < 0)) {
						flagged_before_mine_flag =
						    std::min(flagged_before_mine_flag,
						             (int)strategy_wait::extend_p2_sw);
					}
				}
			}
	}

	// 鏡像R型ポケットの圧縮フェーズ2
	{
		for (int range_y = this->range / 2; range_y >= -this->range / 2;
		     range_y--)
			for (int range_x = -this->range / 2; range_x <= this->range / 2;
			     range_x++) {
				Cell3DPosition offset(range_x, range_y, 0);
				Cell3DPosition abspos = module->position + offset;
				if (this->isEmpty(abspos)) continue;
				ParserResult pr_module =
				    parseView(extend_phase2_ne_parser, 'm', offset);
				ParserResult pr_empty =
				    parseView(extend_phase2_ne_parser, 'e', offset);

				ParserResult pr_branch =
				    parseView(extend_phase2_ne_parser, 'b', offset);
				ParserResult pa_left =
				    parseView(extend_phase2_ne_parser, 'l', offset);
				ParserResult pa_right =
				    parseView(extend_phase2_ne_parser, 'r', offset);
				if (pr_module.all_module() and pr_empty.all_empty() and
				    (pr_branch.all_empty() or pr_branch.all_module()) and
				    not pa_right.all_empty() and not pa_left.all_empty()) {
					flagged_all_flag = std::min(
					    flagged_all_flag, (int)strategy_wait::extend_p2_ne);
					if (range_y == 0 and range_x == 0) {
						// 自分自身の場合
						can_process_this_flag =
						    std::min(can_process_this_flag,
						             (int)strategy_wait::extend_p2_ne);
					}
					if (range_x > 0 or ((range_x == 0) and range_y > 0)) {
						flagged_before_mine_flag =
						    std::min(flagged_before_mine_flag,
						             (int)strategy_wait::extend_p2_ne);
					}
				}
			}
	}
	// 鏡像R型ポケットの圧縮フェーズ1
	{
		for (int range_y = this->range / 2; range_y >= -this->range / 2;
		     range_y--)
			for (int range_x = -this->range / 2; range_x <= this->range / 2;
			     range_x++) {
				Cell3DPosition offset(range_x, range_y, 0);
				Cell3DPosition abspos = module->position + offset;
				if (this->isEmpty(abspos)) continue;
				ParserResult pr_module =
				    parseView(extend_phase1_ne_parser, 'm', offset);
				ParserResult pr_empty =
				    parseView(extend_phase1_ne_parser, 'e', offset);

				ParserResult pr_branch =
				    parseView(extend_phase1_ne_parser, 'b', offset);
				ParserResult pa_left =
				    parseView(extend_phase1_ne_parser, 'l', offset);
				ParserResult pa_right =
				    parseView(extend_phase1_ne_parser, 'r', offset);
				if (pr_module.all_module() and pr_empty.all_empty() and
				    (pr_branch.all_empty() or pr_branch.all_module()) and
				    not pa_left.all_empty() and not pa_right.all_empty()) {
					flagged_all_flag = std::min(
					    flagged_all_flag, (int)strategy_wait::extend_p1_ne);
					if (range_y == 0 and range_x == 0) {
						// 自分自身の場合
						can_process_this_flag =
						    std::min(can_process_this_flag,
						             (int)strategy_wait::extend_p1_ne);
					}
					if (range_x > 0 or ((range_x == 0) and range_y > 0)) {
						flagged_before_mine_flag =
						    std::min(flagged_before_mine_flag,
						             (int)strategy_wait::extend_p1_ne);
					}
				}
			}
	}

	// R型ポケットの開拓フェーズ2
	{
		for (int range_y = this->range / 2; range_y >= -this->range / 2;
		     range_y--)
			for (int range_x = -this->range / 2; range_x <= this->range / 2;
			     range_x++) {
				Cell3DPosition offset(range_x, range_y, 0);
				Cell3DPosition abspos = module->position + offset;
				if (this->isEmpty(abspos)) continue;
				ParserResult pr_module =
				    parseView(extend_phase2_sw_parser, 'm', offset);
				ParserResult pr_empty =
				    parseView(extend_phase2_sw_parser, 'e', offset);

				ParserResult pa_left1 =
				    parseView(extend_phase2_sw_parser, 'l', offset);
				ParserResult pa_right1 =
				    parseView(extend_phase2_sw_parser, 'r', offset);
				ParserResult pa_left2 =
				    parseView(extend_phase2_sw_parser, 'L', offset);
				ParserResult pa_right2 =
				    parseView(extend_phase2_sw_parser, 'R', offset);
				if (pr_module.all_module() and pr_empty.all_empty() and
				    ((pa_left1.all_module() or pa_left2.all_module()) and
				     (pa_right1.all_module() or pa_right2.all_module()))) {
					flagged_all_flag = std::min(
					    flagged_all_flag, (int)strategy_wait::extend_p2_nw);
					if (range_y == 0 and range_x == 0) {
						// 自分自身の場合
						can_process_this_flag =
						    std::min(can_process_this_flag,
						             (int)strategy_wait::extend_p2_nw);
					}
					if (range_y > 0 or ((range_y == 0) and range_x < 0)) {
						flagged_before_mine_flag =
						    std::min(flagged_before_mine_flag,
						             (int)strategy_wait::extend_p2_nw);
					}
				}
			}
	}
	{
		for (int range_y = this->range / 2; range_y >= -this->range / 2;
		     range_y--)
			for (int range_x = -this->range / 2; range_x <= this->range / 2;
			     range_x++) {
				Cell3DPosition offset(range_x, range_y, 0);
				Cell3DPosition abspos = module->position + offset;
				if (this->isEmpty(abspos)) continue;
				ParserResult pr_module =
				    parseView(extend_phase1_sw_parser, 'm', offset);
				ParserResult pr_empty =
				    parseView(extend_phase1_sw_parser, 'e', offset);

				ParserResult pa_left1 =
				    parseView(extend_phase1_sw_parser, 'l', offset);
				ParserResult pa_right1 =
				    parseView(extend_phase1_sw_parser, 'r', offset);
				ParserResult pa_left2 =
				    parseView(extend_phase1_sw_parser, 'L', offset);
				ParserResult pa_right2 =
				    parseView(extend_phase1_sw_parser, 'R', offset);
				if (pr_module.all_module() and pr_empty.all_empty() and
				    ((pa_left1.all_module() or pa_left2.all_module()) and
				     (pa_right1.all_module() or pa_right2.all_module()))) {
					flagged_all_flag = std::min(
					    flagged_all_flag, (int)strategy_wait::extend_p1_nw);
					if (range_y == 0 and range_x == 0) {
						// 自分自身の場合
						can_process_this_flag =
						    std::min(can_process_this_flag,
						             (int)strategy_wait::extend_p1_nw);
					}
					if (range_y > 0 or ((range_y == 0) and range_x < 0)) {
						flagged_before_mine_flag =
						    std::min(flagged_before_mine_flag,
						             (int)strategy_wait::extend_p1_nw);
					}
				}
			}
	}
	//	cerr << module->blockId << " : "
	//	     << "can_process_this_flag=" << can_process_this_flag
	//	     << ", flagged_before_mine_flag=" << flagged_before_mine_flag
	//	     << ", flagged_all_flag=" << flagged_all_flag << endl;
	if (can_process_this_flag == strategy_wait::STRATEGY_WAIT_SIZE) {
		// 処理可能な戦略がない場合
		return {STAY, WAITING_COLOR};
	}
	if (can_process_this_flag < flagged_before_mine_flag and
	    can_process_this_flag == flagged_all_flag) {
		// 自分より前で実行可能な戦略がない場合、処理を行う
		switch (static_cast<strategy_wait>(can_process_this_flag)) {
			case p2_nw:
				return {EAST, COMPRESS_PH2_COLOR};
			case p1_nw:
				return {SOUTH, COMPRESS_PH1_COLOR};
			case extend_p2_sw:
				return {NORTH, EXTEND_SW_PH2_COLOR};
			case extend_p1_sw:
				return {EAST, EXTEND_SW_PH1_COLOR};
			case extend_p1_ne:
				return {WEST, EXTEND_NE_PH1_COLOR};
			case extend_p2_ne:
				return {SOUTH, EXTEND_NE_PH2_COLOR};
			case extend_p1_nw:
				return {SOUTH, EXTEND_NW_PH1_COLOR};
			case extend_p2_nw:
				return {EAST, EXTEND_NW_PH2_COLOR};
			default:
				return {STAY, WAITING_COLOR};
		}
	}

	return {STAY, WAITING_COLOR};
}

string CompressFullRangeBlockCode::onInterfaceDraw() {
	string res = "round : " + to_string(rounds) +
	             "\nstrategy: " + to_string(this->getId());
	return res;
}

// note:凸型で操作がおかしかった
// Lpocketの動作が速いか、r angleの動作条件がおかしい
// todo :
// 角の開拓の順を変更する(部分的な長方形の圧縮完了を持っていどうを開始する)
