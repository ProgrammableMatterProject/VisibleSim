#include <bitset>
#include <mutex>
#include <vector>

#include "compressFullRangeBlockCode.hpp"
#include "lcmEvent.hpp"
int CompressFullRangeBlockCode::range = 3;  // 視野範囲(自身を中心とする)
bool CompressFullRangeBlockCode::internal_light = true;
bool CompressFullRangeBlockCode::external_light = true;
// string CompressFullRangeBlockCode::on_note = "not started";
const Color INITIAL_COLOR      = Color();                      // 初期色
const Color LEADER_NORTH_COLOR = Color(128 + 64, 128, 255);  // リーダー色
const Color LEADER_SOUTH_COLOR = Color(128 - 64, 128, 255);  // リーダー色
const Color LEADER_EAST_COLOR  = Color(128, 128 + 64, 255);  // リーダー色
const Color LEADER_WEST_COLOR  = Color(128, 128 - 64, 255);  // リーダー色
const Color FOLLOWER_COLOR     = YELLOW;                      // フォロワー色
const Color HALT_COLOR         = ORANGE;

// 方向リスト
const Cell3DPosition STAY  = Cell3DPosition(0, 0, 0);
const Cell3DPosition NORTH = Cell3DPosition(0, 1, 0);
const Cell3DPosition SOUTH = Cell3DPosition(0, -1, 0);
const Cell3DPosition EAST  = Cell3DPosition(1, 0, 0);
const Cell3DPosition WEST  = Cell3DPosition(-1, 0, 0);

// 検知パターン

// leader election
const PositionParser leader_election_parser({
		"*****", 
		"*eee*", 
		"*exm*", 
		"*em**",
        "*****"});

// follower-move
const PositionParser follower_move_south_parser({
	"*****", 
	"*lxm*", 
	"ome**",
    "*****"});
const PositionParser follower_move_west_parser({
	"*o***", 
	"*ml**", 
	"*ex**",
    "**m**",
	"*****"});
const PositionParser follower_move_north_parser({
	"*****", 
	"**emo", 
	"*mxl*",
	"*****",
	"*****"});
const PositionParser follower_move_east_parser({
	"*****",
	"**m**", 
	"**xe*", 
	"**lm*",
	"***o*"});
// follower-gain-leader
const PositionParser follower_gain_leader_south_parser({
	"*****", 
	"**lm*",
    "**mx*",
	"**em*"});
const PositionParser follower_gain_leader_west_parser({
	"*****", 
	"*eml*",
    "*mxm*",
	"*****"});
const PositionParser follower_gain_leader_north_parser({
	"**me*", 
	"**xm*", 
	"**ml*",
	"*****"});
const PositionParser follower_gain_leader_east_parser({
	"*****", 
	"*mxm*", 
	"*lme*",
	"*****"});
// leader-move
const PositionParser leader_move_east_parser({
	"*****", 
	"**xem", 
	"**mm*",
    "*****"});
const PositionParser leader_move_south_parser({
	"*****", 
	"*mx**", 
	"*me**",
    "**m**",
	"*****"});
const PositionParser leader_move_west_parser({
	"*****", 
	"**mm*", 
	"*mex*",
	"*****"});
const PositionParser leader_move_north_parser({
	"*****", 
	"*m***",
	"*em**",
    "*xm**",
	"*****"});
// leader-release
const PositionParser leader_release_east_parser(
	{"*****", 
	"**xm*", 
	"**ml*",
    "**em*"});
const PositionParser leader_release_south_parser({
	"*****", 
	"*emx*", 
	"*mlm*",
	"*****",
	"*****"});
const PositionParser leader_release_west_parser({
	"*****", 
	"**me*", 
	"**lm*",
    "**mx*",
	"*****"});
const PositionParser leader_release_north_parser({
	"*****", 
	"*mlm*", 
	"*xme*",
	"*****"});

//停止状態の伝搬
const PositionParser halt_propagation_parser({
	"*****", 
	"*ttt*", 
	"*txt*", 
	"*ttt*",
	"*****"});
// 最初に呼び出す
void CompressFullRangeBlockCode::startup() {
	console << "start " << module->blockId << "\n";

	nextPos = module->position;
	this->setLight(INITIAL_COLOR);
	scheduleNextMove();
}

// 計算フェーズ
std::pair<Cell3DPosition, Color> CompressFullRangeBlockCode::compute() {
	Color current_light = this->getLight(this->module_pos);

	{
		ParserResult test_halt = parseView(halt_propagation_parser, 't');
		if (test_halt.light_map[HALT_COLOR] >= 1) {
			this->state = BlockState::TERMINATE;
			return {STAY, HALT_COLOR};
		}
	}
	if (current_light == INITIAL_COLOR) {//初期状態
		ParserResult test_module = parseView(leader_election_parser, 'm');
		ParserResult test_empty  = parseView(leader_election_parser, 'e');

		if (test_module.all_module() && test_empty.all_empty())
			return {STAY, LEADER_EAST_COLOR};

		return {STAY, FOLLOWER_COLOR};

	} else if (current_light == FOLLOWER_COLOR) {
		// 南に動くかを判定

		ParserResult test_empty  = parseView(follower_move_south_parser, 'e');
		ParserResult test_leader = parseView(follower_move_south_parser, 'l');
		ParserResult test_module = parseView(follower_move_south_parser, 'm');
		ParserResult test_not_leader = parseView(follower_move_south_parser, 'o');
		if (test_empty.all_empty() &&
		    test_leader.light_map[LEADER_EAST_COLOR] == 1 &&
		    test_module.all_module() && (test_not_leader.all_empty() ||test_not_leader.light_map[FOLLOWER_COLOR] == 1))
			return {SOUTH, INITIAL_COLOR};
		//西に動くかを判定
		test_empty  = parseView(follower_move_west_parser, 'e');
		test_leader = parseView(follower_move_west_parser, 'l');
		test_module = parseView(follower_move_west_parser, 'm');
		test_not_leader = parseView(follower_move_west_parser, 'o');
		if (test_empty.all_empty() &&
		    test_leader.light_map[LEADER_SOUTH_COLOR] == 1 &&
		    test_module.all_module() && (test_not_leader.all_empty() || test_not_leader.light_map[FOLLOWER_COLOR] == 1))
			return {WEST, INITIAL_COLOR};
		// 北に動くかを判定
		test_empty  = parseView(follower_move_north_parser, 'e');
		test_leader = parseView(follower_move_north_parser, 'l');
		test_module = parseView(follower_move_north_parser, 'm');
		test_not_leader = parseView(follower_move_north_parser, 'o');
		if (test_empty.all_empty() &&
		    test_leader.light_map[LEADER_WEST_COLOR] == 1 &&
		    test_module.all_module() && (test_not_leader.all_empty() || test_not_leader.light_map[FOLLOWER_COLOR] == 1))
			return {NORTH, INITIAL_COLOR};
		//東に動くかを判定
		test_empty  = parseView(follower_move_east_parser, 'e');
		test_leader = parseView(follower_move_east_parser, 'l');
		test_module = parseView(follower_move_east_parser, 'm');
		test_not_leader = parseView(follower_move_east_parser, 'o');
		if (test_empty.all_empty() &&
		    test_leader.light_map[LEADER_NORTH_COLOR] == 1 &&
		    test_module.all_module() && (test_not_leader.all_empty() || test_not_leader.light_map[FOLLOWER_COLOR] == 1))
			return {EAST, INITIAL_COLOR};
		// 南のleaderを引き継ぐかを判定
		test_empty  = parseView(follower_gain_leader_south_parser, 'e');
		test_leader = parseView(follower_gain_leader_south_parser, 'l');
		test_module = parseView(follower_gain_leader_south_parser, 'm');
		if (test_empty.all_empty() &&
		    test_leader.light_map[LEADER_EAST_COLOR] == 1 &&
		    test_module.all_module())
			return {STAY, LEADER_SOUTH_COLOR};
		//引き継げず、terminate状態にするかの判定
		if (test_empty.all_module() &&
		    test_leader.light_map[LEADER_EAST_COLOR] == 1 &&
		    test_module.all_module()) {
			this->state=BlockState::TERMINATE;
				return {STAY, HALT_COLOR};
		}
		// 西のleaderを引き継ぐかを判定
		test_empty  = parseView(follower_gain_leader_west_parser, 'e');
		test_leader = parseView(follower_gain_leader_west_parser, 'l');
		test_module = parseView(follower_gain_leader_west_parser, 'm');
		if (test_empty.all_empty() &&
		    test_leader.light_map[LEADER_SOUTH_COLOR] == 1 &&
		    test_module.all_module())
			return {STAY, LEADER_WEST_COLOR};
		// 引き継げず、terminate状態にするかの判定
		if (test_empty.all_module() &&
		    test_leader.light_map[LEADER_SOUTH_COLOR] == 1 &&
		    test_module.all_module()) {
			this->state = BlockState::TERMINATE;
			return {STAY, HALT_COLOR};
		}
		// 北のleaderを引き継ぐかを判定
		test_empty  = parseView(follower_gain_leader_north_parser, 'e');
		test_leader = parseView(follower_gain_leader_north_parser, 'l');
		test_module = parseView(follower_gain_leader_north_parser, 'm');
		if (test_empty.all_empty() &&
		    test_leader.light_map[LEADER_WEST_COLOR] == 1 &&
		    test_module.all_module())
			return {STAY, LEADER_NORTH_COLOR};
		// 引き継げず、terminate状態にするかの判定
		if (test_empty.all_module() &&
		    test_leader.light_map[LEADER_WEST_COLOR] == 1 &&
		    test_module.all_module()) {
			this->state = BlockState::TERMINATE;
			return {STAY, HALT_COLOR};
		}
		//東のleaderを引き継ぐかを判定
		test_empty  = parseView(follower_gain_leader_east_parser, 'e');
		test_leader = parseView(follower_gain_leader_east_parser, 'l');
		test_module = parseView(follower_gain_leader_east_parser, 'm');
		if (test_empty.all_empty() &&
		    test_leader.light_map[LEADER_NORTH_COLOR] == 1 &&
		    test_module.all_module())
			return {STAY, LEADER_EAST_COLOR};
		// 引き継げず、terminate状態にするかの判定
		if (test_empty.all_module() &&
		    test_leader.light_map[LEADER_NORTH_COLOR] == 1 &&
		    test_module.all_module()) {
			this->state = BlockState::TERMINATE;
			return {STAY, HALT_COLOR};
		}

	} else if (current_light == LEADER_EAST_COLOR) {
		// 移動するかの判定
		ParserResult test_empty  = parseView(leader_move_east_parser, 'e');
		ParserResult test_module = parseView(leader_move_east_parser, 'm');
		if (test_empty.all_empty() && test_module.all_module())
			return {EAST, INITIAL_COLOR};
		// リーダーを解放するかの判定
		test_empty               = parseView(leader_release_east_parser, 'e');
		test_module              = parseView(leader_release_east_parser, 'm');
		ParserResult test_leader = parseView(leader_release_east_parser, 'l');
		if (test_empty.all_empty() &&
		    test_leader.light_map[LEADER_SOUTH_COLOR] == 1 &&
		    test_module.all_module())
			return {STAY, FOLLOWER_COLOR};
	}else if(current_light== LEADER_SOUTH_COLOR){
		// 南に動くかを判定

		ParserResult test_empty  = parseView(leader_move_south_parser, 'e');
		ParserResult test_module = parseView(leader_move_south_parser, 'm');
		if (test_empty.all_empty() && test_module.all_module())
			return {SOUTH, INITIAL_COLOR};
		// リーダーを解放するかの判定
		test_empty               = parseView(leader_release_south_parser, 'e');
		test_module              = parseView(leader_release_south_parser, 'm');
		ParserResult test_leader = parseView(leader_release_south_parser, 'l');
		if (test_empty.all_empty() &&
		    test_leader.light_map[LEADER_WEST_COLOR] == 1 &&
		    test_module.all_module())
			return {STAY, FOLLOWER_COLOR};
	}else if(current_light== LEADER_WEST_COLOR){
		// 西に動くかを判定
		ParserResult test_empty  = parseView(leader_move_west_parser, 'e');
		ParserResult test_module = parseView(leader_move_west_parser, 'm');
		if (test_empty.all_empty() && test_module.all_module())
			return {WEST, INITIAL_COLOR};
		// リーダーを解放するかの判定
		test_empty               = parseView(leader_release_west_parser, 'e');
		test_module              = parseView(leader_release_west_parser, 'm');
		ParserResult test_leader = parseView(leader_release_west_parser, 'l');
		if (test_empty.all_empty() &&
		    test_leader.light_map[LEADER_NORTH_COLOR] == 1 &&
		    test_module.all_module())
		return {STAY, FOLLOWER_COLOR};
	}else if(current_light==LEADER_NORTH_COLOR){
		// 北に動くかを判定
		ParserResult test_empty  = parseView(leader_move_north_parser, 'e');
		ParserResult test_module = parseView(leader_move_north_parser, 'm');
		if (test_empty.all_empty() && test_module.all_module())
			return {NORTH, INITIAL_COLOR};
		// リーダーを解放するかの判定
		test_empty               = parseView(leader_release_north_parser, 'e');
		test_module              = parseView(leader_release_north_parser, 'm');
		ParserResult test_leader = parseView(leader_release_north_parser, 'l');
		if (test_empty.all_empty() &&
		    test_leader.light_map[LEADER_EAST_COLOR] == 1 &&
		    test_module.all_module())
			return {STAY, FOLLOWER_COLOR};
	}
	return {STAY, INITIAL_COLOR};
}

string CompressFullRangeBlockCode::onInterfaceDraw() {
	string res = "round : " + to_string(rounds) + "\nstrategy: " + to_string(this->getId());
	return res;
}

// note:凸型で操作がおかしかった
// Lpocketの動作が速いか、r angleの動作条件がおかしい
