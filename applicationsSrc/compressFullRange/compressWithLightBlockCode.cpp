#include <bitset>
#include <mutex>
#include <vector>

#include "compressFullRangeBlockCode.hpp"
#include "lcmEvent.hpp"
int CompressFullRangeBlockCode::range = 99;  // 視野範囲(自身を中心とする)
bool CompressFullRangeBlockCode::internal_light = false;
bool CompressFullRangeBlockCode::external_light = false;
// string CompressFullRangeBlockCode::on_note = "not started";

// 最初に呼び出す
void CompressFullRangeBlockCode::startup() {
	console << "start " << module->blockId << "\n";

	nextPos = module->position;
	scheduleNextMove();
}

// 計算フェーズ
std::pair<Cell3DPosition, Color> CompressFullRangeBlockCode::compute() {
	return {Cell3DPosition(), BLUE};
}

string CompressFullRangeBlockCode::onInterfaceDraw() {
	string res = "strategy: ";
	return res;
}

// note:凸型で操作がおかしかった
// Lpocketの動作が速いか、r angleの動作条件がおかしい
