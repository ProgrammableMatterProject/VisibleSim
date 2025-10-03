#include "compressFullRangeBlockCode.hpp"

#include <bitset>
#include <mutex>
#include <vector>

#include "lcmEvent.hpp"
// string CompressFullRangeBlockCode::on_note = "not started";

CompressFullRangeBlockCode::CompressFullRangeBlockCode(SlidingCubesBlock* host)
    : SlidingCubesBlockCode(host), module(host) {
	scheduler = getScheduler();
	if (not host) return;

	state = BlockState::LOOK;
}

/**
 * @brief ココをすべてのモジュールで同期したい
 *
 */
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

int CompressFullRangeBlockCode::distance(const Cell3DPosition& p) const {
	return std::max({std::abs(module_pos[0] - p[0]),
	                 std::abs(module_pos[1] - p[1]),
	                 std::abs(module_pos[2] - p[2])});
}

bool CompressFullRangeBlockCode::hasModule(const Cell3DPosition& p) const {
	if (distance(p) > range) return false;
	return views->cellHasBlock(p);
}
bool CompressFullRangeBlockCode::isEmpty(const Cell3DPosition& p) const {
	if (distance(p) > range) return false;
	return not views->cellHasBlock(p);
}
Color CompressFullRangeBlockCode::getLight(const Cell3DPosition& p) const {
	int dist = distance(p);
	if (dist == 0 and internal_light) {
		return this->getColor();
	} else if (dist > 0 and dist <= range and external_light) {
		auto target_module = views->getBlock(p);
		if (target_module) {
			return target_module->color;
		}
	}
	return Color();
}
// 最初に呼び出す
void CompressFullRangeBlockCode::startup() {
	console << "start " << module->blockId << "\n";

	nextPos = module->position;
	scheduleNextMove();
}

// 観測フェーズ
void CompressFullRangeBlockCode::look() {
	module_pos = module->position;
	views      = SlidingCubes::getWorld()->lattice;
}
// 計算フェーズ
std::pair<Cell3DPosition, Color> CompressFullRangeBlockCode::compute() {
	return {Cell3DPosition(), BLUE};
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
// 移動フェーズ
bool CompressFullRangeBlockCode::move() {
	if (nextPos == module->position) {
		return false;
	}

	if (module->canMoveTo(nextPos)) {
		// cerr << "strategy:" << moving_strategy << " \n";
		cerr << "id:" << module->blockId << " ";
		if (module->moveTo(nextPos)) {
			cerr << "moving " << nextPos << "\n";
			return true;
		} else {
			cerr << "failed to move to " << nextPos << "\n";
			return false;
		}

	} else {
		cerr << "move id: " << module->blockId << " : can't move \n";
		return false;
	}
}
// 移動終了時
void CompressFullRangeBlockCode::onMotionEnd() {
	nextPos = module->position;
	// moving_strategy = "none";
	state = BlockState::LOOK;
}

// イベントの処理
void CompressFullRangeBlockCode::processLocalEvent(std::shared_ptr<Event> pev) {
	// cerr << "event by id " << module->blockId << " type=" <<
	// pev->eventType
	//      << " date=" << pev->date << " / now=" << scheduler->now() << "@
	//      "
	//      << module->position << "\n";
	switch (pev->eventType) {
		case EVENT_LOOK:
			if (state == BlockState::LOOK) {
				look();
				state = BlockState::COMPUTE;
			}
			break;
		case EVENT_COMPUTE:
			if (state == BlockState::COMPUTE) {
				auto [pos, color] = compute();

				nextPos = module->position + pos;
				if (color != Color()) setColor(color);
				state = BlockState::MOVE;
			}
			break;
		case EVENT_MOVE:
			if (state == BlockState::MOVE) {
				bool res = move();
				state    = BlockState::MOVING;
				if (!res) {
					onMotionEnd();
				}
			}
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

string CompressFullRangeBlockCode::onInterfaceDraw() {
	string res = "strategy: ";
	return res;
}
void CompressFullRangeBlockCode::setLight(const Color& c) {
	if (!debug) return;
	return SlidingCubesBlockCode::setColor(c);
}

// note:凸型で操作がおかしかった
// Lpocketの動作が速いか、r angleの動作条件がおかしい
