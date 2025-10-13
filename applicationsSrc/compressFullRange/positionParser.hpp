#ifndef __POSITIONPARSER_HPP__
#define __POSITIONPARSER_HPP__

#include <map>
#include <string>
#include <tuple>
#include <vector>

#include "grid/lattice.h"
#include "utils/color.h"
using namespace std;

struct ColorLess {
	bool operator()(const Color& a, const Color& b) const noexcept {
		if (a[0] != b[0]) return a[0] < b[0];
		if (a[1] != b[1]) return a[1] < b[1];
		return a[2] < b[2];
	}
};

class PositionParser {
public:
	std::map<char, std::vector<Cell3DPosition>> pos_map;

	const char IGNORE = '*';
	const char ORIGIN = 'x';

	PositionParser(const std::vector<std::string> grid) {
		int origin_x;
		int origin_y;

		// gridの各要素が同じかどうかチェック
		size_t row_size = grid[0].size();
		size_t col_size = grid.size();
		for (const auto& row : grid) {
			if (row.size() != row_size) {
				throw std::invalid_argument(
				    "All rows in the grid must have the same length.");
			}
		}
		// Xがある場所を探す+それが唯一つかを確認する
		int x_count = 0;
		for (size_t y = 0; y < col_size; ++y) {
			for (size_t x = 0; x < row_size; ++x) {
				if (grid[y][x] == ORIGIN) {
					origin_x = x;
					origin_y = y;
					++x_count;
				}
			}
		}
		if (x_count != 1) {
			throw std::invalid_argument(
			    "There must be exactly one origin point.");
		}
		pos_map = std::map<char, std::vector<Cell3DPosition>>();
		// X,*以外のマークについて、Xからの相対座標を計算し、pos_mapに格納する
		for (size_t y = 0; y < col_size; ++y) {
			for (size_t x = 0; x < row_size; ++x) {
				char mark = grid[y][x];
				if (mark != ORIGIN && mark != IGNORE) {
					int rel_x = static_cast<int>(x) - origin_x;
					int rel_y = static_cast<int>(y) - origin_y;
					pos_map[mark].emplace_back(rel_x, -rel_y, 0);
				}
			}
		}
	}
};
struct ParserResult {
	int target_cells;                // 検索対象のセルの数
	int empty_cells;                 // 空のセルの数
	int module_cells;                // モジュールの数
	std::map<Color, int, ColorLess> light_map;  // ライトの色ごとの数
	ParserResult()
	    : target_cells(0), empty_cells(0), module_cells(0), light_map() {}
	~ParserResult() {
		// Destructor
		light_map.clear();
	}
};
/*
class PositionParser{
グリッドの情報と中心からのオフセットを与えることでグリッドに与えた特定のマークにあるモジュールの数、空の数、モジュールのうち、ライトの数が取得できる
グリッドには'*'無視、'x':起点座標、それ以外:ターゲット座標で区別される

constcutor : グリッドを与え、マップを生成する
作成したマップはアスキーコード -> position3Dの可変長配列で格納する
要件: グリッドは長方形、xがただ1つ含まれる

prase : オフセットと記号を与え、記号にあるモジュールを取得する
返り値:
{
total : 合計モジュール数
empty : 空の数
robot : ロボットの数
light : color --> 該当するモジュール数
}
}

*/
#endif /* __POSITIONPARSER_HPP__ */