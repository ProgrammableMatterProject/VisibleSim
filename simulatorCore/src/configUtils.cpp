
#include "configUtils.h"

using namespace std;

namespace ConfigUtils {

    string xmlBlockList(Color &color, float blockSize[3],
                        map<int, BuildingBlock*> &blocks) {
        string str = string("\t<blockList color=");
        str = str + colorToXmlString(color) + " blockSize=" + array3DToXmlString(blockSize) + " >\n";
        map<int, BaseSimulator::BuildingBlock*>::iterator it;
        for(it = blocks.begin(); 
            it != blocks.end(); it++) {
            BlinkyBlocks::BlinkyBlocksBlock* bb = (BlinkyBlocks::BlinkyBlocksBlock*) it->second;
            str = str + xmlBlinkyBlock(bb);
        }

        str = str + "\t</blockList>";

        return str;
    }

}
