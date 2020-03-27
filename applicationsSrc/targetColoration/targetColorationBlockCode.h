#ifndef targetColorationBlockCode_H_
#define targetColorationBlockCode_H_

#include "catoms3DBlockCode.h"
#include "catoms3DSimulator.h"

#include "rotation3DEvents.h"
#include "catoms3DBlock.h"

#include "polymer.h"

class targetColorationBlockCode : public Catoms3D::Catoms3DBlockCode {
private:
    // custom attribute
    Polymer *polymer = NULL;
public:

    Catoms3D::Catoms3DBlock *catom;

    targetColorationBlockCode(Catoms3D::Catoms3DBlock *host);
    ~targetColorationBlockCode();

    /**
     * @brief This function is called on startup of the blockCode,
     it can be used to perform initial configuration of the host or this instance of the distributed program
    */
    void startup() override;

    /** @brief Returns a new instance of this BlocKCode. Needed to associate code to module.
     *  @return pointer to a newly allocated instance of this distributed program, for host assignment */
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new targetColorationBlockCode((Catoms3DBlock*)host));
    };

    /**
     * User-implemented keyboard handler function that gets called when
     *  a key press event could not be caught by openglViewer
     * @note call is made from GlutContext::keyboardFunc (openglViewer.h)
     */
    virtual void onUserKeyPressed(unsigned char c, int x, int y) override;

    /**
     * Call by world during GL drawing phase, can be used by a user
     *  to draw custom Gl content into the simulated world
     * @note call is made from World::GlDraw
     */
    virtual void onGlDraw() override;

    void simulatePolymer();
};

#endif /* targetColorationBlockCode_H_ */
