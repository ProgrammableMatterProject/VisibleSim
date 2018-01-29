#ifndef CATOMS3DMOTIONRULES_H
#define CATOMS3DMOTIONRULES_H

#include "lattice.h"
#include "catoms3DBlock.h"

//!< \namespace Catoms3D
namespace Catoms3D {
enum MotionRuleLinkType { hexaFace, octaFace};

class Catoms3DMotionRulesLink;

class Catoms3DMotionRulesConnector {
public :
    short ID;
    vector <Catoms3DMotionRulesLink*> tabLinks;

    Catoms3DMotionRulesConnector(int n):ID(n) {};
    void addLink(Catoms3DMotionRulesLink *lnk);    
};

class Catoms3DMotionRulesLink {
    
    Catoms3DMotionRulesConnector *conFrom; //!< origin connector
    Catoms3DMotionRulesConnector *conTo; //!< destination connector
    double angle; //!< rotations angle
    double radius; //!< radius of curvature
    Vector3D axis1; //!< first rotation axis
    Vector3D axis2; //!< second rotation axis
    vector <int> tabBlockingIDs; //!< array of blocking ID
    MotionRuleLinkType MRLT;
public :
    Catoms3DMotionRulesLink(MotionRuleLinkType m,
                            Catoms3DMotionRulesConnector *from,
                            Catoms3DMotionRulesConnector *to,
                            double a,double r,const Vector3D& ax1,const Vector3D& ax2):
        conFrom(from),conTo(to),angle(a),radius(r),axis1(ax1),axis2(ax2), MRLT(m) {};
    /**
       \brief Get connector ID of destination of the motion
       \return destination connector ID
    **/
    inline short getConToID() { return conTo->ID; };
    /**
       \brief Gets the connector ID of source of the motion
       \return source connector ID
    **/
    inline short getConFromID() { return conFrom->ID; };

    inline bool isOctaFace() { return MRLT==octaFace; };

    // inline Catoms3DLinkDirection getDirection() { return Catoms3DLinkDirection(axis1, axis2); };
    
    /**
       \brief Returns an array containing the ids of the two connectors forming the link such that [fromCon, ToCon]
    **/
    std::array<short, 2> getConnectors() const;    

    /** \brief Indicates whether the link concerns the connector whose ID is passed as argument 
        \param conId The ID of the connector 
        \return true if link concerns conId, false otherwise
    **/
    bool concernsConnector(short conId) const;

    /** \brief Indicates whether the link concerns the connectors whose ID are passed as arguments
        \param conId1 The ID of the first connector 
        \param conId2 The ID of the second connector 
        \return true if link connects conId1 and condId2, false otherwise
    **/
    bool concernsConnectors(short conId1, short conId2) const;

    /**
       \brief Get string like "X->Y" where X is a character representing origin ID ('0..9,A,B') and Y is a character representing destination ID
    **/
    string getID() const;    
    /**
       \brief Add a blocking connector ID to the list
       \param id of the connector that must be free to allow the motion
    **/
    void addBlockingConnector(int id);
    /**
       \brief Add a blocking connectors into the list
       \param Comma separated string of ids of the connectors that must be free to allow the motion
    **/
    void addBlockingConnectorsString(const string &str);
    /**
       \brief Return if the rule is valid for a Catoms 3D
       \param The evaluated 3D catom
       \return boolean result
    **/
    bool isValid(const Catoms3DBlock *c3d);
    /**
       \brief Get the list of cells that must be free to apply the rule
       \param The evaluated 3D catom, get position and orientation for the rule
       \return vector of Cell3DPosition containing blocking positions in the grid
    **/
    vector<Cell3DPosition> getBlockingCellsList(const Catoms3DBlock *c3d);
    /**
       \brief Get the final position of the catom c3D after applying the rule
       \param The evaluated 3D catom
       \return Position in the grid
    **/
    Cell3DPosition getFinalPosition(Catoms3DBlock *c3d);
    /**
       \brief Send a rotation event associated to the rule
       \param mobile : mobile catom that will turn
       \param fixed : pivot catom of rotation
       \param t : time of start of rotation
    **/
    void sendRotationEvent(Catoms3DBlock *mobile,Catoms3DBlock *fixed,double t);

    /** \brief Could return something like TOP_TOPLEFT, so as to project one couple of connectors from one module over the ones of a neighbor 
        \todo */
    Catoms3DMotionRulesLink* getDirectionTuple() const;
};

/** 
    \class Catoms3DMotionRules
    \brief Define the graph of possible motions for a 3D Catom
**/
class Catoms3DMotionRules {
    Catoms3DMotionRulesConnector *tabConnectors[12]; //!< array of connector rules
public:    
    Catoms3DMotionRules();
    virtual ~Catoms3DMotionRules();

    /** \brief See what I was trying to do here?
        \todo */
    short mapConnectorOntoModule(Catoms3DBlock *catom,
                                 short conLinkCatom, short conToPivot);
    
    /**
       \brief Returns if c3d catom is able to turn from the orientation fromId to the toId one
       \param c3d: the catom
       \param fromId : initial connector
       \param toId : final connector
       \return true if c3d catom is able to turn from the orientation fromId to the toId one */
    bool isValid(const Catoms3DBlock& c3d,int fromId, int toId);
    /**
       \brief Get the list of valid motion rules from a connector for c3D catom
       \param c3d: the catom
       \param fromId : initial connector
       \param vec : vector of valid motion rules
       \return return if c3d catom is able to turn from the orientation fromId to the toId one */
    bool getValidMotionList(const Catoms3DBlock* c3d,int from,
                            vector<Catoms3DMotionRulesLink*>&vec);

    /**
       \brief Get the list of valid motion rules from any connector of a C3D catom
       \param c3d The catom whose rotation is being considered
       \param vec an output vector of the resulting valid motion rules
       \return return true if at least one rotation is possible, false otherwise
       \attention 
       \todo */
    bool getValidRotationsListForCatom(const Catoms3DBlock* c3d,
                                       vector<Catoms3DMotionRulesLink*>& vec);

    /**
       \brief Get the list of valid motion rules from any connector of a C3D catom
       \param c3d The catom whose rotation is being considered
       \param vec an output vector of the resulting valid motion rules
       \return return true if at least one rotation is possible, false otherwise
       \attention 
       \todo */
    bool getValidMotionListFromPivot(const Catoms3DBlock* pivot, int from,
                                     vector<Catoms3DMotionRulesLink*>&vec,
                                     const FCCLattice *lattice,const Target *target);

    /**
       \brief Finds all possible connector links on the surface of the pivot catom that a neighbor module could take
       \param pivot The catom on whose surface a path needs to be found
       \param links A vector reference that will hold all the solution links (\attention {PTHA: a set might be better suited})
       \return true if at least one link exists, false otherwise (\attention{PTHA: might not be necessary})
       \remarks This is different from finding a set of rotations that a module could perform, as in this case it's not the pivot catom that will perform the motion, hence there are fewer blocking constraints
       \todo bpiranda / pthalamy - Implement function
     **/
    bool getValidSurfaceLinksOnCatom(const Catoms3DBlock* pivot,
                                     vector<Catoms3DMotionRulesLink*>& links);
    
protected:
private:
    void addLinks3(int id1, int id2, int id3,
                   const Vector3D &axis1,const Vector3D &axis2,const Vector3D &axis3);
    void addLinks4(int id1, int id2, int id3, int id4,
                   const Vector3D &left,const Vector3D &lup,const Vector3D &rup);
    void addLink(MotionRuleLinkType mrlt,int id1, int id2,
                 double angle,double radius,
                 const Vector3D &axis1,const Vector3D &axis2,int n,int *tabBC);
};

std::ostream& operator<<(std::ostream &stream, Catoms3DMotionRulesLink const& mrl);

} // Catoms3D namespace
#endif // CATOMS3DMOTIONRULES_H
