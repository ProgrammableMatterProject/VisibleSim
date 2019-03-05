#ifndef DATOMSMOTIONRULES_H
#define DATOMSMOTIONRULES_H

#include "lattice.h"
#include "datomsBlock.h"
#include "deformationEvents.h"

//!< \namespace Datoms
namespace Datoms {

enum ConnectorDirection { NORTH_WEST, NORTH_EAST,
                          EAST,
                          SOUTH_EAST, SOUTH_WEST,
                          WEST,
                          NUM_CONDIRS};
enum ConnectorOrientation { UP, DOWN, LEFT, RIGHT };

class DatomsMotionRulesLink;

class DatomsMotionRulesConnector {
public :
    int ID;
    vector <DatomsMotionRulesLink*> tabLinks;

    DatomsMotionRulesConnector(int n):ID(n) {};
    void addLink(DatomsMotionRulesLink *lnk);
};

class DatomsMotionRulesLink {

    DatomsMotionRulesConnector *conFrom; //!< origin connector
    DatomsMotionRulesConnector *conTo; //!< destination connector
    Vector3D axis1; //!< first rotation axis
    Vector3D axis2; //!< second rotation axis
    vector <int> tabBlockingIDs; //!< array of blocking ID
    DeformationLinkType MRLT;
    uint8_t modelId;
public :
    DatomsMotionRulesLink(DeformationLinkType  m,DatomsMotionRulesConnector *from,DatomsMotionRulesConnector *to,const Vector3D& ax1,const Vector3D& ax2,uint8_t id):
        conFrom(from),conTo(to),axis1(ax1),axis2(ax2),MRLT(m),modelId(id) {};
/**
   \brief Get connector ID of destination of the motion
   \return destination connector ID
**/
    inline int getConToID() const { return conTo->ID; };
/**
       \brief Gets the connector ID of source of the motion
       \return source connector ID
**/
    inline short getConFromID() const { return conFrom->ID; };

    inline bool isOctaFace() const { return MRLT==OctaFace; };
    inline bool isHexaFace() const { return MRLT==HexaFace; };
    inline DeformationLinkType  getMRLT() const { return MRLT; };

/**
    @param mobile datom about to move
    @param pivot Fixed datom that will be used as a pivot
    @return Deformation object corresponding to this specific connector link on surface of pivot
*/
    Deformation getDeformations(const DatomsBlock* mobile, const DatomsBlock* pivot) const;

/**
    \brief Indicates whether the link concerns the connector whose ID is passed as argument
    \param conId The ID of the connector
    \return true if link concerns conId, false otherwise
**/
    bool concernsConnector(short conId) const;

/**
    \brief Indicates whether the link concerns the connectors whose ID are passed as arguments
    \param conId1 The ID of the first connector
    \param conId2 The ID of the second connector
    \return true if link connects conId1 and condId2, false otherwise
**/
    bool concernsConnectors(short conId1, short conId2) const;

/**
   \brief Get string like "X->Y" where X is a caractere representing origin ID ('0..9,A,B') and Y is a caractere representing destination ID
**/
    string getID();
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
    bool isValid(const DatomsBlock *c3d);
/**
   \brief Get the list of cells that must be free to apply the rule
   \param The evaluated 3D catom, get position and orientation for the rule
   \return vector of Cell3DPosition containing blocking positions in the grid
**/
    vector<Cell3DPosition> getBlockingCellsList(const DatomsBlock *c3d);
/**
   \brief Get the final position of the catom c3D after applying the rule
   \param The evaluated 3D catom
   \return Position in the grid
**/
    Cell3DPosition getFinalPosition(DatomsBlock *c3d);
/**
   \brief Send a rotation event associated to the rule
   \param mobile : mobile catom that will turn
   \param fixed : pivot catom of rotation
   \param t : time of start of rotation
**/
    void sendRotationEvent(DatomsBlock *mobile,DatomsBlock *fixed,double t);

/**
    \brief Could return something like TOP_TOPLEFT, so as to project one couple of connectors from one module over the ones of a neighbor
    \todo */
    DatomsMotionRulesLink* getDirectionTuple() const;

};

/*! \class DatomsMotionRules
    \brief Define the graph of possible motions for a 3D Catom
*/
class DatomsMotionRules {
    DatomsMotionRulesConnector *tabConnectors[12]; //!< array of connector rules
    public:
        DatomsMotionRules();
        virtual ~DatomsMotionRules();
    /**
        @brief Gets a collection containing all outgoing links from connector con
        @param con links source connector
        @return a reference to the vector of links for that connector
    **/
    const vector<DatomsMotionRulesLink*>& getMotionRulesLinksForConnector(short con);

    /**
        @param anchorCon latching connector to another datom
        @param conTo connector whose direction to determine relative to anchorCon
        @return ConnectorDirection corresponding to conTo relative to anchorCon or -1 if an input is invalid or anchorCon and conTo are not neighbor connectors
    **/
    short getConnectorDirection(short anchorCon, short conTo);

    /**
        @brief Transforms the input connector direction and returns the result
        @param d input connector direction
        @param inverted if false, mirroring will be done relative to the y-axis, and x-axis if true (\attention{false is default if left unspecified})
        @return mirror connector of input direction d
    **/
    static ConnectorDirection getMirrorConnectorDirection(ConnectorDirection d,
                                                          bool inverted = false);

    /**
        @brief Returns connector in mirror direction of d from connector conFrom
        @param conFrom source connector
        @param d connector direction that must be mirrored to find mirror neighbor
        @param inverted if false, mirroring will be done relative to the y-axis, and x-axis if true (\attention{false is default if left unspecified})
        @return connector in mirror direction of d from connector conFrom, or -1 if conFrom invalid
    **/
    static short getMirrorNeighborConnector(short conFrom, ConnectorDirection d,
                                            bool inverted = false);

    /**
        @brief Returns connector in direction d from conFrom
        @param conFrom source connector
        @param d direction of the neighbor
        @return connector in direction d from conFrom, or -1 if input is invalid
    **/
    static short getNeighborConnector(short conFrom, ConnectorDirection d);

    static const short* getNeighborConnectors(short conFrom);

    /**
        \brief Returns if atom datom is able to turn from the orientation fromId to the toId one
        \param atom: the datom
        \param fromId : initial connector
        \param toId : final connector
        \return true if atom datom is able to turn from the orientation fromId to the toId one
    **/
    bool isValide(const DatomsBlock& atom,int fromId, int toId);

    /**
        \brief Get the list of valid motion rules from a connector for atom datom
        \param atom: the datom
        \param fromId : initial connector
        \param vec : vector of valid motion rules
        \return return if atom datom is able to turn from the orientation fromId to the toId one
    **/
    bool getValidMotionList(const DatomsBlock* atom,int from,vector<DatomsMotionRulesLink*>&vec);

    /**
        \brief Get the list of valid motion rules from any connector around a pivot datom
        \param pivot The datom used as pivot
        \param vec an output vector of the resulting valid motion rules
        \return return true if at least one rotation is possible, false otherwise
    **/
    bool getValidMotionListFromPivot(const DatomsBlock* pivot,int from,vector<DatomsMotionRulesLink*>&vec,const SkewFCCLattice *lattice,const Target *target);

    /**
        \brief Finds all possible connector links on the surface of the pivot datom that a neighbor module could take
        \param pivot The catom on whose surface a path needs to be found
        \param links A vector reference that will hold all the solution links (\attention {PTHA: a set might be better suited})
        \return true if at least one link exists, false otherwise (\attention{PTHA: might not be necessary})
        \remarks This is different from finding a set of rotations that a module could perform, as in this case it's not the pivot catom that will perform the motion, hence there are fewer blocking constraints
    **/
    bool getValidSurfaceLinksOnDatom(const DatomsBlock* pivot,
                                     vector<DatomsMotionRulesLink*>& links);

    /**
        \brief Attempts to match a surface link from a pivot to a connector link for a connected mobile module to follow
        @param pivLink link to match
        @param m module that seeks to use surface link
        @param pivot pivot module
        @return a link of m that matches pivLink
    **/
    const DatomsMotionRulesLink*
    getMobileModuleLinkMatchingPivotLink(const DatomsMotionRulesLink* pivLink,
                                         const DatomsBlock* m,
                                         const DatomsBlock* pivot);

    /**
        \brief  Computes and return the mirror connector of mirroringCon of m1, on the surface of m2 with m1 and m2 connected through the connector of id dockingConM1  and dockingConM2, of m1 and m2, respectively.
        @note If m1 was to rotate from its mirroringCon connector to its dockingConM1 connector using m2 as pivot, the mirror connector of mirroringCon corresponds to the connector of m2 on which m1 is now attached.
        @param m1 reference module. Module that wants to move.
        @param m2 pivot module
        @param dockingConM1 connector through which m1 is attached to m2 (belongs to m1).
        @param dockingConM2 connector through which m2 is attached to m1 (belongs to m2).
        @param mirroringCon connector to be mirrored on m2 (belongs to m1).
        @return mirror connector of dockingCon on m2 (belongs to m2), or -1 if the two connectors are not neighbors (not connected through a face).
    **/
    static short getMirrorConnectorOnModule(const DatomsBlock *m1, const DatomsBlock *m2,
                                            short dockingConM1, short dockingConM2,
                                            short mirroringCon);

    protected:
    private:
        void addLinks4(int id1, int id2, int id3, int id4,const Vector3D &left,const Vector3D &lup,const Vector3D &rup,uint8_t modelId);
        void addLink(DeformationLinkType  mrlt,int id1, int id2,const Vector3D &axis1,const Vector3D &axis2,int n,int *tabBC,uint8_t modelId);
};

std::ostream& operator<<(std::ostream &stream, DatomsMotionRulesLink const& mrl);

} // Datoms namespace
#endif // DATOMSMOTIONRULES_H
