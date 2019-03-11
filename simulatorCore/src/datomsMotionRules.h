#ifndef DATOMSMOTIONRULES_H
#define DATOMSMOTIONRULES_H

#include "lattice.h"
#include "datomsBlock.h"
#include "deformationEvents.h"

//!< \namespace Datoms
namespace Datoms {
	
/*enum ConnectorDirection { NORTH_WEST, NORTH_EAST,
                          EAST,
                          SOUTH_EAST, SOUTH_WEST,
                          WEST,
                          NUM_CONDIRS};
enum ConnectorOrientation { UP, DOWN, LEFT, RIGHT };*/

class DatomsMotionRulesLink;
class DatomsMotionRulesPiston;

class DatomsMotionRulesConnector {
public :
	int ID;
	vector <DatomsMotionRulesLink*> tabLinks;
	DatomsMotionRulesPiston* tabPtrPistons[4];
	DatomsMotionRulesConnector(int n):ID(n) {};
	void addLink(DatomsMotionRulesLink *lnk);
	void addPiston(DatomsMotionRulesPiston*ptr);
};

class DatomsMotionRulesPiston {
public :
	int ID;
	Vector3D direction; // direction of the piston in the datom coordinate system
	PistonId modelId;
	Vector3D Caxis[4],Vaxis[4];
	
	DatomsMotionRulesPiston(int n,const Vector3D& V,PistonId model):ID(n),direction(V),modelId(model) {};
	void setAxis(short i,const Vector3D& C, const Vector3D& V) {
		Caxis[i]=C; Vaxis[i]=V.normer();
	};
};

class DatomsMotionRulesLink {
public :
	DatomsMotionRulesConnector *conFrom; //!< origin connector
	DatomsMotionRulesConnector *conTo; //!< destination connector
	DatomsMotionRulesPiston *piston; // !< actuator
	short jointFrom,jointTo;
	Cell3DPosition tabBlockingCells[4]; // !< array of blocking ID
	short nbBlockingCells;

	DatomsMotionRulesLink(DatomsMotionRulesConnector *from,DatomsMotionRulesConnector *to,DatomsMotionRulesPiston *p,short s1,short s2,const Cell3DPosition &pos1,const Cell3DPosition &pos2,const Cell3DPosition &pos3);
	DatomsMotionRulesLink(DatomsMotionRulesConnector *from,DatomsMotionRulesConnector *to,DatomsMotionRulesPiston *p,short s1,short s2,const Cell3DPosition &pos1,const Cell3DPosition &pos2,const Cell3DPosition &pos3,const Cell3DPosition &pos4);
	
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

	
/** 
	@param mobile datom about to move
	@param pivot Fixed datom that will be used as a pivot
	@return Deformation object corresponding to this specific connector link on surface of pivot
*/
    Deformation getDeformations(const DatomsBlock* mobile, const DatomsBlock* pivot) const;
    
/**
	\brief Returns an array containing the ids of the two connectors forming the link such that [fromCon, ToCon]
**/
    std::array<short, 2> getConnectors() const;

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
   \brief Return if the rule is valid for a Catoms 3D
   \param pivot The evaluated pivot datom
   \return boolean result
**/
	bool isValid(const DatomsBlock *pivot);
/**
   \brief Get the list of cells that must be free to apply the rule
   \param pivot The evaluated 3D catom, get position and orientation for the rule
   \return vector of Cell3DPosition containing blocking positions in the grid
**/
	vector<Cell3DPosition> getBlockingCellsList(const DatomsBlock *pivot);
	
/**
   \brief Get the list of cells that must be free to apply the rule
   \param mobile The mobile datom, get position and orientation for the rule
   \return vector of Cell3DPosition containing blocking positions in the grid
**/
    Cell3DPosition getFinalPosition(DatomsBlock *mobile);
/**
   \brief Send a rotation event associated to the rule
   \param mobile : mobile catom that will turn
   \param fixed : pivot catom of rotation
   \param t : time of start of rotation
**/
    void sendRotationEvent(DatomsBlock *mobile,DatomsBlock *fixed,double t);
};

/*! \class DatomsMotionRules
    \brief Define the graph of possible motions for a 3D Catom
*/
class DatomsMotionRules {
    DatomsMotionRulesConnector *tabConnectors[12]; //!< array of connector rules
    DatomsMotionRulesPiston *tabPistons[6]; //<! array of piston data
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
		\brief Returns if module datom is able to turn from the orientation fromId to the toId one
		\param atom: the datom
		\param fromId : initial connector
		\param toId : final connector
		\return true if atom datom is able to turn from the orientation fromId to the toId one 
	**/
    bool isValid(const DatomsBlock& atom,int fromId, int toId);
	
	/**
		\brief Get the list of valid motion rules from a connector for atom datom
		\param pivot the datom pivot
		\param fromId initial connector
		\param vec vector of valid motion rules
		\return return if atom datom is able to turn from the orientation fromId to the toId one 
	**/
	bool getValidMotionList(const DatomsBlock* pivot,int from,vector<DatomsMotionRulesLink*>&vec);
	
	/**
		*		\brief Get the PistonId of modul close to a position
		*		\param module The selected module
		*		\param pos position in front of the piston
		*		\return return id of the piston
		**/
	PistonId getPistonId(const DatomsBlock *module,const Vector3D &pos);
	DatomsMotionRulesPiston** getTabPtrPistons(short connector) { return tabConnectors[connector]->tabPtrPistons; }
	protected:
	private:
		void addLinks(int conFrom,DatomsMotionRulesPiston* act,int id1, int id2,int id3,const DatomsMotionRulesPiston* P0, const DatomsMotionRulesPiston* P1, const DatomsMotionRulesPiston* P2,short j0,short j1,short j2,short j3);
		void addLink(int conFrom, int id1, DatomsMotionRulesPiston* act, const Vector3D &C1, const Vector3D &C2, const Vector3D &C3,short j0,short j1);
		void addLink(int conFrom, int id1, DatomsMotionRulesPiston* act, const Vector3D &C1, const Vector3D &C2, const Vector3D &C3, const Vector3D &C4,short j0,short j1);
};

std::ostream& operator<<(std::ostream &stream, DatomsMotionRulesLink const& mrl);

} // Datoms namespace
#endif // DATOMSMOTIONRULES_H
