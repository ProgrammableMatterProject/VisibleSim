  #include <vector>
  #include <string>

  using namespace std;

  enum SEED_DIRECTION {UP, DOWN};

  class Seed {
  private: 
      Seed *parent; 
      vector <Seed*> children; 
      SEED_DIRECTION direction;

      int id; 

  public: 
      Seed();
      Seed(int id, SEED_DIRECTION direction);
      Seed(int id, SEED_DIRECTION direction, Seed *parent);

      Seed *GetParent(); 
      SEED_DIRECTION GetDirection();

      void SetChildren(vector<Seed*> children);
      vector<Seed*> GetChildren();
      void AddChildren(Seed &children);

      void SetId(int id);
      int GetId();
  };
