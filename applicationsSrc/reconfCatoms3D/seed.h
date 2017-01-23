  #include <vector>
  #include <string>

  using namespace std;

  class Seed {
  private: 
      Seed *parent; 
      vector <Seed*> children; 

      int id; 

  public: 
      Seed();
      Seed(int id);
      Seed(Seed &parent, int id);

      Seed *GetParent(); 

      void SetChildren(vector<Seed*> children);
      vector<Seed*> GetChildren();
      void AddChildren(Seed &children);

      void SetId(int id);
      int GetId();
  };
