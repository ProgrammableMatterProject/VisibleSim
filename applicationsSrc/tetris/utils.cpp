class CoordsData{
public :
    int height ;
    int width ;
    unsigned int nbTree ;

    CoordsData(int h, int w, unsigned int t)
    {
        height = h ;
        width = w ;
        nbTree = t ;
    }
};

class TmnData{
public:
    int rotation ;
    int position ;
    int color ;

    TmnData(int r, int p, int c)
    {
        rotation = r ;
        position = p ;
        color = c ;
    }
};