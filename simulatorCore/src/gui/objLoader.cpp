/////////////////////////////////////////////////////////////////////////////
// File:        objLoader.cpp
// Project:     OBJ models file loader
// Author:      Benoit Piranda
// Modified by:
// Created:     2008-09-29
// Copyright:   (c) Benoit Piranda
// Licence:     UFC licence
// Version: 1.6 Windows
/////////////////////////////////////////////////////////////////////////////

#include <climits>
#include <cassert>
#include "objLoader.h"
#include "../math/vector3D.h"
#ifdef DEBUG_GRAPHICS
#include "../utils/trace.h"
#endif

using namespace std;

namespace ObjLoader {
    int extraire(const char *src, char *dest, int size_dest) {
        int i = 0;
        while (size_dest-- && *src != ' ' && *src != 10 && *src != 13) {
            *dest++ = *src++;
            i++;
        }
        *dest = 0;
        return i;
    }

#ifdef WIN32_VS
    void __cdecl odprintf(const char *format, ...)
    {
    char    buf[4096], *p = buf;
    va_list args;
    int     n;

            va_start(args, format);
            n = _vsnprintf_s(p, sizeof buf - 3, sizeof buf - 3, format, args); // buf-3 is room for CR/LF/NUL
            va_end(args);

            p += (n < 0) ? sizeof buf - 3 : n;

            while ( p > buf  &&  isspace(p[-1]) )
                    *--p = '\0';

            *p++ = '\r';
            *p++ = '\n';
            *p   = '\0';

            OutputDebugString(buf);
    }
#endif

/////////////////////////////////////////////////////////////////////////////
// class ObjLoader
    ObjLoader::ObjLoader(const char *rep, const char *titre) {
        vector<Point3> tabVertex;
        vector<Point3> tabNormal;
        vector<Point2> tabTexture;
        string fullTitle;
        GLuint currentObjectNumber = 0;

        fullTitle = string(rep) + "/" + titre;
#ifdef DEBUG_GRAPHICS
        OUTPUT << "Open " << fullTitle  << " file..."<< endl;
#endif

        ifstream fin(fullTitle);

        if (!fin.is_open()) {
            cerr << "File error: " << fullTitle << endl;
            exit(EXIT_FAILURE);
        }

#ifdef DEBUG_GRAPHICS
        OUTPUT << "Open " << fullTitle  << " file..."<< endl;
#endif
        mtls = nullptr;

        // chargement des points
        char ligne[255], str_pt1[64], str_pt2[64], str_pt3[64], str_pt4[64], txt[256];
        Point2 p2;
        Point3 p3;
        ObjData *objCourant;
        bool g_trouve = false;
        char nom[64];
        int i, numVert[4], numTex[4], numNorm[4];
        Sommet S1, S2, S3, S4;
        do {
#ifdef DEBUG_GRAPHICS
            OUTPUT << "Début de lecture" << endl;
#endif
            // headline
            do {
                fin.getline(ligne, 255);
                // delete spaces and tabs in the beginning of the lines
                if (fin.gcount() > 0) {
                    switch (ligne[0]) {
#ifdef DEBUG_GRAPHICS
                        case '#' : // comment
                            OUTPUT << ligne << endl;
                        break;
#endif
                        case ' ' :
                            break;
                        case 'm' : // mtllib ./tubes.mtl
                            if (strncmp(ligne, "mtllib", 6) == 0) {
                                extraire(ligne + 7, txt, 255);
                                mtls = new MtlLib(rep, txt);
                            }
                            g_trouve = true;
                            break;
                    }
                }
            } while (!fin.eof() && !g_trouve);

            while (!fin.eof()) { // reading data
                g_trouve = false;
                do {
                    fin.getline(ligne, 255);
                    if (fin.gcount() > 1) {
                        switch (ligne[0]) {
                            case ' ' :
                                break;
#ifdef DEBUG_GRAPHICS
                                case '#' :
                                    OUTPUT << ligne << endl;
                                break; // comment
#endif
                            case 'g' :
                                extraire(ligne + 2, nom, 63); // object name
#ifdef DEBUG_GRAPHICS
                                OUTPUT << "object : " << nom << endl;
#endif
                                g_trouve = true;
                                break;
                            case 'v' :
                                if (ligne[1] == 'n') { // normals (vn)
                                    p3.scan(ligne + 2);
                                    tabNormal.push_back(p3);
                                } else if (ligne[1] == 't') { // textures (vt)
                                    p2.scan(ligne + 2);
                                    tabTexture.push_back(p2);
                                } else { // vetices (v)
                                    p3.scan(ligne + 1);
                                    tabVertex.push_back(p3);
                                }
                                break;
#ifdef DEBUG_GRAPHICS
                                default :
                                    OUTPUT << "code '" << ligne[0] << " unknown :" << ligne << endl;
#endif
                        }
                    }
                } while (!fin.eof() && !g_trouve);
#ifdef DEBUG_GRAPHICS
                OUTPUT <<"Fin de lecture des coordonnées"<< endl;
#endif

                if (g_trouve) {
                    objCourant = new ObjData(nom);

                    objCourant->objectNumber = ++currentObjectNumber;
#ifdef DEBUG_GRAPHICS
                    OUTPUT << "new object :" << nom << " num= " << objCourant->objectNumber << endl;
#endif
                    tabObj.push_back(objCourant);
                    objCourant->objMtl = nullptr;
                    g_trouve = false;

                    do {
                        fin.getline(ligne, 255);
                        if (fin.gcount() > 1) {
                            switch (ligne[0]) {
                                case '#' :
#ifdef DEBUG_GRAPHICS
                                    OUTPUT << ligne<< endl;
#endif
                                    g_trouve = true;
                                    break; // comment and end of faces
                                case ' ' :
                                    break;
                                case 'g' :
                                    g_trouve = true;
                                    break;
                                case 'f' : // faces
                                    i = 2;
                                    i += extraire(ligne + i, str_pt1, 63) + 1;
                                    i += extraire(ligne + i, str_pt2, 63) + 1;
                                    i += extraire(ligne + i, str_pt3, 63) + 1;
                                    if (i >= int(strlen(ligne))) {
                                        str_pt4[0] = 0;
                                    } else {
                                        extraire(ligne + i, str_pt4, 63);
                                    }
                                    numeroPoint(str_pt1, numVert[0], numNorm[0], numTex[0]);
                                    numeroPoint(str_pt2, numVert[1], numNorm[1], numTex[1]);
                                    numeroPoint(str_pt3, numVert[2], numNorm[2], numTex[2]);
                                    if (str_pt4[0]) {
                                        numeroPoint(str_pt4, numVert[3], numNorm[3], numTex[3]);
                                    }
/*
#ifdef DEBUG_GRAPHICS
                            OUTPUT << "Face : (" << numVert[0] << "," << numNorm[0] << "," << numTex[0] << ")";
                            OUTPUT << "(" << numVert[1] << "," << numNorm[1] << "," << numTex[1] << ")";
                            OUTPUT << "(" << numVert[2] << "," << numNorm[2] << "," << numTex[2] << ")" << endl;
#endif
*/
                                    if (numTex[0] == 0 || numTex[1] == 0 || numTex[2] == 0) {
#ifdef DEBUG_GRAPHICS
                                       ERRPUT << "No texture coordinates for this object : " << objCourant->nomOriginal
                                               << endl;
                                        system("PAUSE");
#endif
                                    }
                                    S1.set(tabVertex[numVert[0] - 1].v, tabNormal[numNorm[0] - 1].v,
                                           tabTexture[numTex[0] - 1].v);
                                    S2.set(tabVertex[numVert[1] - 1].v, tabNormal[numNorm[1] - 1].v,
                                           tabTexture[numTex[1] - 1].v);
                                    S3.set(tabVertex[numVert[2] - 1].v, tabNormal[numNorm[2] - 1].v,
                                           tabTexture[numTex[2] - 1].v);
                                    objCourant->addFace(S1, S2, S3);
                                    if (str_pt4[0]) {
                                        S4.set(tabVertex[numVert[3] - 1].v, tabNormal[numNorm[3] - 1].v,
                                               tabTexture[numTex[3] - 1].v);
                                        objCourant->addFace(S1, S3, S4);
                                    }
                                    break;
                                case 'u' : // usemtl 09_-_Default
                                    if (strncmp(ligne, "usemtl", 6) == 0) {
#ifdef DEBUG_GRAPHICS
                                        OUTPUT << ligne << endl;
#endif
                                        extraire(ligne + 7, str_pt1, 64);
                                        Mtl *ptrMtl = mtls->getMtlByName(str_pt1);
                                        if (objCourant->objMtl == nullptr) {
                                            objCourant->objMtl = ptrMtl;
#ifdef WIN32_VS
                                            sprintf_s(objCourant->nom,"%s_%s",objCourant->nomOriginal,ptrMtl->nom);
#else
                                            sprintf(objCourant->nom, "%s_%s", objCourant->nomOriginal, ptrMtl->name);
#endif
#ifdef DEBUG_GRAPHICS
                                            OUTPUT << "associe l'objet " << objCourant->nom << endl;
#endif
                                        } else { // on fait un objet par texture
                                            // recherche si l'objet existe !
                                            vector<ObjData *>::const_iterator po = tabObj.begin();
                                            while (po != tabObj.end() && ((*po)->objMtl != ptrMtl ||
                                                                          strcmp((*po)->nomOriginal,
                                                                                 objCourant->nomOriginal) != 0)) {
                                                po++;
                                            }
                                            if (po != tabObj.end()) {
                                                objCourant = (*po);
#ifdef DEBUG_GRAPHICS
                                                OUTPUT << "complete l'objet " << objCourant->nom << endl;
#endif
                                            } else {
                                                char nom2[128];
#ifdef WIN32_VS
                                                sprintf_s(nom2,"%s_%s",nom,ptrMtl->nom);
#else
                                                sprintf(nom2, "%s_%s", nom, ptrMtl->name);
#endif
                                                objCourant = new ObjData(objCourant->nomOriginal);
                                                objCourant->objectNumber = currentObjectNumber;
                                                tabObj.push_back(objCourant);
                                                objCourant->objMtl = ptrMtl;
#ifdef WIN32_VS
                                                sprintf_s(objCourant->nom,"%s_%s",objCourant->nomOriginal,ptrMtl->nom);
#else
                                                sprintf(objCourant->nom, "%s_%s", objCourant->nomOriginal,
                                                        ptrMtl->name);
#endif
#ifdef DEBUG_GRAPHICS
                                                OUTPUT << "nouvel objet :" << objCourant->nom << "num=" << objCourant->objectNumber<< endl;
#endif
                                            }
                                        }
                                    }
                                    break;
                                case 's' : // gestion des groupes de lissage
#ifdef DEBUG_GRAPHICS
                                    ERRPUT << "warning: Smoothing groups not managed !" << endl;
#endif
                                    break;
#ifdef DEBUG_GRAPHICS
                                    default :
                                        OUTPUT << "symbole '" << ligne[0] << " inconnu de :" << ligne << endl;
#endif
                            }
                        }
                    } while (!fin.eof() && !g_trouve);
                }
            }
            fin.close();

            // libération des tableaux intermédiaires
            tabVertex.clear();
            tabTexture.clear();
            tabNormal.clear();
#ifdef DEBUG_GRAPHICS
            OUTPUT << "fin de la lecture" << endl;
#endif
        } while (!fin.eof());
        createVertexArrays();
        // find 'lighted' texture
        ptrMtlLighted = mtls->getMtlByName("lighted");
        if (!ptrMtlLighted) {
#ifdef DEBUG_GRAPHICS
            ERRPUT << "warning: No 'lighted' texture in obj file :" << titre << endl;
#endif
            ptrMtlLighted = mtls->getMtlById(1);
        }
    }

    void ObjLoader::glDraw(void) {
        for (const auto &obj:tabObj) {
            obj->glDraw();
        }
    }

    void ObjLoader::glDraw(GLuint n) {
        for (const auto &obj:tabObj) {
            if (obj->objectNumber == n) obj->glDraw();
        }
    }

    void ObjLoader::glDrawId(int n) {
        glLoadName(n);
        for (const auto &obj:tabObj) {
            obj->glDrawId();
        }
    }

    void ObjLoader::glDrawIdByMaterial(int &n) {
        for (const auto &obj:tabObj) {
            glLoadName(n++);
            obj->glDrawId();
        }
    }

    void ObjLoader::setLightedColor(const GLfloat *color) {
        ptrMtlLighted->Ka[0] = color[0]/255.0;
        ptrMtlLighted->Ka[1] = color[1]/255.0;
        ptrMtlLighted->Ka[2] = color[2]/255.0;
        ptrMtlLighted->Ka[3] = color[3];
        memcpy(ptrMtlLighted->Kd, ptrMtlLighted->Ka, 4 * sizeof(GLfloat));
    }

    void ObjLoader::setLightedColor(const GLubyte *color) {
        ptrMtlLighted->Ka[0] = GLfloat(color[0]) / 255.0;
        ptrMtlLighted->Ka[1] = GLfloat(color[1]) / 255.0;
        ptrMtlLighted->Ka[2] = GLfloat(color[2]) / 255.0;
        ptrMtlLighted->Ka[3] = 1.0f;
        memcpy(ptrMtlLighted->Kd, ptrMtlLighted->Ka, 4 * sizeof(GLfloat));
    }
    
    ObjData *ObjLoader::getObjDataByName(const string &name) const {
        for (const auto &obj:tabObj) {
            if (obj->nomOriginal == name) return obj;
        }
        return nullptr;
    }

    ObjLoader::~ObjLoader(void) {
        vector<ObjData *>::const_iterator ci = tabObj.begin();
        while (ci != tabObj.end()) {
            delete (*ci);
            ci++;
        }
        delete mtls;
    }

/////////////////////////////////////////////////////////////////////////////
// objLoader::numeroPoint
// retourne les numeros de vertex, normal et texture d'un point
    void ObjLoader::numeroPoint(char *str, int &vert, int &norm, int &tex) {
        int pos1, pos2;
        string str_ind(str);
        // extraction des numéros d'indice
        pos1 = (int) str_ind.find_first_of('/');
        pos2 = (int) str_ind.find_last_of('/');
        str[pos1] = 0;

#ifdef WIN32_VS
        if (sscanf_s(str,"%d",&vert)<=0) vert=0;
        str[pos2]=0;
        if (sscanf_s(str+pos1+1,"%d",&tex)<=0) tex=0;
        if (sscanf_s(str+pos2+1,"%d",&norm)<=0) norm=0;
#else
        if (sscanf(str, "%d", &vert) <= 0) vert = 0;
        str[pos2] = 0;
        if (sscanf(str + pos1 + 1, "%d", &tex) <= 0) tex = 0;
        if (sscanf(str + pos2 + 1, "%d", &norm) <= 0) norm = 0;
#endif
    }

    void ObjLoader::createVertexArrays() {
        vector<ObjData *>::const_iterator ci = tabObj.begin();
        while (ci != tabObj.end()) {
            (*ci)->createVertexArray();
            ci++;
        }
    }

    void ObjLoader::saveSTLfacets(ofstream &fout, const Vector3D &v, int ind0, int ind1) const {
        for (const ObjData *obj:tabObj) {
            obj->saveSTLfacets(fout, v, ind0, ind1);
        }
    }

    void ObjLoader::getBB(Vector3D &BBmin, Vector3D &BBmax) {
        auto it=tabObj.begin();
        if (it==tabObj.end()) return;
        BBmin.set((*it)->BBmin.v[0],(*it)->BBmin.v[1],(*it)->BBmin.v[2]);
        BBmax.set((*it)->BBmax.v[0],(*it)->BBmax.v[1],(*it)->BBmax.v[2]);
        it++;
        while (it!=tabObj.end()) {
            BBmin.setMin((*it)->BBmin.v[0],(*it)->BBmin.v[1],(*it)->BBmin.v[2]);
            BBmax.setMax((*it)->BBmax.v[0],(*it)->BBmax.v[1],(*it)->BBmax.v[2]);
            it++;
        }
    }

    bool ObjLoader::isInside(const Vector3D &pos) {
        auto it=tabObj.begin();
        while (it!=tabObj.end() && !(*it)->isInside(pos)) {
            it++;
        }
        return it!=tabObj.end();
    }
///////////////////////////////////////////////////////////////
// class ObjData
    ObjData::ObjData(const char *str) {
        objMtl = nullptr;
        nbreIndices = 0;
        tabVertices = nullptr;
        tabIndices = nullptr;
        center = nullptr;
        BBmin.v[0]=1e6; BBmin.v[1]=1e6; BBmin.v[2]=1e6;
        BBmax.v[0]=-1e6; BBmax.v[1]=-1e6; BBmax.v[2]=-1e6;
#ifdef WIN32_VS
        strncpy_s(nomOriginal,str,64);
#else
        strncpy(nomOriginal, str, 64);
#endif
    }

    ObjData::~ObjData() {
        auto pv = tabVertex.begin();
        while (pv != tabVertex.end()) {
            delete *pv;
            pv++;
        }
        tabVertex.clear();
        auto pi = tabFaces.begin();
        while (pi != tabFaces.end()) {
            delete *pi;
            pi++;
        }
        tabFaces.clear();

        delete[] tabVertices;
        delete[] tabIndices;
        delete center;
    }

    void ObjData::glDraw(void) {
        objMtl->glBind();
        // Bind our buffers much like we would for texturing
        glBindBuffer(GL_ARRAY_BUFFER, vboId);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, indexVboId);

        glEnableClientState(GL_TEXTURE_COORD_ARRAY);
        glEnableClientState(GL_NORMAL_ARRAY);
        glEnableClientState(GL_VERTEX_ARRAY);

        // Resetup our pointers.  This doesn't reinitialise any data, only how we walk through it
        glTexCoordPointer(2, GL_FLOAT, sizeof(vertexPosNrmTx), BUFFER_OFFSET(24));
        glNormalPointer(GL_FLOAT, sizeof(vertexPosNrmTx), BUFFER_OFFSET(12));
        glVertexPointer(3, GL_FLOAT, sizeof(vertexPosNrmTx), BUFFER_OFFSET(0));

        // Actually do our drawing, parameters are Primative (Triangles, Quads, Triangle Fans etc), Elements to
        // draw, Type of each element, Start Offset
        glDrawElements(GL_TRIANGLES, nbreIndices, GL_UNSIGNED_INT, BUFFER_OFFSET(0));

        // Disable our client state back to normal drawing
        glDisableClientState(GL_TEXTURE_COORD_ARRAY);
        glDisableClientState(GL_NORMAL_ARRAY);
        glDisableClientState(GL_VERTEX_ARRAY);
    }

    void ObjData::glDrawId(void) {
        /*glEnableClientState(GL_NORMAL_ARRAY);
        glEnableClientState(GL_TEXTURE_COORD_ARRAY);
        glEnableClientState(GL_VERTEX_ARRAY);
        glNormalPointer(GL_FLOAT, 0, tabNormals);
        glTexCoordPointer(2, GL_FLOAT, 0, tabTexCoords);
        glVertexPointer(3, GL_FLOAT, 0, tabVertices);
        glDrawElements(GL_TRIANGLES, nbreIndices,GL_UNSIGNED_INT,tabIndices);
        glDisableClientState(GL_VERTEX_ARRAY);  // disable vertex arrays
        glDisableClientState(GL_TEXTURE_COORD_ARRAY);
        glDisableClientState(GL_NORMAL_ARRAY);*/

        glBindBuffer(GL_ARRAY_BUFFER, vboId);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, indexVboId);

//	glEnableClientState(GL_TEXTURE_COORD_ARRAY);
//	glEnableClientState(GL_NORMAL_ARRAY);
        glEnableClientState(GL_VERTEX_ARRAY);

        // Resetup our pointers.  This doesn't reinitialise any data, only how we walk through it
//	glTexCoordPointer(2, GL_FLOAT, sizeof(vertexPosNrmTx), BUFFER_OFFSET(24));
//	glNormalPointer(GL_FLOAT, sizeof(vertexPosNrmTx), BUFFER_OFFSET(12));
        glVertexPointer(3, GL_FLOAT, sizeof(vertexPosNrmTx), BUFFER_OFFSET(0));

        // Actually do our drawing, parameters are Primative (Triangles, Quads, Triangle Fans etc), Elements to
        // draw, Type of each element, Start Offset
        glDrawElements(GL_TRIANGLES, nbreIndices, GL_UNSIGNED_INT, BUFFER_OFFSET(0));
        // Disable our client state back to normal drawing
//	glDisableClientState(GL_TEXTURE_COORD_ARRAY);
//	glDisableClientState(GL_NORMAL_ARRAY);
        glDisableClientState(GL_VERTEX_ARRAY);
    }

    void ObjData::saveSTLfacets(ofstream &file, const Vector3D &p, int ind0, int ind1, bool invNormal) const {
        vertexPosNrmTx *pv;
        Vector3D p0, p1, p2, normal, v1, v2;
        GLuint *ptrInd = tabIndices + ind0 * 3;
        int i = (ind1 == -1) ? (nbreIndices / 3 - ind0) : (ind1 - ind0);
        while (i--) {
            pv = tabVertices + *ptrInd++;
            p0.set(pv->x + p[0], pv->y + p[1], pv->z + p[2], 1.0);
            pv = tabVertices + *ptrInd++;
            v1.set(pv->x, pv->y, pv->z, 1.0);
            p1.set(pv->x + p[0], pv->y + p[1], pv->z + p[2], 1.0);
            pv = tabVertices + *ptrInd++;
            p2.set(pv->x + p[0], pv->y + p[1], pv->z + p[2], 1.0);

            v1 = p1 - p0;
            v2 = p2 - p0;
            if (v1.norme2() != 0 && v2.norme2() != 0) {
                normal = v1 ^ v2;
                normal.normer_interne();
                char buf[25];
                if (invNormal) {
                    normal = -1.0 * normal;
                    snprintf(buf, 25, "%5.3f %5.3f %5.3f", normal[0], normal[1], normal[2]);
                    file << "          facet normal " << buf << endl;
                    file << "            outer loop" << endl;
                    snprintf(buf, 25, "%5.3f %5.3f %5.3f", p2[0], p2[1], p2[2]);
                    file << "              vertex " << buf << endl;
                    snprintf(buf, 25, "%5.3f %5.3f %5.3f", p1[0], p1[1], p1[2]);
                    file << "              vertex " << buf << endl;
                    snprintf(buf, 25, "%5.3f %5.3f %5.3f", p0[0], p0[1], p0[2]);
                    file << "              vertex " << buf << endl;
                    file << "            endloop" << endl;
                    file << "          endfacet" << endl;
                } else {
                    snprintf(buf, 25, "%5.3f %5.3f %5.3f", normal[0], normal[1], normal[2]);
                    file << "          facet normal " << buf << endl;
                    file << "            outer loop" << endl;
                    snprintf(buf, 25, "%5.3f %5.3f %5.3f", p0[0], p0[1], p0[2]);
                    file << "              vertex " << buf << endl;
                    snprintf(buf, 25, "%5.3f %5.3f %5.3f", p1[0], p1[1], p1[2]);
                    file << "              vertex " << buf << endl;
                    snprintf(buf, 25, "%5.3f %5.3f %5.3f", p2[0], p2[1], p2[2]);
                    file << "              vertex " << buf << endl;
                    file << "            endloop" << endl;
                    file << "          endfacet" << endl;
                }
            }
        }
    }

    void ObjData::addFace(Sommet &ptr1, Sommet &ptr2, Sommet &ptr3) {
        GLuint idS[3]; // indice des 3 sommets dans le tableau;
        //static int num=0;
        idS[0] = addVertex(ptr1);
        idS[1] = addVertex(ptr2);
        idS[2] = addVertex(ptr3);

        tabFaces.push_back(new FaceTri(idS[0], idS[1], idS[2]));
        //OUTPUT << num++ << endl;
    }

    GLuint ObjData::addVertex(const Sommet &s) {
        // recherche le sommet dans le tableau
        vector<Sommet *>::const_iterator p = tabVertex.begin();
        GLuint i = 0;
        while (p != tabVertex.end()) {
            if (*(*p) == s) return i;
            p++;
            i++;
        }
        if (s.v[0]<BBmin.v[0]) BBmin.v[0]=s.v[0];
        if (s.v[1]<BBmin.v[1]) BBmin.v[1]=s.v[1];
        if (s.v[2]<BBmin.v[2]) BBmin.v[2]=s.v[2];
        if (s.v[0]>BBmax.v[0]) BBmax.v[0]=s.v[0];
        if (s.v[1]>BBmax.v[1]) BBmax.v[1]=s.v[1];
        if (s.v[2]>BBmax.v[2]) BBmax.v[2]=s.v[2];
        tabVertex.push_back(new Sommet(s));
        return i;
    }

    void ObjData::createVertexArray() {
        GLuint sizeVert = tabVertex.size();
        tabVertices = new vertexPosNrmTx[sizeVert];
        vertexPosNrmTx *ptrV = tabVertices;

        center = new Point3(0, 0, 0);
        //vector<Sommet *>::const_iterator pv = tabVertex.begin();
        auto pv = tabVertex.begin();
        int n = 0;
        while (pv != tabVertex.end()) {
            ptrV->x = (*pv)->v[0];
            ptrV->y = (*pv)->v[1];
            ptrV->z = (*pv)->v[2];
            center->v[0] += ptrV->x;
            center->v[1] += ptrV->y;
            center->v[2] += ptrV->z;
            ptrV->nx = (*pv)->n[0];
            ptrV->ny = (*pv)->n[1];
            ptrV->nz = (*pv)->n[2];
            ptrV->s = (*pv)->t[0];
            ptrV->t = (*pv)->t[1];
            ptrV++;
            pv++;
            n++;
        }
        center->v[0] /= n;
        center->v[1] /= n;
        center->v[2] /= n;

        nbreIndices = 3 * tabFaces.size();
        tabIndices = new GLuint[nbreIndices];
        GLuint *ptrI = tabIndices;
        vector<FaceTri *>::const_iterator pi = tabFaces.begin();
        while (pi != tabFaces.end()) {
            memcpy(ptrI, (*pi)->ind, 3 * sizeof(GLuint));
            ptrI += 3;
            pi++;
        }

        // prepare the VBO
        glGenBuffers(1, &vboId);
        glBindBuffer(GL_ARRAY_BUFFER, vboId);
        glBufferData(GL_ARRAY_BUFFER, sizeof(vertexPosNrmTx) * sizeVert, nullptr, GL_STATIC_DRAW);
        glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(vertexPosNrmTx) * sizeVert, tabVertices);
        glTexCoordPointer(2, GL_FLOAT, sizeof(vertexPosNrmTx), BUFFER_OFFSET(24));
        glNormalPointer(GL_FLOAT, sizeof(vertexPosNrmTx), BUFFER_OFFSET(12));
        glVertexPointer(3, GL_FLOAT, sizeof(vertexPosNrmTx), BUFFER_OFFSET(0));

        glGenBuffers(1, &indexVboId); // Generate buffer
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, indexVboId);
        glBufferData(GL_ELEMENT_ARRAY_BUFFER, nbreIndices * sizeof(GLuint), tabIndices, GL_STATIC_DRAW);

        glBindBuffer(GL_ARRAY_BUFFER, vboId);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, indexVboId);
    }

    bool ObjData::intersectFace(const FaceTri &tri, const Vector3D &pos, const Vector3D &dir) {
        const float EPSILON = 0.0000001;
        Vector3D vertex0 = tabVertex[tri.ind[0]]->toVector3D();
        Vector3D vertex1 = tabVertex[tri.ind[1]]->toVector3D();
        Vector3D vertex2 = tabVertex[tri.ind[2]]->toVector3D();
        Vector3D edge1, edge2, h, s, q;
        float a,f,u,v;
        edge1 = vertex1 - vertex0;
        edge2 = vertex2 - vertex0;
        h = dir ^ edge2;
        a = edge1 * h;
        if (a > -EPSILON && a < EPSILON)
            return false;    // Le rayon est parallèle au triangle.

        f = 1.0f/a;
        s = pos - vertex0;
        u = f * (s * h);
        if (u < 0.0 || u > 1.0) return false;
        q = s ^ edge1;
        v = f * (dir *q);
        if (v < 0.0 || u + v > 1.0) return false;

        // On calcule t pour savoir ou le point d'intersection se situe sur la ligne.
        float t = f * (edge2 *q);
        return (t > EPSILON);
        /*if (t > EPSILON) // Intersection avec le rayon
        {
            outIntersectionPoint = rayOrigin + rayVector * t;
            return true;
        }
        else // On a bien une intersection de droite, mais pas de rayon.
            return false;*/
    }

    bool ObjData::isInside(const Vector3D &pos) {
        // count the number of intersections between a ray and the borders
        Vector3D direction(2,1,5);
        direction.normer_interne();

        int n=0;
        for (auto &f:tabFaces) {
            n+=intersectFace(*f,pos,direction);
        }
        return n%2==1;
    }
/////////////////////////////////////////////////////////////////////////////
// class mtlLib
    MtlLib::MtlLib(const char *rep, const char *titre) {
        char txt[256];
        Mtl *currentMtl;
#ifdef WIN32_VS
        sprintf_s(txt,256,"%s\\%s",rep,titre);
#else
        sprintf(txt, "%s/%s", rep, titre);
#endif
        ifstream fin(txt);
        if (!fin.is_open()) {
            char erreur[1024];
#ifdef WIN32_VS
            sprintf_s(erreur,1024,"File error : %s",txt);
#else
            sprintf(erreur, "File error : '%s'", txt);
#endif
            perror("MtlLib");
            exit(EXIT_FAILURE);
        }

// lecture des lignes
        char ligne[255], dest[255];
        while (!fin.eof()) {
            fin.getline(ligne, 255);
            int i = 0;
            while (ligne[i] == ' ' || ligne[i] == '\t') i++;

            if (fin.gcount() > i) {
                switch (ligne[i]) {
                    case '#' : // commentaire
#ifdef DEBUG_GRAPHICS
                        OUTPUT << ligne << endl;
#endif
                        break;
                    case ' ' :
                        break;
                    case 'n' : { // le nom du mat�riau (newmtl)
                        currentMtl = new Mtl();
#ifdef WIN32_VS
                        int lng = (int)strlen(ligne);
                        currentMtl->nom = new char[lng];
                        strncpy_s(currentMtl->nom,lng,ligne+i+7,lng);
                        odprintf("newmtl %s",currentMtl->nom);
#else
                        extraire(ligne + i + 7, dest, 255);
                        int lng = (int) strlen(dest) + 1;
                        currentMtl->name = new char[lng];
                        strncpy(currentMtl->name, dest, lng);
#endif
                        tabMtl.push_back(currentMtl);
                    }
                        break;
                    case 'K' :
                        if (ligne[i + 1] == 'a') { // Ka
#ifdef WIN32_VS
                            sscanf_s(ligne+i+2,"%f %f %f",&currentMtl->Ka[0],&currentMtl->Ka[1],&currentMtl->Ka[2]);
#else
                            sscanf(ligne + i + 2, "%f %f %f", &currentMtl->Ka[0], &currentMtl->Ka[1],
                                   &currentMtl->Ka[2]);
#endif
                            currentMtl->Ka[3] = 1.0;
                            //OUTPUT << "Ka :" << currentMtl->Ka.v[0] << "," << currentMtl->Ka.v[1] << "," << currentMtl->Ka.v[2] << endl;
                        } else if (ligne[i + 1] == 'd') { // Kd
#ifdef WIN32_VS
                            sscanf_s(ligne+i+2,"%f %f %f",&currentMtl->Kd[0],&currentMtl->Kd[1],&currentMtl->Kd[2]);
#else
                            sscanf(ligne + i + 2, "%f %f %f", &currentMtl->Kd[0], &currentMtl->Kd[1],
                                   &currentMtl->Kd[2]);
#endif
                            currentMtl->Kd[3] = 1.0;
                            //OUTPUT << "Kd :" << currentMtl->Kd.v[0] << "," << currentMtl->Kd.v[1] << "," << currentMtl->Kd.v[2] << endl;
                        } else if (ligne[i + 1] == 's') { // Ks
#ifdef WIN32_VS
                            sscanf_s(ligne+i+2,"%f %f %f",&currentMtl->Ks[0],&currentMtl->Ks[1],&currentMtl->Ks[2]);
#else
                            sscanf(ligne + i + 2, "%f %f %f", &currentMtl->Ks[0], &currentMtl->Ks[1],
                                   &currentMtl->Ks[2]);
#endif
                            currentMtl->Ks[3] = 1.0;
                            //OUTPUT << "Ks :" << currentMtl->Ks.v[0] << "," << currentMtl->Ks.v[1] << "," << currentMtl->Ks.v[2] << endl;
                        } else if (ligne[i + 1] == 'e') { // Ke
#ifdef WIN32_VS
                            sscanf_s(ligne+i+2,"%f %f %f",&currentMtl->Ke[0],&currentMtl->Ke[1],&currentMtl->Ke[2]);
#else
                            sscanf(ligne + i + 2, "%f %f %f", &currentMtl->Ke[0], &currentMtl->Ke[1],
                                   &currentMtl->Ke[2]);
#endif
                            currentMtl->Ke[3] = 1.0;
                            //OUTPUT << "Ke :" << currentMtl->Ke.v[0] << "," << currentMtl->Ke.v[1] << "," << currentMtl->Ke.v[2] << endl;
                        }
                        break;
                    case 'N' :
                        if (ligne[i + 1] == 's') { // Ns
#ifdef WIN32_VS
                            sscanf_s(ligne+i+2,"%f",&currentMtl->Ns);
#else
                            sscanf(ligne + i + 2, "%f", &currentMtl->Ns);
#endif
                            if (currentMtl->Ns == 0) {
                                currentMtl->Ks[0] = 0.;
                                currentMtl->Ks[1] = 0.;
                                currentMtl->Ks[2] = 0.;
                                currentMtl->Ks[3] = 1.;
                            }
                            currentMtl->Ns *= 2.55f;
                        }
                        //OUTPUT << "Ns :" << currentMtl->Ns << endl;
                        break;
                    case 'm' : //	void setAmbientAndDiffuseColor(GLfloat *color);
                        if (strncmp(ligne + i, "map_Kd", 6) == 0) {
                            extraire(ligne + i + 7, txt, 255);
                            string str(txt);
                            int pos = str.find_last_of('\\');
                            if (pos == -1) {
                                pos = str.find_last_of('/');
                            }
                            /*currentMtl->Kd[0]=1.;
                            currentMtl->Kd[1]=1.;
                            currentMtl->Kd[2]=1.;*/
                            currentMtl->Kd[3] = 1.;
                            currentMtl->mapKd = new char[strlen(rep) + str.length() + 2 - pos];
#ifdef WIN32_VS
                            strncpy_s(currentMtl->mapKd,lng,ligne+pos+1,lng);
#else
                            sprintf(currentMtl->mapKd, "%s/%s", rep, txt + pos + 1);
//						OUTPUT << currentMtl->mapKd << "," << strlen(rep)+str.length()+2-pos << endl;
                            //strncpy(currentMtl->mapKd,ligne+pos+1,lng);
#endif
                        }
                        break;
#ifdef DEBUG_GRAPHICS
                        default : OUTPUT << "Inconnu : " << ligne << endl;
#endif
                }
            }
        }
        fin.close();
    }

/////////////////////////////////////////////////////////////////////////////
// mtlLib::getMtlByName(nom)
// Recherche d'un matériau dans la liste par son nom
    Mtl *MtlLib::getMtlByName(const char *searched) {
        vector<Mtl *>::const_iterator p = tabMtl.begin();

        while (p != tabMtl.end()) {
            if (strcmp((*p)->name, searched) == 0) return *p;
            p++;
        }
        return nullptr;
    }

/////////////////////////////////////////////////////////////////////////////
// mtlLib::getMtlById(nom)
// Recherche d'un mat�riau dans la liste par son id
    Mtl *MtlLib::getMtlById(int id) {
        vector<Mtl *>::const_iterator p;

        for (p = tabMtl.begin(); p != tabMtl.end(); p++) {
            if ((*p)->id == id) return *p;
        }
        return nullptr;
    }

/////////////////////////////////////////////////////////////////////////////
// mtlLib::getDefaultMtl
// chargement d'un matériau par défault
    Mtl *MtlLib::getDefaultMtl() {
        if (tabMtl.empty()) {
            Mtl *current = new Mtl();
            current->name = new char[20];
#ifdef WIN32_VS
            strncpy_s(current->name,20,"mtl_default_loader",20);
#else
            strncpy(current->name, "mtl_default_loader", 20);
#endif
            current->Ka[0] = 0.1f;
            current->Ka[1] = 0.1f;
            current->Ka[2] = 0.1f;
            current->Ka[3] = 1.f;
            current->Kd[0] = 0.5f;
            current->Kd[1] = 0.5f;
            current->Kd[2] = 0.5f;
            current->Kd[3] = 1.f;
            current->Ks[0] = 0.8f;
            current->Ks[1] = 0.8f;
            current->Ks[2] = 0.8f;
            current->Ks[3] = 1.f;
            current->Ns = 1.f;
            tabMtl.push_back(current);
        }
        return *tabMtl.begin();
    }

/////////////////////////////////////////////////////////////////////////////
// class mtl
    Mtl::Mtl() {
        static unsigned int num = 1;

        id = num++;
        mapKd = nullptr;
        name = nullptr;
        glTexId = 0;
    }

    Mtl::~Mtl() {
        delete[] name;
        delete[] mapKd;
    }

    void Mtl::glBind() {
        glMaterialfv(GL_FRONT, GL_AMBIENT, Ka);
        glMaterialfv(GL_FRONT, GL_DIFFUSE, Kd);
        if (mapKd) {
            if (!glTexId) {
                int lx, ly;
                glTexId = loadTexture(mapKd, lx, ly);
            }
            //glEnable(GL_TEXTURE_2D);
            enableTexture(true);
            glBindTexture(GL_TEXTURE_2D, glTexId);
        } else {
            //glDisable(GL_TEXTURE_2D);
            enableTexture(false);
        }
        glMaterialfv(GL_FRONT, GL_SPECULAR, Ks);
        glMaterialf(GL_FRONT, GL_SHININESS, Ns);
    }

/////////////////////////////////////////////////////////////////////////////
// pour DEBUG : affichage d'un Vector3D
    istream &operator>>(istream &in, Sommet &p3) {
        in >> p3.v[0] >> p3.v[1] >> p3.v[2];
        return in;
    }

    void Sommet::set(GLfloat *tabV, GLfloat *tabN, GLfloat *tabT) {
        memcpy(v, tabV, 3 * sizeof(GLfloat));
        memcpy(n, tabN, 3 * sizeof(GLfloat));
        memcpy(t, tabT, 2 * sizeof(GLfloat));
    }

    bool Sommet::operator==(const Sommet &s) {
        return (v[0] == s.v[0] && v[1] == s.v[1] && v[2] == s.v[2] &&
                n[0] == s.n[0] && n[1] == s.n[1] && n[2] == s.n[2] &&
                t[0] == s.t[0] && t[1] == s.t[1]);
    }

    bool Sommet::isCloseTo(const Sommet &s, float threshold2) {
        float dx = v[0] - s.v[0], dy = v[1] - s.v[1], dz = v[2] - s.v[2];
        float d2 = dx * dx + dy * dy + dz * dz;
        //cout << d2 << endl;
        return d2 < threshold2;
    }

}

