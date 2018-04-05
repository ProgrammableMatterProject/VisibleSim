#include "polymer.h"

Polymer::Polymer(int lx,int ly,int sub,float hInit,float r,float dx,float dy) { 
	_nx = lx*sub;
	_ny = ly*sub;
	_lx = lx+2;
	_ly = ly+2;
	_sub = sub;
	_radius=r;
	_dx = dx;
	_dy = dy;
	
	cout << "debut lx=" << _lx << ", ly=" << _ly << " nx=" << _nx << " ny=" << _ny <<endl;

// tableau contenant toutes les informations g�om�triques
// GL_N3F_V3F texture, normale, position
  _tabGeom = new GLfloat[6*(_nx+1)*(_ny+1)];
  _tabIndices = new GLuint[4*_nx*_ny];

// remplissage du tableau de donn�es g�om�triques
  int ix,iy;
  float x,y;
  GLfloat *ptr=_tabGeom;
  GLuint ind,*ptri=_tabIndices;

	// initialisation de la grille de simulation physique
	_tabZ = new float[(_lx+1)*(_ly+1)];
	_tabZ_1 = new float[(_lx+1)*(_ly+1)];
	_tabVitesseZ = new float[(_lx+1)*(_ly+1)];

	memset(_tabVitesseZ,0,(_lx+1)*(_ly+1)*sizeof(float));
	// initialisation of _tabZ to hInit
	float *ptrZ = _tabZ;
	int i=(_lx+1)*(_ly+1);
	while (i--) {
		*ptrZ++ = hInit;
	}
	
	for (iy=0,y=0.; iy<=_ny; iy++,y+=_dy) { 
		for (ix=0,x=0.; ix<=_nx; ix++,x+=_dx) {
// normale
			*ptr++=0.f;
			*ptr++=0.f;
			*ptr++=1.f;
// position
			*ptr++=(GLfloat)(x/sub);
			*ptr++=(GLfloat)(y/sub);
			*ptr++=hInit;
// indices des facettes
			if (ix!=_nx && iy!=_ny) { // cas particulier des points aux bords
				ind = iy+ix*(_ny+1);
				*ptri++=ind;
				*ptri++=ind+_ny+1;
				*ptri++=ind+2+_ny;
				*ptri++=ind+1;
			}
		}
	}
}

Polymer::~Polymer()
{ delete [] _tabGeom;
  delete [] _tabIndices;
  delete [] _tabZ;
  delete [] _tabZ_1;
  delete [] _tabVitesseZ;
}

// mise � jour des donn�es de simulation physique pour une �volution de dur�e dt
float Polymer::positionInstant(float dt) { 
  memcpy(_tabZ_1,_tabZ,(_lx+1)*(_ly+1)*sizeof(float)); // copie des anciennes valeurs de hauteur

  // application du mod�le masse-ressort
  float *ptrZ_1=_tabZ_1,*ptrZ=_tabZ,*ptrV=_tabVitesseZ,F,dz;
  int ix,iy;

	float velocitySum=0;
	Vector3D pos;
	for (iy=0; iy<=_ly; iy++) { 
		pos.pt[1] = iy*_dy;
		for (ix=0; ix<=_lx; ix++) { 
			pos.pt[0] = ix*_dx;
		
			F = -MASSE*GRAVITE; // forces appliqu�es au point
			if (ix!=0) { 
				dz = *(ptrZ_1-1)-*ptrZ_1;
				if (dz>0.) F += (sqrt(_sub*_sub+dz*dz)-_sub)*RAIDEUR1;
				else F -= (sqrt(_sub*_sub+dz*dz)-_sub)*RAIDEUR1;
			}
			if (ix!=_lx) { 
				dz = *(ptrZ_1+1)-*ptrZ_1;
				if (dz>0.) F += (sqrt(_sub*_sub+dz*dz)-_sub)*RAIDEUR1;
				else F -= (sqrt(_sub*_sub+dz*dz)-_sub)*RAIDEUR1;
			}
			if (iy!=0) { 
				dz = *(ptrZ_1-_lx-1)-*ptrZ_1;
				if (dz>0.) F += (sqrt(_sub*_sub+dz*dz)-_sub)*RAIDEUR1;
				else F -= (sqrt(_sub*_sub+dz*dz)-_sub)*RAIDEUR1;
			}
			if (iy!=_ly) { 
				dz = *(ptrZ_1+_lx+1)-*ptrZ_1;
				if (dz>0.) F += (sqrt(_sub*_sub+dz*dz)-_sub)*RAIDEUR1;
				else F -= (sqrt(_sub*_sub+dz*dz)-_sub)*RAIDEUR1;
			}
			F -= AMORT*(*ptrV);
			*ptrV += dt*F/MASSE; // calcul de la nouvelle vitesse
			
			// force de résistance si collision
			pos.pt[2] = *ptrZ+dt*(*ptrV);
			//cout << *ptrZ << "," << pos << endl;
			if (collision(pos)) {
				*ptrV=0;
			} else {
				*ptrZ += dt*(*ptrV);
			}
			
			velocitySum+=*ptrV;

			ptrZ++;
			ptrV++;
			ptrZ_1++;
			
		}
	}
  return velocitySum;
}

// construction de la surface g�om�trique � partir du mod�le de simulation
void Polymer::calculerPolymer()
{ static float tabax[16],tabay[4],Z[4];

  float *ptrZ,*ptra,tx,ty;
  GLfloat *ptr;

  int ix,iy,iix,iiy;

  for (iix=0; iix<_lx-1; iix++)
  { ptrZ=_tabZ+iix;
    ptra=tabax;
    *ptra++ = ( ptrZ[0] + 4.0*ptrZ[1] + ptrZ[2])/6.0 ; // (Pi-1 + 4 Pi + Pi+1)/6
    *ptra++ = (-ptrZ[0] + ptrZ[2])/2.0 ;               // (- Pi-1 + Pi+1)/2
    *ptra++ = ( ptrZ[0] - 2.0*ptrZ[1] + ptrZ[2])/2.0 ; // (Pi-1 - 2 Pi + Pi+1)/2
    *ptra++ = (-ptrZ[0] + 3.0*ptrZ[1] - 3.0*ptrZ[2] + ptrZ[3])/6.0 ; // (-Pi-1 + 3 Pi - 3 Pi+1 + Pi+2)/6
    ptrZ+=_lx+1;
    *ptra++ = ( ptrZ[0] + 4.0*ptrZ[1] + ptrZ[2])/6.0 ; // (Pi-1 + 4 Pi + Pi+1)/6
    *ptra++ = (-ptrZ[0] + ptrZ[2])/2.0 ;               // (- Pi-1 + Pi+1)/2
    *ptra++ = ( ptrZ[0] - 2.0*ptrZ[1] + ptrZ[2])/2.0 ; // (Pi-1 - 2 Pi + Pi+1)/2
    *ptra++ = (-ptrZ[0] + 3.0*ptrZ[1] - 3.0*ptrZ[2] + ptrZ[3])/6.0 ; // (-Pi-1 + 3 Pi - 3 Pi+1 + Pi+2)/6
    ptrZ+=_lx+1;
    *ptra++ = ( ptrZ[0] + 4.0*ptrZ[1] + ptrZ[2])/6.0 ; // (Pi-1 + 4 Pi + Pi+1)/6
    *ptra++ = (-ptrZ[0] + ptrZ[2])/2.0 ;               // (- Pi-1 + Pi+1)/2
    *ptra++ = ( ptrZ[0] - 2.0*ptrZ[1] + ptrZ[2])/2.0 ; // (Pi-1 - 2 Pi + Pi+1)/2
    *ptra++ = (-ptrZ[0] + 3.0*ptrZ[1] - 3.0*ptrZ[2] + ptrZ[3])/6.0 ; // (-Pi-1 + 3 Pi - 3 Pi+1 + Pi+2)/6
    ptrZ+=_lx+1;
    *ptra++ = ( ptrZ[0] + 4.0*ptrZ[1] + ptrZ[2])/6.0 ; // (Pi-1 + 4 Pi + Pi+1)/6
    *ptra++ = (-ptrZ[0] + ptrZ[2])/2.0 ;               // (- Pi-1 + Pi+1)/2
    *ptra++ = ( ptrZ[0] - 2.0*ptrZ[1] + ptrZ[2])/2.0 ; // (Pi-1 - 2 Pi + Pi+1)/2
    *ptra++ = (-ptrZ[0] + 3.0*ptrZ[1] - 3.0*ptrZ[2] + ptrZ[3])/6.0 ; // (-Pi-1 + 3 Pi - 3 Pi+1 + Pi+2)/6

    for (iiy=0; iiy<_ly-1; iiy++)
    {
      for (ix=0,tx=0.; ix<_sub; ix++,tx+=1.0/_sub)
	  { ptra = tabax;
	    Z[0] = ((ptra[3]*tx + ptra[2])*tx + ptra[1])*tx + ptra[0];
	    ptra+=4;
	    Z[1] = ((ptra[3]*tx + ptra[2])*tx + ptra[1])*tx + ptra[0];
	    ptra+=4;
	    Z[2] = ((ptra[3]*tx + ptra[2])*tx + ptra[1])*tx + ptra[0];
	    ptra+=4;
	    Z[3] = ((ptra[3]*tx + ptra[2])*tx + ptra[1])*tx + ptra[0];
        ptra = tabay;
        *ptra++ = ( Z[0] + 4.0*Z[1] + Z[2])/6.0 ; // (Pi-1 + 4 Pi + Pi+1)/6
        *ptra++ = (-Z[0] + Z[2])/2.0 ;               // (- Pi-1 + Pi+1)/2
        *ptra++ = ( Z[0] - 2.0*Z[1] + Z[2])/2.0 ; // (Pi-1 - 2 Pi + Pi+1)/2
        *ptra++ = (-Z[0] + 3.0*Z[1] - 3.0*Z[2] + Z[3])/6.0 ; // (-Pi-1 + 3 Pi - 3 Pi+1 + Pi+2)/6
	    ptr=_tabGeom + (iiy*_sub*(_nx+1)+ix+iix*_sub)*6 ;
	    for (iy=0,ty=0.; iy<_sub; iy++,ty+=1.0/_sub)
	    { ptr[0]=-(1.-ty)*((3.*tabax[7]*tx + 2.*tabax[6])*tx + tabax[5])-ty*((3.*tabax[11]*tx + 2.*tabax[10])*tx + tabax[9]);
          ptr[1]=-((3.*tabay[3]*ty + 2.*tabay[2])*ty + tabay[1]);
          ptr[5]=((tabay[3]*ty + tabay[2])*ty + tabay[1])*ty + tabay[0];
          ptr+=(_nx+1)*6;
	    }
      }
      memcpy(tabax,tabax+4,12*sizeof(float));
      ptrZ+=_lx+1;
 	  ptra=tabax+12;
      *ptra++ = ( ptrZ[0] + 4.0*ptrZ[1] + ptrZ[2])/6.0 ; // (Pi-1 + 4 Pi + Pi+1)/6
      *ptra++ = (-ptrZ[0] + ptrZ[2])/2.0 ;               // (- Pi-1 + Pi+1)/2
      *ptra++ = ( ptrZ[0] - 2.0*ptrZ[1] + ptrZ[2])/2.0 ; // (Pi-1 - 2 Pi + Pi+1)/2
      *ptra++ = (-ptrZ[0] + 3.0*ptrZ[1] - 3.0*ptrZ[2] + ptrZ[3])/6.0 ; // (-Pi-1 + 3 Pi - 3 Pi+1 + Pi+2)/6
	}
  }
}

void Polymer::glDraw() { 
	static GLfloat ambi[] = { 0.1,0.1,0.1,1.0 },
                 diff[] = { 1.0,1.0,1.0,1.0 },
                 spec[] = { 1.0,1.0,0.0,1.0 };
	glPushMatrix();

	glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT,ambi);
	glMaterialfv(GL_FRONT_AND_BACK,GL_DIFFUSE,diff);
	glMaterialfv(GL_FRONT_AND_BACK,GL_SPECULAR,spec);
	glMaterialf(GL_FRONT_AND_BACK,GL_SHININESS,25.0);

	/*glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);
	glInterleavedArrays(GL_N3F_V3F,0,_tabGeom);
	glDrawElements(GL_QUADS,4*_nx*_ny,GL_UNSIGNED_INT,_tabIndices);
	glDisableClientState(GL_NORMAL_ARRAY);
	glDisableClientState(GL_VERTEX_ARRAY);*/
	int ix,iy;
	for (iy=0; iy<_ny; iy++) {
		GLfloat *ptr0 = _tabGeom+3+iy*6*(_nx+1);
		GLfloat *ptr1 = ptr0+6*(_nx+1);
		glBegin(GL_QUAD_STRIP);
		for (ix=0; ix<_nx+1; ix++) {
			glNormal3fv(ptr1-3);
			glVertex3fv(ptr1);
			glNormal3fv(ptr0-3);
			glVertex3fv(ptr0);
			ptr0+=6;
			ptr1+=6;
		}
		glEnd();
	}
	glPopMatrix();
}

bool Polymer::collision(const Vector3D pos) {
	vector<Vector3D>::iterator it = tabPt.begin();
	float d;
	while (it!=tabPt.end()) {
		d = (pos-*it).norme2();
		if (d<_radius) return true;
		it++;
	}
	return false;
}
