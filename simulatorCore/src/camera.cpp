/*
 * Camera.cpp
 *
 *  Created on: 29 janv. 2012
 *
 */

/***************************************************************************
  camera.cpp  -  gestion de la camera OpenGL
                             -------------------
    copyright            : (C) 2011 by Benoît Piranda
    email                : benoit.piranda@univ-fcomte.fr
 LASELDI / Universite de Franche-Comté
 ***************************************************************************/

#include "camera.h"
Camera::Camera(double t, double p, double d, double SX,double SY) {
	phi=p;
	theta=t;
	distance = d;
	sensibilityX=SX;
	sensibilityY=SY;
	targetMotion=false;
	updatePositionFromAngles();
}

void Camera::updatePositionFromAngles() {
	double OCamx = distance*cos(phi)*cos(theta),
		   OCamy = distance*cos(phi)*sin(theta),
		   OCamz = distance*sin(phi);
	position.pt[0] = target.pt[0] + OCamx;
	position.pt[1] = target.pt[1] + OCamy;
	position.pt[2] = target.pt[2] + OCamz;
	Xcam.set(OCamy,-OCamx,0);
	Xcam.normer_interne();
	Ycam.set(-OCamx*OCamz,-OCamy*OCamz,OCamx*OCamx+OCamy*OCamy);
	Ycam.normer_interne();
}

void Camera::mouseDown(int x, int y,bool tm) {
	mouse[0]=x;
	mouse[1]=y;
	targetMotion=tm;
}

void Camera::mouseUp(int x, int y) {
	if (targetMotion)
	{ target+=-0.1*(mouse[0]-x)*Xcam + 0.1*(y-mouse[1])*Ycam;
	} else
	{ theta += (mouse[0]-x)*sensibilityX;
	  phi -= (mouse[1]-y)*sensibilityY;
	  if (phi>M_PI/2) phi=M_PI/2;
	  else if (phi<-M_PI/2) phi=-M_PI/2;
	}
	targetMotion=false;
	updatePositionFromAngles();
}

void Camera::mouseMove(int x, int y) {
	if (targetMotion) {
		target+=-0.1*(mouse[0]-x)*Xcam + 0.1*(y-mouse[1])*Ycam;
	} else {
		theta += (mouse[0]-x)*sensibilityX;
		phi -= (mouse[1]-y)*sensibilityY;
		if (phi>M_PI/2) phi=M_PI/2;
		else if (phi<-M_PI/2) phi=-M_PI/2;
	}
	mouse[0]=x;
	mouse[1]=y;
  	updatePositionFromAngles();
}

void Camera::mouseZoom(double pas) {
	distance+=pas;
	updatePositionFromAngles();
}

void Camera::glLookAt() {
	gluLookAt(position[0],position[1],position[2],target[0],target[1],target[2], 0.,0.,1.);
}

void Camera::setLightParameters(const Vector3D &t, double th,double ph,double d,double angle,double nearplane,double farplane) {
	ls.target[0] = t[0];
	ls.target[1] = t[1];
	ls.target[2] = t[2];
	ls.theta=th*M_PI/180;
	ls.phi=ph*M_PI/180;
	ls.distance=d;
	ls.falloffAngle=angle;
	ls.near_plane=nearplane;
	ls.far_plane=farplane;
	ls.calcMatrixs();
}

void Camera::glProjection() {
	gluPerspective(angle,w_h,near_plane,far_plane);
}

void Camera::updateIntrinsics(double a,double rwh,double np,double fp) {
	angle=a;
	w_h = rwh;
	near_plane=np;
	far_plane=fp;
}

void Camera::mouseLightDown(int x, int y) {
	mouse[0]=x;
	mouse[1]=y;
}

void Camera::mouseLightUp(int x, int y) {
	ls.theta += (mouse[0]-x)*sensibilityX;
	ls.phi -= (mouse[1]-y)*sensibilityY;
	if (ls.phi>M_PI/2) ls.phi=M_PI/2;
	else if (ls.phi<-M_PI/2) ls.phi=-M_PI/2;

	ls.calcMatrixs();
}

void Camera::mouseLightMove(int x, int y) {
	ls.theta += (mouse[0]-x)*sensibilityX;
	ls.phi -= (mouse[1]-y)*sensibilityY;
	if (ls.phi>M_PI/2) ls.phi=M_PI/2;
	else if (ls.phi<-M_PI/2) ls.phi=-M_PI/2;
	mouse[0]=x;
	mouse[1]=y;

	ls.calcMatrixs();
}

void Camera::mouseLightZoom(double pas) {
	ls.distance+=GLfloat(pas);
	ls.calcMatrixs();
}


const Vector3D Camera::getDirectionSpherical() {
    return Vector3D(90.0 + (theta * 180.0 / M_PI), phi * 180.0 / M_PI, distance, 0);
}

LightSource::LightSource() {
	falloffAngle=60.0;
	near_plane=1.0;
	far_plane=10000.0;
	theta=phi=0;
	distance=1000;
}

void LightSource::calcMatrixs() {
	dir[0] = -cos(phi)*cos(theta);
	dir[1] = -cos(phi)*sin(theta);
	dir[2] = -sin(phi);
	pos[0] = target[0] - distance*dir[0];
	pos[1] = target[1] - distance*dir[1];
	pos[2] = target[2] - distance*dir[2];
	pos[3]=1.0;

	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
	glLoadIdentity();
	gluLookAt(pos[0],pos[1],pos[2],target[0],target[1],target[2],0,0,1);
	glGetFloatv(GL_MODELVIEW_MATRIX, matMV);
	Matrix mat(matMV),mat_1;
	mat.inverse(mat_1);
	mat_1.fillArray(matMV_1);
	glLoadIdentity ();
	gluPerspective(falloffAngle*2., 1.0f, near_plane, far_plane);
	glGetFloatv(GL_MODELVIEW_MATRIX, matP); // mémorisation de la matrice de projection pour la source
	glPopMatrix();
}

void LightSource::draw() {
	glPushMatrix();
	glTranslatef(pos[0],pos[1],pos[2]);
	glutSolidCone(1.0,1.0,10.0,10.0);
	glPopMatrix();
}


const Vector3D LightSource::getDirectionSpherical() {
    return Vector3D(90.0 + (theta * 180.0 / M_PI), phi * 180.0 / M_PI, distance, 0);
}


ostream& operator<<(ostream& f,const Camera &c)
{ f << "(" << c.phi*180.0/M_PI << "," << c.theta*180.0/M_PI << "," << c.distance << ")";
  return f;
}
