#include "seed.h"
#include <iostream>

Seed::Seed(int id, SEED_DIRECTION direction){
    this->id = id;
    this->direction = direction;
    std::cout << "PARENT = " << this->parent << std::endl;
    this->parent = NULL; 
}

Seed::Seed(int id, SEED_DIRECTION direction, Seed *parent){
    this->id = id;
    this->direction = direction;
    this->parent = parent; 
}

SEED_DIRECTION Seed::GetDirection(){
    return this->direction;
}

Seed* Seed::GetParent(){
    return this->parent;
}

void Seed::SetChildren(vector<Seed*> children){
    this->children = children; 
}

vector<Seed*> Seed::GetChildren(){
    return this->children;
}

void Seed::AddChildren(Seed &children){
    this->children.push_back(&children);
}

void Seed::SetId(int id){
    this->id = id;
}

int Seed::GetId(){
    return id;
}
