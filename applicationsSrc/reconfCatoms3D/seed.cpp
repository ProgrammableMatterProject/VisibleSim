#include "seed.h"

Seed::Seed(){
    this->parent = NULL; 
    this->id = 0;
}

Seed::Seed(int id){
    this->parent = NULL; 
    this->id = id;
}

Seed::Seed(Seed &parent, int id){
    this->parent = &parent; 
    this->id = id;
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
