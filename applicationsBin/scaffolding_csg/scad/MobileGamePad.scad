rotate([90, 0, 0]) {
    union() { 
    
        difference() { 
            cube(size = [12, 7, 0.5], center = true);
            translate([3.75, -1.5, 0.25]) {
                cylinder(h = 1, r1 = 0.4, r2 = 0.8, $fn = 100, center = true);
            }
        }
        
       /* difference() {
            rotate([90, 0, 0]) {
                translate([6, 0, 0]) { 
                    union(){
                        cylinder(h = 4, r1 = 1.5, r2 = 1.5, $fn = 100, center=true);
                        translate([0, 0, 2]){
                            sphere(r = 1.5, $fn = 100);
                        }
                        translate([0, 0, -2]){
                            sphere(r = 1.5, $fn = 100);
                        }
                    }
                }
                translate([-6, 0, 0]) { 
                    union(){
                        cylinder(h = 4, r1 = 1.5, r2 = 1.5, $fn = 100, center=true);
                        translate([0, 0, 2]){
                            sphere(r = 1.5, $fn = 100);
                        }
                        translate([0, 0, -2]){
                            sphere(r = 1.5, $fn = 100);
                        }
                    }
                }
            }
            
            translate([0, 0, 1.25]) {
                cube(size = [15, 7, 2], center = true);
            }
        
        }*/
        
        
        translate([-3.75, -1.5, 0.25]){
            difference() {
                cylinder(h = 0.3, r = 1, $fn = 100, center = true);
                
                translate([0.8, 0.8, 0]) {
                    cube(size = [1, 1, 2], center = true);
                }
                
                translate([0.8, -0.8, 0]) {
                    cube(size = [1, 1, 2], center = true);
                }
                
                translate([-0.8, -0.8, 0]) {
                    cube(size = [1, 1, 2], center = true);
                }
                
                translate([-0.8, 0.8, 0]) {
                    cube(size = [1, 1, 2], center = true);
                }
            }
        }
        
        translate([3.75, -1.5, 0.25]) {
            
            union() { 
                cylinder(h = 0.4, r = 0.15, $fn = 100, center = true);
                
                difference() { 
                    translate([0, 0, -0.4]) {
                        sphere(r = 0.5, $fn = 100);
                    }
                    
                    translate([0, 0, -0.9]) {
                        cube([1, 1, 1], center = true);
                    }
                }
                
                difference() {
                    translate([0, 0, 0.25]){
                        cylinder(h = 0.2, r = 0.4, $fn = 100, center = true);
                    }
                    
                    translate([0, 0, 0.7]) {
                        sphere(r = 0.48, $fn = 100);
                    }
                }
                           
            }
        }
        
    }
}
