rotate([90, 0, 0]) {
    union() { 
    
        cube(size = [12, 7, 0.5], center = true);
        
        difference() {
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
        
        }
        
        translate([-5, -0.25, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        translate([-5, -1.5, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        translate([-5, -2.75, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        
        translate([-3.75, -0.25, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        translate([-3.75, -1.5, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        translate([-3.75, -2.75, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        
        translate([-2.5, -0.25, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        translate([-2.5, -1.5, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        translate([-2.5, -2.75, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        
        translate([-1.25, -0.25, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        translate([-1.25, -1.5, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        translate([-1.25, -2.75, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        
        translate([0, -0.25, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        translate([0, -1.5, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        translate([0, -2.75, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        
        translate([5, -0.25, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        translate([5, -1.5, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        translate([5, -2.75, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        
        translate([3.75, -0.25, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        translate([3.75, -1.5, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        translate([3.75, -2.75, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        
        translate([2.5, -0.25, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        translate([2.5, -1.5, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        translate([2.5, -2.75, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        
        translate([1.25, -0.25, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        translate([1.25, -1.5, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        translate([1.25, -2.75, 0.25]){
            cube(size = [1, 1, 0.2], center = true);
        }
        
    }
}
