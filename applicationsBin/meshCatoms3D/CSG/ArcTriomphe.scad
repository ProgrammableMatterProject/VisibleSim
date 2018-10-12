scale(1.5,1.5,1.5)
difference() {
    union() {
        translate([0, 0, 5])
        color([0,0,1])
        cube([6.51, 15.01, 25.01], true);
            
        translate([20, 0, 5])
        color([1,0,0])
        cube([6.51, 15.01, 25.01], true);
        
        color([1,1,1]) {
            difference(){
                translate([10, 0, 10]) 
                rotate([0,90,0])
                cube([15, 15, 26], true);
                translate([10, 0, 5]) 
                rotate([90,0,0])
                cylinder(15.01, 7, 7, true);        
                translate([10, 0, -1])
                cube([13.86, 15, 14], true);
            }
        }
    }
    union() {
        translate([10, 0, 4]) rotate([0,90,0])
        cylinder(30, 4, 4, true);
        translate([10, 0, -2])
        cube([30, 7.85, 11.5], true);
    }
}