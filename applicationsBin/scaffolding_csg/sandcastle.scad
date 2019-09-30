union() {
    cube([80,80,6], true);
    
    difference() {
        translate([0,-37,7])
        cube([80,6,20], true);
        translate([0,-37,15])
        cube([20,6.2,30], true);
    }
        
    translate([-34,-40,-3])
    rotate([0,0,90])
    cube([80,6,20]);

    translate([0,37,7])
    cube([80,6,20], true);
        
    translate([37,0,7])
    rotate([0,0,90])
    cube([80,6,20], center=true);

    translate([-37,-37,12])
    cylinder(h=30, d=12, center=true);

    translate([37,37,12])
    cylinder(h=30, d=12, center=true);

    translate([-37,37,12])
    cylinder(h=30, d=12, center=true);

    translate([37,-37,12])
    cylinder(h=30, d=12, center=true);

    translate([0,0,12.5])
    cube([40,40,20], center=true);

    translate([0,0,32])
    cylinder(h=20, d=35, center=true);

    difference() {
        translate([0,0,35])
        sphere(19, center=true);
        translate([0,0,33])
        cylinder(h=18, d=38, center=true);
    }

}