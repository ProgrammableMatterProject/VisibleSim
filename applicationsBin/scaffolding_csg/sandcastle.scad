scale([2,2,2]) union() {
    cube([40,40,6]);
    rotate([0,0,0])
    cube([40,6,20]);
    rotate([0,0,90])
    cube([40,6,20]);
    rotate([0,0,90])
    cube([40,6,20]);
    translate([20,20,13])
    cylinder(h=25, d=12, center=true);
    translate([20,20,30])
    sphere(8);
}