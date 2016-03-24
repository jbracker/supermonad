let var x : {b : {a : Integer, b : {x : Boolean, y : Integer}, c : Boolean},
             a : {z : Integer},
             c : Integer}
    := { c = 1, a = {z = 99}, b = {c = true, a = 5, b = {x = false, y = 17}}};
    var r1 : {b : {y : Integer, x : Boolean}, a : Integer, c : Boolean};
    const y : Integer = 44;
    var r2 : {c : Integer,
              b : {a : Integer, c : Boolean, b : {y : Integer, x : Boolean}},
              a : {z : Integer}}
in begin
    r2 := x;
    r1 := r2.b;
    putint(x.b.b.y + x.c + x.b.a + y);
    putint(r1.b.y + r1.a)
end
