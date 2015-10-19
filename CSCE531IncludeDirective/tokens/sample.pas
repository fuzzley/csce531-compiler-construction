{ computes the factorial of n }
function fact(n	: integer) : integer;

begin
   if n = 0 then
      fact = 1
   else
      fact = n * fact(n - 1)
end; { fact }

{ takes x as a percent and returns the actual value }
function percent(x : real) : real;

begin
   percent = x * 1.0e-2;
end; { percent }
