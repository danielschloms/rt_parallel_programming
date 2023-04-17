-- https://en.wikibooks.org/wiki/Ada_Programming/Mathematical_calculations#Root
-- needed for floating point exponentiation
with Ada.Numerics.Generic_Elementary_Functions;

with Typenames; use Typenames;

package Constants is
  -- See root calculation link above
  package Value_Functions is new Ada.Numerics.Generic_Elementary_Functions
   (LLFloat);
  use Value_Functions;

  ONE_THIRD      : constant LLFloat := 1.0 / 3.0;
  -- Check a, b against max_Int64^(1/3) (precalculated) ~ 2.09 * 10^6
  MAX_THIRD_ROOT : constant Int64   := Int64 (LLFloat (Int64'Last)**ONE_THIRD);
end Constants;
