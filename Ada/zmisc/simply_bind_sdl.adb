-- to compile
-- ensure the path where dll or so resides, for example (on windows)
-- $ where sdl2.dll
-- will give
--      c:/bin/sdl2.dll
-- then:
-- $ gnatmake -aOc:/bin simply_bind_sdl.adb
with Interfaces.C; use Interfaces.C;

procedure simply_bind_sdl is
    function SDL_Init(number : unsigned) return unsigned
        with Import => True,
        Convention => C,
        External_Name => "SDL_Init";

    value_returned : unsigned;
begin
    value_returned := SDL_Init(0);
end;
