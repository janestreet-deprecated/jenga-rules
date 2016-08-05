let cflags =
  if Compiler_selection.with_frame_pointers
  then ["-fno-omit-frame-pointer"]
  else ["-fomit-frame-pointer"]

let disabled_warnings =
  [ 4; 29; 40; 41; 42; 44; 45; 48; 52; 56; 58; 59 ]

let arch_cflags =
  if Compiler_selection.m32
  then ["-m32"]
  else []
