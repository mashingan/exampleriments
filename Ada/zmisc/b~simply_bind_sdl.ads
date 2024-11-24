pragma Warnings (Off);
pragma Ada_95;
with System;
with System.Parameters;
with System.Secondary_Stack;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: 11.2.0" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_simply_bind_sdl" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#8d1ad9fc#;
   pragma Export (C, u00001, "simply_bind_sdlB");
   u00002 : constant Version_32 := 16#66132de6#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#2e4c2784#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#edec285f#;
   pragma Export (C, u00004, "interfacesS");
   u00005 : constant Version_32 := 16#e49bce3e#;
   pragma Export (C, u00005, "interfaces__cB");
   u00006 : constant Version_32 := 16#6c9a16d7#;
   pragma Export (C, u00006, "interfaces__cS");
   u00007 : constant Version_32 := 16#5640f7f6#;
   pragma Export (C, u00007, "ada__exceptionsB");
   u00008 : constant Version_32 := 16#73c91185#;
   pragma Export (C, u00008, "ada__exceptionsS");
   u00009 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00009, "adaS");
   u00010 : constant Version_32 := 16#51b6c352#;
   pragma Export (C, u00010, "ada__exceptions__last_chance_handlerB");
   u00011 : constant Version_32 := 16#2c60dc9e#;
   pragma Export (C, u00011, "ada__exceptions__last_chance_handlerS");
   u00012 : constant Version_32 := 16#2ffa5114#;
   pragma Export (C, u00012, "systemS");
   u00013 : constant Version_32 := 16#adf22619#;
   pragma Export (C, u00013, "system__soft_linksB");
   u00014 : constant Version_32 := 16#b3431c8f#;
   pragma Export (C, u00014, "system__soft_linksS");
   u00015 : constant Version_32 := 16#cd5b9fe9#;
   pragma Export (C, u00015, "system__secondary_stackB");
   u00016 : constant Version_32 := 16#de516690#;
   pragma Export (C, u00016, "system__secondary_stackS");
   u00017 : constant Version_32 := 16#896564a3#;
   pragma Export (C, u00017, "system__parametersB");
   u00018 : constant Version_32 := 16#68a895df#;
   pragma Export (C, u00018, "system__parametersS");
   u00019 : constant Version_32 := 16#ced09590#;
   pragma Export (C, u00019, "system__storage_elementsB");
   u00020 : constant Version_32 := 16#02391b10#;
   pragma Export (C, u00020, "system__storage_elementsS");
   u00021 : constant Version_32 := 16#ce3e0e21#;
   pragma Export (C, u00021, "system__soft_links__initializeB");
   u00022 : constant Version_32 := 16#5697fc2b#;
   pragma Export (C, u00022, "system__soft_links__initializeS");
   u00023 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00023, "system__stack_checkingB");
   u00024 : constant Version_32 := 16#a1453afc#;
   pragma Export (C, u00024, "system__stack_checkingS");
   u00025 : constant Version_32 := 16#34742901#;
   pragma Export (C, u00025, "system__exception_tableB");
   u00026 : constant Version_32 := 16#74c450e9#;
   pragma Export (C, u00026, "system__exception_tableS");
   u00027 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00027, "system__exceptionsB");
   u00028 : constant Version_32 := 16#47993ce2#;
   pragma Export (C, u00028, "system__exceptionsS");
   u00029 : constant Version_32 := 16#69416224#;
   pragma Export (C, u00029, "system__exceptions__machineB");
   u00030 : constant Version_32 := 16#bff81f32#;
   pragma Export (C, u00030, "system__exceptions__machineS");
   u00031 : constant Version_32 := 16#aa0563fc#;
   pragma Export (C, u00031, "system__exceptions_debugB");
   u00032 : constant Version_32 := 16#57e0c06f#;
   pragma Export (C, u00032, "system__exceptions_debugS");
   u00033 : constant Version_32 := 16#9f73225f#;
   pragma Export (C, u00033, "system__img_intS");
   u00034 : constant Version_32 := 16#01838199#;
   pragma Export (C, u00034, "system__tracebackB");
   u00035 : constant Version_32 := 16#6f77a74f#;
   pragma Export (C, u00035, "system__tracebackS");
   u00036 : constant Version_32 := 16#1f08c83e#;
   pragma Export (C, u00036, "system__traceback_entriesB");
   u00037 : constant Version_32 := 16#09528275#;
   pragma Export (C, u00037, "system__traceback_entriesS");
   u00038 : constant Version_32 := 16#e98b9a6e#;
   pragma Export (C, u00038, "system__traceback__symbolicB");
   u00039 : constant Version_32 := 16#9fa412cf#;
   pragma Export (C, u00039, "system__traceback__symbolicS");
   u00040 : constant Version_32 := 16#179d7d28#;
   pragma Export (C, u00040, "ada__containersS");
   u00041 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00041, "ada__exceptions__tracebackB");
   u00042 : constant Version_32 := 16#6b52f2d4#;
   pragma Export (C, u00042, "ada__exceptions__tracebackS");
   u00043 : constant Version_32 := 16#e865e681#;
   pragma Export (C, u00043, "system__bounded_stringsB");
   u00044 : constant Version_32 := 16#5807700d#;
   pragma Export (C, u00044, "system__bounded_stringsS");
   u00045 : constant Version_32 := 16#66134eae#;
   pragma Export (C, u00045, "system__crtlS");
   u00046 : constant Version_32 := 16#fa73b1ad#;
   pragma Export (C, u00046, "system__dwarf_linesB");
   u00047 : constant Version_32 := 16#6224b9ce#;
   pragma Export (C, u00047, "system__dwarf_linesS");
   u00048 : constant Version_32 := 16#5b4659fa#;
   pragma Export (C, u00048, "ada__charactersS");
   u00049 : constant Version_32 := 16#ba03ad8f#;
   pragma Export (C, u00049, "ada__characters__handlingB");
   u00050 : constant Version_32 := 16#21df700b#;
   pragma Export (C, u00050, "ada__characters__handlingS");
   u00051 : constant Version_32 := 16#4b7bb96a#;
   pragma Export (C, u00051, "ada__characters__latin_1S");
   u00052 : constant Version_32 := 16#e6d4fa36#;
   pragma Export (C, u00052, "ada__stringsS");
   u00053 : constant Version_32 := 16#24ece25f#;
   pragma Export (C, u00053, "ada__strings__mapsB");
   u00054 : constant Version_32 := 16#ac61938c#;
   pragma Export (C, u00054, "ada__strings__mapsS");
   u00055 : constant Version_32 := 16#08e4a28f#;
   pragma Export (C, u00055, "system__bit_opsB");
   u00056 : constant Version_32 := 16#0765e3a3#;
   pragma Export (C, u00056, "system__bit_opsS");
   u00057 : constant Version_32 := 16#c15d0349#;
   pragma Export (C, u00057, "system__unsigned_typesS");
   u00058 : constant Version_32 := 16#20c3a773#;
   pragma Export (C, u00058, "ada__strings__maps__constantsS");
   u00059 : constant Version_32 := 16#a0d3d22b#;
   pragma Export (C, u00059, "system__address_imageB");
   u00060 : constant Version_32 := 16#8e16cc2e#;
   pragma Export (C, u00060, "system__address_imageS");
   u00061 : constant Version_32 := 16#9d4c9123#;
   pragma Export (C, u00061, "system__img_unsS");
   u00062 : constant Version_32 := 16#20ec7aa3#;
   pragma Export (C, u00062, "system__ioB");
   u00063 : constant Version_32 := 16#b1b8a65b#;
   pragma Export (C, u00063, "system__ioS");
   u00064 : constant Version_32 := 16#ed6ed711#;
   pragma Export (C, u00064, "system__mmapB");
   u00065 : constant Version_32 := 16#5a602070#;
   pragma Export (C, u00065, "system__mmapS");
   u00066 : constant Version_32 := 16#92d882c5#;
   pragma Export (C, u00066, "ada__io_exceptionsS");
   u00067 : constant Version_32 := 16#a8ba7b3b#;
   pragma Export (C, u00067, "system__mmap__os_interfaceB");
   u00068 : constant Version_32 := 16#52ab6463#;
   pragma Export (C, u00068, "system__mmap__os_interfaceS");
   u00069 : constant Version_32 := 16#cc55e4f6#;
   pragma Export (C, u00069, "system__os_libB");
   u00070 : constant Version_32 := 16#d872da39#;
   pragma Export (C, u00070, "system__os_libS");
   u00071 : constant Version_32 := 16#ec4d5631#;
   pragma Export (C, u00071, "system__case_utilB");
   u00072 : constant Version_32 := 16#102fe740#;
   pragma Export (C, u00072, "system__case_utilS");
   u00073 : constant Version_32 := 16#2a8e89ad#;
   pragma Export (C, u00073, "system__stringsB");
   u00074 : constant Version_32 := 16#4fec7d81#;
   pragma Export (C, u00074, "system__stringsS");
   u00075 : constant Version_32 := 16#2fffb3cf#;
   pragma Export (C, u00075, "system__object_readerB");
   u00076 : constant Version_32 := 16#97b71ff7#;
   pragma Export (C, u00076, "system__object_readerS");
   u00077 : constant Version_32 := 16#37baa613#;
   pragma Export (C, u00077, "system__val_lliS");
   u00078 : constant Version_32 := 16#dcdf5cb3#;
   pragma Export (C, u00078, "system__val_lluS");
   u00079 : constant Version_32 := 16#269742a9#;
   pragma Export (C, u00079, "system__val_utilB");
   u00080 : constant Version_32 := 16#835ae7ea#;
   pragma Export (C, u00080, "system__val_utilS");
   u00081 : constant Version_32 := 16#d12f5796#;
   pragma Export (C, u00081, "system__exception_tracesB");
   u00082 : constant Version_32 := 16#2dd6549f#;
   pragma Export (C, u00082, "system__exception_tracesS");
   u00083 : constant Version_32 := 16#fc72f8ac#;
   pragma Export (C, u00083, "system__win32S");
   u00084 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00084, "system__wch_conB");
   u00085 : constant Version_32 := 16#348773c6#;
   pragma Export (C, u00085, "system__wch_conS");
   u00086 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00086, "system__wch_stwB");
   u00087 : constant Version_32 := 16#19965fc7#;
   pragma Export (C, u00087, "system__wch_stwS");
   u00088 : constant Version_32 := 16#1f681dab#;
   pragma Export (C, u00088, "system__wch_cnvB");
   u00089 : constant Version_32 := 16#3b30c935#;
   pragma Export (C, u00089, "system__wch_cnvS");
   u00090 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00090, "system__wch_jisB");
   u00091 : constant Version_32 := 16#bb40d014#;
   pragma Export (C, u00091, "system__wch_jisS");
   u00092 : constant Version_32 := 16#eca5ecae#;
   pragma Export (C, u00092, "system__memoryB");
   u00093 : constant Version_32 := 16#76873720#;
   pragma Export (C, u00093, "system__memoryS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.img_int%s
   --  system.io%s
   --  system.io%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  system.unsigned_types%s
   --  system.img_uns%s
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
   --  system.traceback%s
   --  system.traceback%b
   --  ada.characters.handling%s
   --  system.case_util%s
   --  system.os_lib%s
   --  system.secondary_stack%s
   --  system.standard_library%s
   --  ada.exceptions%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.soft_links%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_llu%s
   --  system.val_lli%s
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  ada.exceptions.traceback%s
   --  ada.exceptions.traceback%b
   --  system.address_image%s
   --  system.address_image%b
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  system.bounded_strings%s
   --  system.bounded_strings%b
   --  system.case_util%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.containers%s
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  ada.strings.maps%s
   --  ada.strings.maps%b
   --  ada.strings.maps.constants%s
   --  interfaces.c%s
   --  interfaces.c%b
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.exceptions.machine%s
   --  system.exceptions.machine%b
   --  system.win32%s
   --  ada.characters.handling%b
   --  system.exception_traces%s
   --  system.exception_traces%b
   --  system.memory%s
   --  system.memory%b
   --  system.mmap%s
   --  system.mmap.os_interface%s
   --  system.mmap.os_interface%b
   --  system.mmap%b
   --  system.object_reader%s
   --  system.object_reader%b
   --  system.dwarf_lines%s
   --  system.dwarf_lines%b
   --  system.os_lib%b
   --  system.secondary_stack%b
   --  system.soft_links.initialize%s
   --  system.soft_links.initialize%b
   --  system.soft_links%b
   --  system.standard_library%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  ada.exceptions%b
   --  simply_bind_sdl%b
   --  END ELABORATION ORDER

end ada_main;
